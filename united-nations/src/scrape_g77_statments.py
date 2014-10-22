# This script scrapes statements and speeches from the g77. 
# You can run the script and write the contents out to a file 
# named g77_statements.csv using the command:
# python scrape_g77_statements.py
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import re, codecs

# Create a headless browser.
browser = webdriver.Chrome()

# Define the XPath queries we'll use later.
statement_query = '//span//ul//li//a[contains(@href, "statement")]' 
speech_query =  '//span//ul//li//a[contains(@href, "Speeches")]'
date_query = '//html//body//table//b//font'

# Grab text from speeches and statements.
def get_text(browser, href):
  browser.get(href)
  text_parts = browser.find_elements_by_xpath('//p')
  body = u" ".join([e.text for e in text_parts]).replace(u",", u" ")
  body = u" ".join(body.splitlines)
  return(body)

# Grab the date from the speech or statement url.
def date_from_href(href):
  ds = re.findall(r"\d{6}", href)[0]
  if (href.find("Speeches") > 0):
    ret = ds[0:2] + u"-" + ds[2:4]
    if int(ds[4:6]) > 50:
      ret += u"-19" + ds[4:]
    else:
      ret += u"-20" + ds[4:]
  else:
    if int(ret[0:2]) > 50:
      ret = u"19" + ret[0:2]
    else:
      ret = "20" + ret[0:2]
    ret = ds[2:4] + u"-" + ds[0:2] + u"-" + ret
  return(ret)

# Statements contain unicode characters.
f = codecs.open("../data/g77_statements.csv", "w", "utf-8")

year_ext = [unicode(year) + u".html" for year in xrange(1997, 2014)] + [u""]
for ye in year_ext:
  browser.get("http://www.g77.org/statement/" + ye)
  speech_elems = browser.find_elements_by_xpath(speech_query)
  statement_elems = browser.find_elements_by_xpath(statement_query)
  speech_hrefs = [x.get_attribute('href') for x in speech_elems]
  # Make sure we only get html documents.
  speech_hrefs = filter(lambda x: x[-3:] == 'htm', speech_hrefs)
  statement_hrefs = [x.get_attribute('href') for x in statement_elems]
  speech_hrefs = filter(lambda x: x[-3:] == 'htm', statement_elems)
  for href in speech_hrefs:
    f.write(u",".join([u"speech"] + [date_from_href(href)] + 
      [get_text(browser, href)]) + u"\n")
  for href in statement_hrefs:
    f.write(u",".join([u"speech"] + [date_from_href(href)] + 
      [get_text(browser, href)]) + u"\n")

browser.close()
