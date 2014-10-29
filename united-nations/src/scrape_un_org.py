from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import re, codecs

browser = webdriver.PhantomJS()

#meeting_cov=[u"http://www.un.org/press/en/content/economic-and-social-council/meetings-coverage", 
def get_date_and_text(browser, href):
  browser.get(href)
  text_parts = browser.find_elements_by_xpath('//p')
  body = u" ".join([e.text for e in text_parts]).replace(u",", u" ")
  body = body.replace(u'"', u"")
  body = u" ".join(body.splitlines())
  date_part = \
    browser.find_elements_by_xpath('//span[@class="date-display-single"]')
  date_string = date_part[0].get_attribute("content")[0:10]
  print( href + " " + date_string )
  return([date_string, href, body])

groups = [u'economic-and-social-council', u'security-council', 
          u'general-assembly', u'secretary-general']
urls = [u"http://www.un.org/press/en/content/" + x + u'/meetings-coverage' for
  x in groups[0:3]]
groups = groups + u'http://www.un.org/press/en/content/statements-and-messages'

for url in urls:
  print("Getting data for " + url[i] + "\n")
  browser.get(url)
  done = False
  links = []
  while not done:
  print("Getting data from " + browser.current_url + "\n")
  elem=browser.find_elements_by_xpath('//ul//li[@class="pager-next last"]//a')
  if len(elem) > 0: 
    next_link = elem[0].get_attribute("href")
    elem = browser.find_elements_by_xpath('//ul//li[@class="pager-current"]')
    if elem[0].text == u'Page 0' or if elem[0].text == u'':
      done=True
  else:
    done=True
  link_elems = browser.find_elements_by_xpath('//div//span//a') 
  links += [x.get_attribute("href") for x in link_elems]
  if not done:
    browser.get(next_link)

f = codecs.open("../data/" + group + ".csv", "w", "utf-8")
for link in links:
  print(group + " " + link + "\n")
  f.write(u",".join(get_date_and_text(browser, link)) + u"\n")

f.close()


