#!/usr/bin/python3

import requests
from lxml import html

BASE_URL = 'https://www.metal-archives.com'

class Band(object):
    def __init__(self, name=None, members=None, all_members=None):
        self.name = name
        self.members = members
        self.all_members = all_members

    def print_members(self):
        print('{} currently has {} members:'.format(self.name, len(self.members)))
        for member in self.members:
            print(member.text)
        print()
        print('I also have a list of everyone who ever played with {}:'.format(self.name, len(self.members)))
        for member in self.all_members:
            print(member.text)

def get_by_name_and_id(name, band_id):
    url = BASE_URL + '/bands/{}/{}'.format(name, band_id)
    return get_by_url(url)

def get_by_url(link):
    page = requests.get(link)
    tree = html.fromstring(page.content)
    members_all = tree.xpath("//div[@id='band_tab_members_all']//tr[@class='lineupRow']/td/a")
    members_current = tree.xpath("//div[@id='band_tab_members_current']//tr[@class='lineupRow']/td/a")
    #print(members[0].get('href'))
    #print('Members: ', members)
    return Band(members=members_current, all_members=members_all)

def search_band_name(search_term):
    def parse_bandlink(band):
        tree = html.fromstring(band)
        a_element = tree.xpath("//a")[0]
        band_link = a_element.get('href')
        band_name = a_element.text
        return band_link, band_name
    print('Doing my best to find bands. Please wait a few seconds.')
    page = requests.get(BASE_URL + '/search/ajax-band-search/?field=name&query={}&iDisplayStart=0&iDisplayLength=200'.format(search_term))
    search_result = page.json()
    n = search_result['iTotalDisplayRecords']
    if n<1:
        print('No band found. Maybe try another search term.')
        return
    if n==1:
        print('Found a band. Please wait while I spy on them')
        band_list = [parse_bandlink(search_result['aaData'][0][0])]
    else:
        print('Found {} Bands. Please choose one.'.format(n))
        band_list = dict()
        for index, band in enumerate(search_result['aaData']):
            band_list[str(index)] = (band_link, band_name) = parse_bandlink(band[0])
            print('[{}] {} from {} ({})'.format(index, band_name, band[2], band[1]))
    choice = input('Number: ') if n>1 else 0
    print()
    try:
        band = get_by_url(band_list[choice][0])
    except (TypeError, KeyError):
        # user prolly screwed up input
        print('Are you drunk?')
        return
    band.name = band_list[choice][1]
    band.print_members()

#epica = get_by_name_and_id('Epica', 6716)
#epica.print_members()

while True:
    print()
    search_term = input('Search for a band: ')
    print()
    search_band_name(search_term)
