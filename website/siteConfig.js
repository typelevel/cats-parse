// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

const repoUrl = 'https://github.com/typelevel/cats-parse';

const siteConfig = {
  title: 'Cats Parse',
  tagline: 'parsing library for the cats ecosystem',
  url: 'https://typelevel.org',
  baseUrl: '/cats-parse/',

  projectName: 'cats-parse',
  organizationName: 'typelevel',

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'installation', label: 'Docs'},
    {blog: true, label: 'Blog'},
    {href: repoUrl, label: "GitHub", external: true},
  ],

  headerIcon: 'img/favicon.ico',
  footerIcon: 'img/favicon.ico',
  favicon: 'img/favicon.ico',

  colors: {
    primaryColor: '#516094',
    secondaryColor: '#384367',
  },
  
  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} The Typelevel cats-parse Project Developers`,

  highlight: {
    theme: 'default',
  },

  scripts: ['https://buttons.github.io/buttons.js'],

  onPageNav: 'separate',
  cleanUrl: true,

  ogImage: 'img/undraw_online.svg',
  twitterImage: 'img/undraw_tweetstorm.svg',
};

module.exports = siteConfig;
