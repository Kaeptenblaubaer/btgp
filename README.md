Skip to content
Search or jump to…
Pull requests
Issues
Marketplace
Explore
 
@Kaeptenblaubaer 
Kaeptenblaubaer
/
bitemporal
Public
1
00
Code
Issues
Pull requests
Actions
Projects
Wiki
Security
Insights
Settings
bitemporal
/
README.md
in
master
 

Spaces

1

Soft wrap
1
![beware - work in progress](./wip.png)
2
​
3
# bitemporal
4
# A pilot project for a poor man's bitemporal data management using a simple application level transaction and workflow logic
5
​
6
The project is based on this framework:
7
https://github.com/digitallyinduced/ihp
8
​
9
## Bitemporal data model
10
### The spatial analogy
11
​
12
A bitemporal model allows for modelling change in two dimensions of time:
13
* referenced time like e.g. in contracts that are agreed to be valid for a certain time, and secondly
14
* transaction time, like e.g. the time, when a contract 
15
or a change of contract is agreed upon
16
​
17
For instance, we agreed
18
* on Monday to paint the house blue next year,
19
* on Tuesday to paint it yellow two years later, and
20
* on Wednesday to paint it red already one year later
21
* and leave it at that.
22
​
23
The first idea of this approach is spatial representation of the two temporal dimensions.
24
The above course of agreements
25
would be described by sets of adjacent rectangles, reference time running horizontally, 
26
transaction time vertically.
27
​
28
![Areas of Validity](./ValidityRectangles.png)
29
​
30
Colloquially phrased, such a model provides answers to questions like 
31
​
32
"What did we think at *transaction time* what we agreed upon as of *referenced time*"
33
​
34
Reference time and transaction time can be seen as
35
constituting a point which is contained in a 
36
of a set of rectangles of validity and thus identifies a version.
37
​
38
### Composite bitemporal objects
39
​
40
![beware - work in progress](./BitemporaleHistorisierung.png)
41
​
42
The second idea is based on the fact that object attributes maybe independendent from one another, for instance we 
43
repaint the house more often, than we change the janitor.
44
​
45
In the example above each (re-)paint of the house, produces a new version, the color of which differs from its predecessor, while the name of the janitor does not.
Keine ausgewählt
Attach files by dragging & dropping, selecting or pasting them.
@Kaeptenblaubaer
Commit changes
Commit summary
Create README.md
Optional extended description
Add an optional extended description…
 Commit directly to the master branch.
 Create a new branch for this commit and start a pull request. Learn more about pull requests.
 
© 2021 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
