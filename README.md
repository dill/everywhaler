`everywhaler`
===========

# The database

Crew database of 127,531 records of men leaving the port of New Bedford on whaling voyages from 1809 to 1927. [More information on the data](https://www.whalingmuseum.org/online_exhibits/crewlist/about.php).

Data originally digitized by volunteers working with the [New Bedford Whaling Museum](https://www.whalingmuseum.org/).

The database is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License](https://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US).

This project is in no way affiliated with the New Bedford Whaling Museum.

# Files

- `crewlist.csv` database CSV file from the Whaling Museum website
- `format_data.R` script to put the data into an R `data.frame`
- `heights.R` helper script to convert between the weirdly formatted seaman heights and metres (thanks to [James Curran for this code](https://gist.github.com/jmcurran/36734b7d62d4995e0b16ee4b2b7b4075))
- `whalers.RData` RData file with the `data.frame` containing the data, suitably formatted for R.

# License

Data is from the New Bedford Whaling Museum and is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License](https://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US).

Code is licensed under the [GNU General Public License (version >= 2)](https://opensource.org/licenses/GPL-2.0).
