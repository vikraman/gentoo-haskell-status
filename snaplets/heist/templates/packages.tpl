<table>
  <thead>
    <tr>
      <th>Package</th>
      <th>
        Gentoo version
        <a href="#" onClick="updatePackages()">Update</a>
      </th>
      <th>
        Hackage version
        <a href="#" onClick="updateHackage()">Update</a>
      </th>
    </tr>
  </thead>
  <tbody>
    <packages>
      <tr>
        <td style="background-color: ${packageStatus}">
          <a href="https://hackage.haskell.org/package/${packageName}">
            <packageName/>
          </a>
        </td>
        <td style="background-color: ${packageStatus}">
          <packageVersion/>
        </td>
        <td style="background-color: ${packageStatus}">
          <a href="https://hackage.haskell.org/package/${packageName}-${hackageVersion}">
            <hackageVersion/>
          </a>
        </td>
      </tr>
    </packages>
  </tbody>
</table>
