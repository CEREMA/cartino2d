<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.10.2-A Coruña" minScale="1e+08" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" maxScale="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <customproperties>
    <property value="false" key="WMSBackgroundLayer"/>
    <property value="false" key="WMSPublishDataSourceUrl"/>
    <property value="0" key="embeddedWidgets/count"/>
    <property value="Value" key="identify/format"/>
  </customproperties>
  <pipe>
    <rasterrenderer type="singlebandpseudocolor" alphaBand="-1" band="1" opacity="0.6" classificationMin="0" classificationMax="inf">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader colorRampType="DISCRETE" clip="0" classificationMode="2">
          <colorramp type="gradient" name="[source]">
            <prop k="color1" v="103,0,13,255"/>
            <prop k="color2" v="255,245,240,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.1;165,15,21,255:0.22;203,24,29,255:0.35;239,59,44,255:0.48;251,106,74,255:0.61;252,146,114,255:0.74;252,187,161,255:0.87;254,224,210,255"/>
          </colorramp>
          <item label="TEMPS entre Pic Pluie et Hauteur Max" color="#ff00ff" value="0" alpha="0"/>
          <item label="Avant le pic de pluie" color="#67000d" value="3600" alpha="0"/>
          <item label="0-15 min" color="#ad1016" value="4500" alpha="255"/>
          <item label="15-30 min" color="#d42020" value="5400" alpha="255"/>
          <item label="30-60 min" color="#f24431" value="7200" alpha="255"/>
          <item label="1-2 h " color="#fc7050" value="10800" alpha="255"/>
          <item label="2-3 h" color="#fc9777" value="14400" alpha="255"/>
          <item label="3-6 h" color="#fdbea5" value="25200" alpha="255"/>
          <item label="6-12 h" color="#ffe1d3" value="46800" alpha="255"/>
          <item label=">=12 h" color="#fff5f0" value="inf" alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="0"/>
    <huesaturation colorizeRed="255" colorizeOn="0" saturation="0" colorizeStrength="100" colorizeGreen="128" colorizeBlue="128" grayscaleMode="0"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
