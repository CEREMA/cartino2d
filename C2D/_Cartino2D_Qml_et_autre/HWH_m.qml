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
    <rasterrenderer type="singlebandpseudocolor" alphaBand="-1" band="1" opacity="0.6" classificationMin="0" classificationMax="1000">
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
            <prop k="color1" v="0,0,4,255"/>
            <prop k="color2" v="252,253,191,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.0196078;2,2,11,255:0.0392157;5,4,22,255:0.0588235;9,7,32,255:0.0784314;14,11,43,255:0.0980392;20,14,54,255:0.117647;26,16,66,255:0.137255;33,17,78,255:0.156863;41,17,90,255:0.176471;49,17,101,255:0.196078;57,15,110,255:0.215686;66,15,117,255:0.235294;74,16,121,255:0.254902;82,19,124,255:0.27451;90,22,126,255:0.294118;98,25,128,255:0.313725;106,28,129,255:0.333333;114,31,129,255:0.352941;121,34,130,255:0.372549;129,37,129,255:0.392157;137,40,129,255:0.411765;145,43,129,255:0.431373;153,45,128,255:0.45098;161,48,126,255:0.470588;170,51,125,255:0.490196;178,53,123,255:0.509804;186,56,120,255:0.529412;194,59,117,255:0.54902;202,62,114,255:0.568627;210,66,111,255:0.588235;217,70,107,255:0.607843;224,76,103,255:0.627451;231,82,99,255:0.647059;236,88,96,255:0.666667;241,96,93,255:0.686275;244,105,92,255:0.705882;247,114,92,255:0.72549;249,123,93,255:0.745098;251,133,96,255:0.764706;252,142,100,255:0.784314;253,152,105,255:0.803922;254,161,110,255:0.823529;254,170,116,255:0.843137;254,180,123,255:0.862745;254,189,130,255:0.882353;254,198,138,255:0.901961;254,207,146,255:0.921569;254,216,154,255:0.941176;253,226,163,255:0.960784;253,235,172,255:0.980392;252,244,182,255"/>
          </colorramp>
          <item label="HAUTEUR EAU (m)" color="#ff00ff" value="0" alpha="0"/>
          <item label="&lt;= 0.05" color="#fdfdfd" value="0.05" alpha="0"/>
          <item label="0.05 - 0.1" color="#b2ffb2" value="0.1" alpha="255"/>
          <item label="0.1 - 0.5" color="#00ff00" value="0.5" alpha="255"/>
          <item label="0.5 - 1" color="#ff7a00" value="1" alpha="255"/>
          <item label="1 - 2" color="#ff00ff" value="2" alpha="255"/>
          <item label="2 - 3" color="#7a00ff" value="3" alpha="255"/>
          <item label="3 - 5" color="#3d007a" value="5" alpha="255"/>
          <item label="5 - 10" color="#3d0000" value="10" alpha="255"/>
          <item label="10 - 1000" color="#000000" value="1000" alpha="255"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast contrast="0" brightness="0"/>
    <huesaturation colorizeRed="255" colorizeOn="0" saturation="0" colorizeStrength="100" colorizeGreen="128" colorizeBlue="128" grayscaleMode="0"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
