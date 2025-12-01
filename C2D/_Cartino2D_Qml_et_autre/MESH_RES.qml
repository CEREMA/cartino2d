<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis hasScaleBasedVisibilityFlag="0" maxScale="0" minScale="1e+08" version="3.18.1-Zürich" styleCategories="AllStyleCategories">
  <renderer-3d layer="hyeto_C00km57pS12_PluieQuasiNulle_00h_00m_02h_00m_02d1275e_db4c_4ff6_8fed_487d0173c6d4" type="mesh">
    <symbol type="mesh">
      <data height="0" add-back-faces="0" alt-clamping="relative"/>
      <material shininess="0" ambient="25,25,25,255" specular="255,255,255,255" diffuse="179,179,179,255">
        <data-defined-properties>
          <Option type="Map">
            <Option value="" type="QString" name="name"/>
            <Option name="properties"/>
            <Option value="collection" type="QString" name="type"/>
          </Option>
        </data-defined-properties>
      </material>
      <advanced-settings texture-single-color="0,128,0,255" renderer-3d-enabled="0" wireframe-line-color="128,128,128,255" min-color-ramp-shader="0" max-color-ramp-shader="255" arrows-spacing="25" wireframe-enabled="0" smoothed-triangle="0" vertical-scale="1" arrows-enabled="0" arrows-fixed-size="0" wireframe-line-width="1" texture-type="0" level-of-detail="-1" vertical-group-index="-1" vertical-relative="0">
        <colorrampshader classificationMode="1" minimumValue="0" maximumValue="255" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
          <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
            <numericFormat id="basic">
              <Option type="Map">
                <Option value="" type="QChar" name="decimal_separator"/>
                <Option value="6" type="int" name="decimals"/>
                <Option value="0" type="int" name="rounding_type"/>
                <Option value="false" type="bool" name="show_plus"/>
                <Option value="true" type="bool" name="show_thousand_separator"/>
                <Option value="false" type="bool" name="show_trailing_zeros"/>
                <Option value="" type="QChar" name="thousand_separator"/>
              </Option>
            </numericFormat>
          </rampLegendSettings>
        </colorrampshader>
      </advanced-settings>
      <data-defined-properties>
        <Option type="Map">
          <Option value="" type="QString" name="name"/>
          <Option name="properties"/>
          <Option value="collection" type="QString" name="type"/>
        </Option>
      </data-defined-properties>
    </symbol>
  </renderer-3d>
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal reference-time="1900-01-01T00:00:00Z" start-time-extent="1900-01-01T00:00:00Z" matching-method="0" temporal-active="1" end-time-extent="1900-01-01T02:00:00Z"/>
  <customproperties/>
  <mesh-renderer-settings>
    <active-dataset-group scalar="4" vector="0"/>
    <scalar-settings interpolation-method="none" group="0" min-val="0" opacity="1" max-val="6">
      <colorrampshader classificationMode="2" minimumValue="0" maximumValue="6" clip="0" colorRampType="DISCRETE" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="255,0,255,0" type="QString" name="color1"/>
            <Option value="0,0,4,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0166667;255,0,255,0:0.0833333;252,253,191,255:0.166667;254,159,109,255:0.333333;222,73,105,255:0.833333;140,41,129,255:1.66667;59,15,111,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="255,0,255,0"/>
          <prop k="color2" v="0,0,4,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0166667;255,0,255,0:0.0833333;252,253,191,255:0.166667;254,159,109,255:0.333333;222,73,105,255:0.833333;140,41,129,255:1.66667;59,15,111,255"/>
        </colorramp>
        <item color="#ff00ff" alpha="0" value="0.1" label="VITESSE"/>
        <item color="#fcfdbf" alpha="255" value="0.5" label="0.100000 - 0.500000"/>
        <item color="#fe9f6d" alpha="255" value="1" label="0.500000 - 1.000000"/>
        <item color="#de4969" alpha="255" value="2" label="1.000000 - 2.000000"/>
        <item color="#8c2981" alpha="255" value="5" label="2.000000 - 5.000000"/>
        <item color="#3b0f6f" alpha="255" value="10" label="5.000000 - 10.000000"/>
        <item color="#000004" alpha="255" value="inf" label="> 10.000000"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="1" min-val="0" opacity="1" max-val="50.223">
      <colorrampshader classificationMode="2" minimumValue="0" maximumValue="50.223" clip="0" colorRampType="DISCRETE" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="255,0,255,0" type="QString" name="color1"/>
            <Option value="0,0,0,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.00099556;255,0,255,0:0.00199112;178,255,178,255:0.0099556;0,255,0,255:0.0199112;255,122,0,255:0.0398224;255,0,255,255:0.0597336;122,0,255,255:0.099556;61,0,122,255:0.199112;61,0,0,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="255,0,255,0"/>
          <prop k="color2" v="0,0,0,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.00099556;255,0,255,0:0.00199112;178,255,178,255:0.0099556;0,255,0,255:0.0199112;255,122,0,255:0.0398224;255,0,255,255:0.0597336;122,0,255,255:0.099556;61,0,122,255:0.199112;61,0,0,255"/>
        </colorramp>
        <item color="#ff00ff" alpha="0" value="0.05" label="HAUTEUR"/>
        <item color="#b2ffb2" alpha="255" value="0.1" label="0.050000 - 0.100000"/>
        <item color="#00ff00" alpha="255" value="0.5" label="0.100000 - 0.500000"/>
        <item color="#ff7a00" alpha="255" value="1" label="0.500000 - 1.000000"/>
        <item color="#ff00ff" alpha="255" value="2" label="1.000000 - 2.000000"/>
        <item color="#7a00ff" alpha="255" value="3" label="2.000000 - 3.000000"/>
        <item color="#3d007a" alpha="255" value="5" label="3.000000 - 5.000000"/>
        <item color="#3d0000" alpha="255" value="10" label="5.000000 - 10.000000"/>
        <item color="#000000" alpha="255" value="inf" label="> 10.000000"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="2" min-val="212.95" opacity="1" max-val="313.84">
      <colorrampshader classificationMode="1" minimumValue="212.9499969482422" maximumValue="313.8399963378906" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="212.9499969482422" label="213"/>
        <item color="#1b068d" alpha="255" value="214.92822787827453" label="215"/>
        <item color="#260591" alpha="255" value="216.90646889730684" label="217"/>
        <item color="#2f0596" alpha="255" value="218.88469982733918" label="219"/>
        <item color="#38049a" alpha="255" value="220.86294084637146" label="221"/>
        <item color="#41049d" alpha="255" value="222.8411717764038" label="223"/>
        <item color="#4903a0" alpha="255" value="224.81940270643616" label="225"/>
        <item color="#5102a3" alpha="255" value="226.7976538144684" label="227"/>
        <item color="#5901a5" alpha="255" value="228.77590492250062" label="229"/>
        <item color="#6100a7" alpha="255" value="230.75415603053284" label="231"/>
        <item color="#6900a8" alpha="255" value="232.73230624856566" label="233"/>
        <item color="#7100a8" alpha="255" value="234.7105573565979" label="235"/>
        <item color="#7801a8" alpha="255" value="236.68880846463014" label="237"/>
        <item color="#8004a8" alpha="255" value="238.66705957266237" label="239"/>
        <item color="#8707a6" alpha="255" value="240.6453106806946" label="241"/>
        <item color="#8e0ca4" alpha="255" value="242.62356178872682" label="243"/>
        <item color="#9511a1" alpha="255" value="244.60171200675964" label="245"/>
        <item color="#9c179e" alpha="255" value="246.57996311479187" label="247"/>
        <item color="#a21d9a" alpha="255" value="248.5582142228241" label="249"/>
        <item color="#a82296" alpha="255" value="250.53646533085632" label="251"/>
        <item color="#ae2892" alpha="255" value="252.51471643888854" label="253"/>
        <item color="#b42e8d" alpha="255" value="254.49296754692077" label="254"/>
        <item color="#ba3388" alpha="255" value="256.471218654953" label="256"/>
        <item color="#bf3984" alpha="255" value="258.4493688729858" label="258"/>
        <item color="#c43e7f" alpha="255" value="260.4276199810181" label="260"/>
        <item color="#c9447a" alpha="255" value="262.40587108905027" label="262"/>
        <item color="#cd4a76" alpha="255" value="264.38412219708255" label="264"/>
        <item color="#d24f71" alpha="255" value="266.3623733051147" label="266"/>
        <item color="#d6556d" alpha="255" value="268.340624413147" label="268"/>
        <item color="#da5b69" alpha="255" value="270.3187746311798" label="270"/>
        <item color="#de6164" alpha="255" value="272.297025739212" label="272"/>
        <item color="#e26660" alpha="255" value="274.27527684724424" label="274"/>
        <item color="#e66c5c" alpha="255" value="276.25352795527647" label="276"/>
        <item color="#e97257" alpha="255" value="278.23177906330875" label="278"/>
        <item color="#ed7953" alpha="255" value="280.2100301713409" label="280"/>
        <item color="#f07f4f" alpha="255" value="282.1882812793732" label="282"/>
        <item color="#f3854b" alpha="255" value="284.166431497406" label="284"/>
        <item color="#f58c46" alpha="255" value="286.1446826054382" label="286"/>
        <item color="#f79342" alpha="255" value="288.12293371347045" label="288"/>
        <item color="#f99a3e" alpha="255" value="290.10118482150267" label="290"/>
        <item color="#fba139" alpha="255" value="292.0794359295349" label="292"/>
        <item color="#fca835" alpha="255" value="294.0576870375671" label="294"/>
        <item color="#fdaf31" alpha="255" value="296.0358372556" label="296"/>
        <item color="#feb72d" alpha="255" value="298.0140883636322" label="298"/>
        <item color="#febe2a" alpha="255" value="299.9923394716644" label="300"/>
        <item color="#fdc627" alpha="255" value="301.97059057969665" label="302"/>
        <item color="#fcce25" alpha="255" value="303.9488416877289" label="304"/>
        <item color="#fbd724" alpha="255" value="305.9270927957611" label="306"/>
        <item color="#f8df25" alpha="255" value="307.90524301379395" label="308"/>
        <item color="#f6e826" alpha="255" value="309.8834941218262" label="310"/>
        <item color="#f3f027" alpha="255" value="311.8617452298584" label="312"/>
        <item color="#f0f921" alpha="255" value="313.8399963378906" label="314"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="3" min-val="0" opacity="1" max-val="16500.7">
      <colorrampshader classificationMode="2" minimumValue="0" maximumValue="16500.7" clip="0" colorRampType="DISCRETE" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="255,0,255,0" type="QString" name="color1"/>
            <Option value="215,25,28,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="1.21207e-05;43,131,186,255:3.03017e-05;107,176,175,255:4.84828e-05;171,221,164,255:5.75733e-05;213,238,178,255:6.36337e-05;21,24,30,255:7.27242e-05;254,215,144,255:9.09052e-05;253,174,97,255:0.000121207;234,99,62,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="255,0,255,0"/>
          <prop k="color2" v="215,25,28,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="1.21207e-05;43,131,186,255:3.03017e-05;107,176,175,255:4.84828e-05;171,221,164,255:5.75733e-05;213,238,178,255:6.36337e-05;21,24,30,255:7.27242e-05;254,215,144,255:9.09052e-05;253,174,97,255:0.000121207;234,99,62,255"/>
        </colorramp>
        <item color="#ff00ff" alpha="0" value="0" label="FROUDE"/>
        <item color="#2b83ba" alpha="255" value="0.2" label="0.000000 - 0.200000"/>
        <item color="#6bb0af" alpha="255" value="0.5" label="0.200000 - 0.500000"/>
        <item color="#abdda4" alpha="255" value="0.8" label="0.500000 - 0.800000"/>
        <item color="#d5eeb2" alpha="255" value="0.95" label="0.800000 - 0.950000"/>
        <item color="#15181e" alpha="255" value="1.05" label="0.950000 - 1.050000"/>
        <item color="#fed790" alpha="255" value="1.2" label="1.050000 - 1.200000"/>
        <item color="#fdae61" alpha="255" value="1.5" label="1.200000 - 1.500000"/>
        <item color="#ea633e" alpha="255" value="2" label="1.500000 - 2.000000"/>
        <item color="#d7191c" alpha="255" value="inf" label="> 2.000000"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="4" min-val="0" opacity="1" max-val="382.186">
      <colorrampshader classificationMode="2" minimumValue="0" maximumValue="382.186" clip="0" colorRampType="DISCRETE" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="255,0,255,0" type="QString" name="color1"/>
            <Option value="0,0,4,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="2.61653e-05;255,0,255,3:0.000130826;252,253,191,255:0.000654132;254,201,141,255:0.00130826;253,149,103,255:0.00261653;241,96,93,255:0.00523305;205,63,113,255:0.0104661;158,47,127,255:0.0235487;114,31,129,255:0.0654132;68,15,118,255:0.261653;24,15,62,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="255,0,255,0"/>
          <prop k="color2" v="0,0,4,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="2.61653e-05;255,0,255,3:0.000130826;252,253,191,255:0.000654132;254,201,141,255:0.00130826;253,149,103,255:0.00261653;241,96,93,255:0.00523305;205,63,113,255:0.0104661;158,47,127,255:0.0235487;114,31,129,255:0.0654132;68,15,118,255:0.261653;24,15,62,255"/>
        </colorramp>
        <item color="#ff00ff" alpha="0" value="0" label="DEBIT"/>
        <item color="#ff00ff" alpha="3" value="0.01" label="0.000000 - 0.010000"/>
        <item color="#fcfdbf" alpha="255" value="0.05" label="0.010000 - 0.050000"/>
        <item color="#fec98d" alpha="255" value="0.25" label="0.050000 - 0.250000"/>
        <item color="#fd9567" alpha="255" value="0.5" label="0.250000 - 0.500000"/>
        <item color="#f1605d" alpha="255" value="1" label="0.500000 - 1.000000"/>
        <item color="#cd3f71" alpha="255" value="2" label="1.000000 - 2.000000"/>
        <item color="#9e2f7f" alpha="255" value="4" label="2.000000 - 4.000000"/>
        <item color="#721f81" alpha="255" value="9" label="4.000000 - 9.000000"/>
        <item color="#440f76" alpha="255" value="25" label="9.000000 - 25.000000"/>
        <item color="#180f3e" alpha="255" value="100" label="25.000000 - 100.000000"/>
        <item color="#000004" alpha="255" value="inf" label="> 100.000000"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="5" min-val="0" opacity="1" max-val="20.41871070861816">
      <colorrampshader classificationMode="1" minimumValue="0" maximumValue="20.41871070861816" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <item color="#1b068d" alpha="255" value="0.400365995832443" label="0.4"/>
        <item color="#260591" alpha="255" value="0.800734033535957" label="0.801"/>
        <item color="#2f0596" alpha="255" value="1.2011000293684" label="1.2"/>
        <item color="#38049a" alpha="255" value="1.601468067071915" label="1.6"/>
        <item color="#41049d" alpha="255" value="2.001834062904358" label="2"/>
        <item color="#4903a0" alpha="255" value="2.402200058736801" label="2.4"/>
        <item color="#5102a3" alpha="255" value="2.802570138311386" label="2.8"/>
        <item color="#5901a5" alpha="255" value="3.202940217885971" label="3.2"/>
        <item color="#6100a7" alpha="255" value="3.603310297460556" label="3.6"/>
        <item color="#6900a8" alpha="255" value="4.003659958324432" label="4"/>
        <item color="#7100a8" alpha="255" value="4.404030037899017" label="4.4"/>
        <item color="#7801a8" alpha="255" value="4.804400117473603" label="4.8"/>
        <item color="#8004a8" alpha="255" value="5.204770197048187" label="5.2"/>
        <item color="#8707a6" alpha="255" value="5.605140276622771" label="5.61"/>
        <item color="#8e0ca4" alpha="255" value="6.005510356197357" label="6.01"/>
        <item color="#9511a1" alpha="255" value="6.405860017061233" label="6.41"/>
        <item color="#9c179e" alpha="255" value="6.80623009663582" label="6.81"/>
        <item color="#a21d9a" alpha="255" value="7.2066001762104035" label="7.21"/>
        <item color="#a82296" alpha="255" value="7.60697025578499" label="7.61"/>
        <item color="#ae2892" alpha="255" value="8.007340335359572" label="8.01"/>
        <item color="#b42e8d" alpha="255" value="8.407710414934158" label="8.41"/>
        <item color="#ba3388" alpha="255" value="8.808080494508744" label="8.81"/>
        <item color="#bf3984" alpha="255" value="9.20843015537262" label="9.21"/>
        <item color="#c43e7f" alpha="255" value="9.608800234947205" label="9.61"/>
        <item color="#c9447a" alpha="255" value="10.00917031452179" label="10"/>
        <item color="#cd4a76" alpha="255" value="10.409540394096375" label="10.4"/>
        <item color="#d24f71" alpha="255" value="10.809910473670959" label="10.8"/>
        <item color="#d6556d" alpha="255" value="11.210280553245543" label="11.2"/>
        <item color="#da5b69" alpha="255" value="11.61063021410942" label="11.6"/>
        <item color="#de6164" alpha="255" value="12.011000293684004" label="12"/>
        <item color="#e26660" alpha="255" value="12.411370373258592" label="12.4"/>
        <item color="#e66c5c" alpha="255" value="12.811740452833176" label="12.8"/>
        <item color="#e97257" alpha="255" value="13.212110532407761" label="13.2"/>
        <item color="#ed7953" alpha="255" value="13.612480611982345" label="13.6"/>
        <item color="#f07f4f" alpha="255" value="14.01285069155693" label="14"/>
        <item color="#f3854b" alpha="255" value="14.413200352420807" label="14.4"/>
        <item color="#f58c46" alpha="255" value="14.813570431995391" label="14.8"/>
        <item color="#f79342" alpha="255" value="15.213940511569978" label="15.2"/>
        <item color="#f99a3e" alpha="255" value="15.614310591144562" label="15.6"/>
        <item color="#fba139" alpha="255" value="16.014680670719144" label="16"/>
        <item color="#fca835" alpha="255" value="16.415050750293734" label="16.4"/>
        <item color="#fdaf31" alpha="255" value="16.815400411157608" label="16.8"/>
        <item color="#feb72d" alpha="255" value="17.215770490732194" label="17.2"/>
        <item color="#febe2a" alpha="255" value="17.61614057030678" label="17.6"/>
        <item color="#fdc627" alpha="255" value="18.016510649881365" label="18"/>
        <item color="#fcce25" alpha="255" value="18.416880729455947" label="18.4"/>
        <item color="#fbd724" alpha="255" value="18.817250809030533" label="18.8"/>
        <item color="#f8df25" alpha="255" value="19.21760046989441" label="19.2"/>
        <item color="#f6e826" alpha="255" value="19.617970549468993" label="19.6"/>
        <item color="#f3f027" alpha="255" value="20.01834062904358" label="20"/>
        <item color="#f0f921" alpha="255" value="20.418710708618164" label="20.4"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="20.41871070861816" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="6" min-val="0" opacity="1" max-val="0">
      <colorrampshader classificationMode="1" minimumValue="0" maximumValue="0" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="0" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="7" min-val="212.9499969482422" opacity="1" max-val="313.8399963378906">
      <colorrampshader classificationMode="1" minimumValue="212.9499969482422" maximumValue="313.8399963378906" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="212.9499969482422" label="213"/>
        <item color="#1b068d" alpha="255" value="214.92822787827453" label="215"/>
        <item color="#260591" alpha="255" value="216.90646889730684" label="217"/>
        <item color="#2f0596" alpha="255" value="218.88469982733918" label="219"/>
        <item color="#38049a" alpha="255" value="220.86294084637146" label="221"/>
        <item color="#41049d" alpha="255" value="222.8411717764038" label="223"/>
        <item color="#4903a0" alpha="255" value="224.81940270643616" label="225"/>
        <item color="#5102a3" alpha="255" value="226.7976538144684" label="227"/>
        <item color="#5901a5" alpha="255" value="228.77590492250062" label="229"/>
        <item color="#6100a7" alpha="255" value="230.75415603053284" label="231"/>
        <item color="#6900a8" alpha="255" value="232.73230624856566" label="233"/>
        <item color="#7100a8" alpha="255" value="234.7105573565979" label="235"/>
        <item color="#7801a8" alpha="255" value="236.68880846463014" label="237"/>
        <item color="#8004a8" alpha="255" value="238.66705957266237" label="239"/>
        <item color="#8707a6" alpha="255" value="240.6453106806946" label="241"/>
        <item color="#8e0ca4" alpha="255" value="242.62356178872682" label="243"/>
        <item color="#9511a1" alpha="255" value="244.60171200675964" label="245"/>
        <item color="#9c179e" alpha="255" value="246.57996311479187" label="247"/>
        <item color="#a21d9a" alpha="255" value="248.5582142228241" label="249"/>
        <item color="#a82296" alpha="255" value="250.53646533085632" label="251"/>
        <item color="#ae2892" alpha="255" value="252.51471643888854" label="253"/>
        <item color="#b42e8d" alpha="255" value="254.49296754692077" label="254"/>
        <item color="#ba3388" alpha="255" value="256.471218654953" label="256"/>
        <item color="#bf3984" alpha="255" value="258.4493688729858" label="258"/>
        <item color="#c43e7f" alpha="255" value="260.4276199810181" label="260"/>
        <item color="#c9447a" alpha="255" value="262.40587108905027" label="262"/>
        <item color="#cd4a76" alpha="255" value="264.38412219708255" label="264"/>
        <item color="#d24f71" alpha="255" value="266.3623733051147" label="266"/>
        <item color="#d6556d" alpha="255" value="268.340624413147" label="268"/>
        <item color="#da5b69" alpha="255" value="270.3187746311798" label="270"/>
        <item color="#de6164" alpha="255" value="272.297025739212" label="272"/>
        <item color="#e26660" alpha="255" value="274.27527684724424" label="274"/>
        <item color="#e66c5c" alpha="255" value="276.25352795527647" label="276"/>
        <item color="#e97257" alpha="255" value="278.23177906330875" label="278"/>
        <item color="#ed7953" alpha="255" value="280.2100301713409" label="280"/>
        <item color="#f07f4f" alpha="255" value="282.1882812793732" label="282"/>
        <item color="#f3854b" alpha="255" value="284.166431497406" label="284"/>
        <item color="#f58c46" alpha="255" value="286.1446826054382" label="286"/>
        <item color="#f79342" alpha="255" value="288.12293371347045" label="288"/>
        <item color="#f99a3e" alpha="255" value="290.10118482150267" label="290"/>
        <item color="#fba139" alpha="255" value="292.0794359295349" label="292"/>
        <item color="#fca835" alpha="255" value="294.0576870375671" label="294"/>
        <item color="#fdaf31" alpha="255" value="296.0358372556" label="296"/>
        <item color="#feb72d" alpha="255" value="298.0140883636322" label="298"/>
        <item color="#febe2a" alpha="255" value="299.9923394716644" label="300"/>
        <item color="#fdc627" alpha="255" value="301.97059057969665" label="302"/>
        <item color="#fcce25" alpha="255" value="303.9488416877289" label="304"/>
        <item color="#fbd724" alpha="255" value="305.9270927957611" label="306"/>
        <item color="#f8df25" alpha="255" value="307.90524301379395" label="308"/>
        <item color="#f6e826" alpha="255" value="309.8834941218262" label="310"/>
        <item color="#f3f027" alpha="255" value="311.8617452298584" label="312"/>
        <item color="#f0f921" alpha="255" value="313.8399963378906" label="314"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="313.8399963378906" fixed-width="0.26" ignore-out-of-range="0" minimum-value="212.9499969482422" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="8" min-val="0" opacity="1" max-val="10800">
      <colorrampshader classificationMode="1" minimumValue="0" maximumValue="10800" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <item color="#1b068d" alpha="255" value="211.76424000000003" label="211.76"/>
        <item color="#260591" alpha="255" value="423.52956" label="423.53"/>
        <item color="#2f0596" alpha="255" value="635.2938" label="635.29"/>
        <item color="#38049a" alpha="255" value="847.05912" label="847.06"/>
        <item color="#41049d" alpha="255" value="1058.82336" label="1058.8"/>
        <item color="#4903a0" alpha="255" value="1270.5876" label="1270.6"/>
        <item color="#5102a3" alpha="255" value="1482.3539999999998" label="1482.4"/>
        <item color="#5901a5" alpha="255" value="1694.1204" label="1694.1"/>
        <item color="#6100a7" alpha="255" value="1905.8868" label="1905.9"/>
        <item color="#6900a8" alpha="255" value="2117.6424" label="2117.6"/>
        <item color="#7100a8" alpha="255" value="2329.4087999999997" label="2329.4"/>
        <item color="#7801a8" alpha="255" value="2541.1752" label="2541.2"/>
        <item color="#8004a8" alpha="255" value="2752.9416" label="2752.9"/>
        <item color="#8707a6" alpha="255" value="2964.7079999999996" label="2964.7"/>
        <item color="#8e0ca4" alpha="255" value="3176.4744" label="3176.5"/>
        <item color="#9511a1" alpha="255" value="3388.2299999999996" label="3388.2"/>
        <item color="#9c179e" alpha="255" value="3599.9964" label="3600"/>
        <item color="#a21d9a" alpha="255" value="3811.7628" label="3811.8"/>
        <item color="#a82296" alpha="255" value="4023.5292000000004" label="4023.5"/>
        <item color="#ae2892" alpha="255" value="4235.2955999999995" label="4235.3"/>
        <item color="#b42e8d" alpha="255" value="4447.062" label="4447.1"/>
        <item color="#ba3388" alpha="255" value="4658.8284" label="4658.8"/>
        <item color="#bf3984" alpha="255" value="4870.584" label="4870.6"/>
        <item color="#c43e7f" alpha="255" value="5082.3504" label="5082.4"/>
        <item color="#c9447a" alpha="255" value="5294.1168" label="5294.1"/>
        <item color="#cd4a76" alpha="255" value="5505.8832" label="5505.9"/>
        <item color="#d24f71" alpha="255" value="5717.6496" label="5717.6"/>
        <item color="#d6556d" alpha="255" value="5929.415999999999" label="5929.4"/>
        <item color="#da5b69" alpha="255" value="6141.1716" label="6141.2"/>
        <item color="#de6164" alpha="255" value="6352.937999999999" label="6352.9"/>
        <item color="#e26660" alpha="255" value="6564.7044000000005" label="6564.7"/>
        <item color="#e66c5c" alpha="255" value="6776.4708" label="6776.5"/>
        <item color="#e97257" alpha="255" value="6988.2372000000005" label="6988.2"/>
        <item color="#ed7953" alpha="255" value="7200.0036" label="7200"/>
        <item color="#f07f4f" alpha="255" value="7411.7699999999995" label="7411.8"/>
        <item color="#f3854b" alpha="255" value="7623.5256" label="7623.5"/>
        <item color="#f58c46" alpha="255" value="7835.2919999999995" label="7835.3"/>
        <item color="#f79342" alpha="255" value="8047.058400000001" label="8047.1"/>
        <item color="#f99a3e" alpha="255" value="8258.8248" label="8258.8"/>
        <item color="#fba139" alpha="255" value="8470.591199999999" label="8470.6"/>
        <item color="#fca835" alpha="255" value="8682.3576" label="8682.4"/>
        <item color="#fdaf31" alpha="255" value="8894.1132" label="8894.1"/>
        <item color="#feb72d" alpha="255" value="9105.8796" label="9105.9"/>
        <item color="#febe2a" alpha="255" value="9317.646" label="9317.6"/>
        <item color="#fdc627" alpha="255" value="9529.412400000001" label="9529.4"/>
        <item color="#fcce25" alpha="255" value="9741.1788" label="9741.2"/>
        <item color="#fbd724" alpha="255" value="9952.9452" label="9952.9"/>
        <item color="#f8df25" alpha="255" value="10164.7008" label="10165"/>
        <item color="#f6e826" alpha="255" value="10376.4672" label="10376"/>
        <item color="#f3f027" alpha="255" value="10588.2336" label="10588"/>
        <item color="#f0f921" alpha="255" value="10800" label="10800"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="10800" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="9" min-val="0" opacity="1" max-val="21.81872749328613">
      <colorrampshader classificationMode="1" minimumValue="0" maximumValue="21.81872749328613" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <item color="#1b068d" alpha="255" value="0.427817244942856" label="0.428"/>
        <item color="#260591" alpha="255" value="0.855636671758461" label="0.856"/>
        <item color="#2f0596" alpha="255" value="1.283453916701317" label="1.28"/>
        <item color="#38049a" alpha="255" value="1.711273343516922" label="1.71"/>
        <item color="#41049d" alpha="255" value="2.139090588459778" label="2.14"/>
        <item color="#4903a0" alpha="255" value="2.566907833402634" label="2.57"/>
        <item color="#5102a3" alpha="255" value="2.994729442090988" label="2.99"/>
        <item color="#5901a5" alpha="255" value="3.422551050779343" label="3.42"/>
        <item color="#6100a7" alpha="255" value="3.850372659467697" label="3.85"/>
        <item color="#6900a8" alpha="255" value="4.278172449428558" label="4.28"/>
        <item color="#7100a8" alpha="255" value="4.705994058116913" label="4.71"/>
        <item color="#7801a8" alpha="255" value="5.133815666805267" label="5.13"/>
        <item color="#8004a8" alpha="255" value="5.561637275493622" label="5.56"/>
        <item color="#8707a6" alpha="255" value="5.989458884181976" label="5.99"/>
        <item color="#8e0ca4" alpha="255" value="6.41728049287033" label="6.42"/>
        <item color="#9511a1" alpha="255" value="6.845080282831192" label="6.85"/>
        <item color="#9c179e" alpha="255" value="7.272901891519546" label="7.27"/>
        <item color="#a21d9a" alpha="255" value="7.700723500207901" label="7.7"/>
        <item color="#a82296" alpha="255" value="8.128545108896256" label="8.13"/>
        <item color="#ae2892" alpha="255" value="8.55636671758461" label="8.56"/>
        <item color="#b42e8d" alpha="255" value="8.984188326272964" label="8.98"/>
        <item color="#ba3388" alpha="255" value="9.412009934961318" label="9.41"/>
        <item color="#bf3984" alpha="255" value="9.83980972492218" label="9.84"/>
        <item color="#c43e7f" alpha="255" value="10.267631333610534" label="10.3"/>
        <item color="#c9447a" alpha="255" value="10.695452942298889" label="10.7"/>
        <item color="#cd4a76" alpha="255" value="11.123274550987244" label="11.1"/>
        <item color="#d24f71" alpha="255" value="11.551096159675598" label="11.6"/>
        <item color="#d6556d" alpha="255" value="11.978917768363951" label="12"/>
        <item color="#da5b69" alpha="255" value="12.406717558324814" label="12.4"/>
        <item color="#de6164" alpha="255" value="12.834539167013167" label="12.8"/>
        <item color="#e26660" alpha="255" value="13.262360775701524" label="13.3"/>
        <item color="#e66c5c" alpha="255" value="13.690182384389876" label="13.7"/>
        <item color="#e97257" alpha="255" value="14.118003993078233" label="14.1"/>
        <item color="#ed7953" alpha="255" value="14.545825601766586" label="14.5"/>
        <item color="#f07f4f" alpha="255" value="14.97364721045494" label="15"/>
        <item color="#f3854b" alpha="255" value="15.401447000415802" label="15.4"/>
        <item color="#f58c46" alpha="255" value="15.829268609104156" label="15.8"/>
        <item color="#f79342" alpha="255" value="16.257090217792513" label="16.3"/>
        <item color="#f99a3e" alpha="255" value="16.684911826480864" label="16.7"/>
        <item color="#fba139" alpha="255" value="17.11273343516922" label="17.1"/>
        <item color="#fca835" alpha="255" value="17.540555043857577" label="17.5"/>
        <item color="#fdaf31" alpha="255" value="17.968354833818434" label="18"/>
        <item color="#feb72d" alpha="255" value="18.39617644250679" label="18.4"/>
        <item color="#febe2a" alpha="255" value="18.823998051195144" label="18.8"/>
        <item color="#fdc627" alpha="255" value="19.251819659883502" label="19.3"/>
        <item color="#fcce25" alpha="255" value="19.679641268571853" label="19.7"/>
        <item color="#fbd724" alpha="255" value="20.107462877260208" label="20.1"/>
        <item color="#f8df25" alpha="255" value="20.53526266722107" label="20.5"/>
        <item color="#f6e826" alpha="255" value="20.963084275909424" label="21"/>
        <item color="#f3f027" alpha="255" value="21.390905884597778" label="21.4"/>
        <item color="#f0f921" alpha="255" value="21.818727493286133" label="21.8"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="21.81872749328613" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <scalar-settings interpolation-method="none" group="10" min-val="0" opacity="1" max-val="591.4968872070313">
      <colorrampshader classificationMode="1" minimumValue="0" maximumValue="591.4968872070313" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <item color="#1b068d" alpha="255" value="11.597952664978028" label="11.6"/>
        <item color="#260591" alpha="255" value="23.195964479644775" label="23.2"/>
        <item color="#2f0596" alpha="255" value="34.7939171446228" label="34.8"/>
        <item color="#38049a" alpha="255" value="46.39192895928955" label="46.4"/>
        <item color="#41049d" alpha="255" value="57.98988162426758" label="58"/>
        <item color="#4903a0" alpha="255" value="69.5878342892456" label="69.6"/>
        <item color="#5102a3" alpha="255" value="81.18590525360106" label="81.2"/>
        <item color="#5901a5" alpha="255" value="92.78397621795655" label="92.8"/>
        <item color="#6100a7" alpha="255" value="104.38204718231201" label="104"/>
        <item color="#6900a8" alpha="255" value="115.97952664978027" label="116"/>
        <item color="#7100a8" alpha="255" value="127.57759761413574" label="128"/>
        <item color="#7801a8" alpha="255" value="139.1756685784912" label="139"/>
        <item color="#8004a8" alpha="255" value="150.7737395428467" label="151"/>
        <item color="#8707a6" alpha="255" value="162.37181050720213" label="162"/>
        <item color="#8e0ca4" alpha="255" value="173.9698814715576" label="174"/>
        <item color="#9511a1" alpha="255" value="185.56736093902586" label="186"/>
        <item color="#9c179e" alpha="255" value="197.16543190338135" label="197"/>
        <item color="#a21d9a" alpha="255" value="208.76350286773683" label="209"/>
        <item color="#a82296" alpha="255" value="220.3615738320923" label="220"/>
        <item color="#ae2892" alpha="255" value="231.95964479644775" label="232"/>
        <item color="#b42e8d" alpha="255" value="243.5577157608032" label="244"/>
        <item color="#ba3388" alpha="255" value="255.1557867251587" label="255"/>
        <item color="#bf3984" alpha="255" value="266.75326619262694" label="267"/>
        <item color="#c43e7f" alpha="255" value="278.3513371569824" label="278"/>
        <item color="#c9447a" alpha="255" value="289.9494081213379" label="290"/>
        <item color="#cd4a76" alpha="255" value="301.5474790856934" label="302"/>
        <item color="#d24f71" alpha="255" value="313.1455500500488" label="313"/>
        <item color="#d6556d" alpha="255" value="324.74362101440425" label="325"/>
        <item color="#da5b69" alpha="255" value="336.34110048187256" label="336"/>
        <item color="#de6164" alpha="255" value="347.939171446228" label="348"/>
        <item color="#e26660" alpha="255" value="359.53724241058353" label="360"/>
        <item color="#e66c5c" alpha="255" value="371.13531337493896" label="371"/>
        <item color="#e97257" alpha="255" value="382.73338433929445" label="383"/>
        <item color="#ed7953" alpha="255" value="394.33145530364993" label="394"/>
        <item color="#f07f4f" alpha="255" value="405.92952626800536" label="406"/>
        <item color="#f3854b" alpha="255" value="417.52700573547367" label="418"/>
        <item color="#f58c46" alpha="255" value="429.1250766998291" label="429"/>
        <item color="#f79342" alpha="255" value="440.7231476641846" label="441"/>
        <item color="#f99a3e" alpha="255" value="452.32121862854" label="452"/>
        <item color="#fba139" alpha="255" value="463.9192895928955" label="464"/>
        <item color="#fca835" alpha="255" value="475.517360557251" label="476"/>
        <item color="#fdaf31" alpha="255" value="487.11484002471923" label="487"/>
        <item color="#feb72d" alpha="255" value="498.7129109890747" label="499"/>
        <item color="#febe2a" alpha="255" value="510.31098195343014" label="510"/>
        <item color="#fdc627" alpha="255" value="521.9090529177856" label="522"/>
        <item color="#fcce25" alpha="255" value="533.5071238821412" label="534"/>
        <item color="#fbd724" alpha="255" value="545.1051948464966" label="545"/>
        <item color="#f8df25" alpha="255" value="556.7026743139648" label="557"/>
        <item color="#f6e826" alpha="255" value="568.3007452783203" label="568"/>
        <item color="#f3f027" alpha="255" value="579.8988162426758" label="580"/>
        <item color="#f0f921" alpha="255" value="591.4968872070313" label="591"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <edge-settings stroke-width-unit="0">
        <mesh-stroke-width width-varying="0" use-absolute-value="0" maximum-value="591.4968872070313" fixed-width="0.26" ignore-out-of-range="0" minimum-value="0" maximum-width="3" minimum-width="0.26"/>
      </edge-settings>
    </scalar-settings>
    <vector-settings user-grid-height="10" user-grid-width="10" color="0,0,0,255" user-grid-enabled="0" coloring-method="0" filter-min="-1" filter-max="-1" group="0" symbology="0" line-width="0.25">
      <colorrampshader classificationMode="1" minimumValue="nan" maximumValue="nan" clip="0" colorRampType="INTERPOLATED" labelPrecision="6">
        <colorramp type="gradient" name="[source]">
          <Option type="Map">
            <Option value="13,8,135,255" type="QString" name="color1"/>
            <Option value="240,249,33,255" type="QString" name="color2"/>
            <Option value="0" type="QString" name="discrete"/>
            <Option value="gradient" type="QString" name="rampType"/>
            <Option value="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255" type="QString" name="stops"/>
          </Option>
          <prop k="color1" v="13,8,135,255"/>
          <prop k="color2" v="240,249,33,255"/>
          <prop k="discrete" v="0"/>
          <prop k="rampType" v="gradient"/>
          <prop k="stops" v="0.0196078;27,6,141,255:0.0392157;38,5,145,255:0.0588235;47,5,150,255:0.0784314;56,4,154,255:0.0980392;65,4,157,255:0.117647;73,3,160,255:0.137255;81,2,163,255:0.156863;89,1,165,255:0.176471;97,0,167,255:0.196078;105,0,168,255:0.215686;113,0,168,255:0.235294;120,1,168,255:0.254902;128,4,168,255:0.27451;135,7,166,255:0.294118;142,12,164,255:0.313725;149,17,161,255:0.333333;156,23,158,255:0.352941;162,29,154,255:0.372549;168,34,150,255:0.392157;174,40,146,255:0.411765;180,46,141,255:0.431373;186,51,136,255:0.45098;191,57,132,255:0.470588;196,62,127,255:0.490196;201,68,122,255:0.509804;205,74,118,255:0.529412;210,79,113,255:0.54902;214,85,109,255:0.568627;218,91,105,255:0.588235;222,97,100,255:0.607843;226,102,96,255:0.627451;230,108,92,255:0.647059;233,114,87,255:0.666667;237,121,83,255:0.686275;240,127,79,255:0.705882;243,133,75,255:0.72549;245,140,70,255:0.745098;247,147,66,255:0.764706;249,154,62,255:0.784314;251,161,57,255:0.803922;252,168,53,255:0.823529;253,175,49,255:0.843137;254,183,45,255:0.862745;254,190,42,255:0.882353;253,198,39,255:0.901961;252,206,37,255:0.921569;251,215,36,255:0.941176;248,223,37,255:0.960784;246,232,38,255:0.980392;243,240,39,255"/>
        </colorramp>
        <item color="#0d0887" alpha="255" value="0" label="0"/>
        <item color="#1b068d" alpha="255" value="0.4003660131663" label="0.4"/>
        <item color="#260591" alpha="255" value="0.800734068203759" label="0.801"/>
        <item color="#2f0596" alpha="255" value="1.20110008137006" label="1.2"/>
        <item color="#38049a" alpha="255" value="1.601468136407519" label="1.6"/>
        <item color="#41049d" alpha="255" value="2.00183414957382" label="2"/>
        <item color="#4903a0" alpha="255" value="2.402200162740119" label="2.4"/>
        <item color="#5102a3" alpha="255" value="2.802570259648737" label="2.8"/>
        <item color="#5901a5" alpha="255" value="3.202940356557356" label="3.2"/>
        <item color="#6100a7" alpha="255" value="3.603310453465974" label="3.6"/>
        <item color="#6900a8" alpha="255" value="4.0036601316630005" label="4"/>
        <item color="#7100a8" alpha="255" value="4.404030228571618" label="4.4"/>
        <item color="#7801a8" alpha="255" value="4.804400325480238" label="4.8"/>
        <item color="#8004a8" alpha="255" value="5.204770422388856" label="5.2"/>
        <item color="#8707a6" alpha="255" value="5.605140519297474" label="5.61"/>
        <item color="#8e0ca4" alpha="255" value="6.005510616206093" label="6.01"/>
        <item color="#9511a1" alpha="255" value="6.405860294403118" label="6.41"/>
        <item color="#9c179e" alpha="255" value="6.806230391311737" label="6.81"/>
        <item color="#a21d9a" alpha="255" value="7.206600488220356" label="7.21"/>
        <item color="#a82296" alpha="255" value="7.606970585128975" label="7.61"/>
        <item color="#ae2892" alpha="255" value="8.007340682037592" label="8.01"/>
        <item color="#b42e8d" alpha="255" value="8.40771077894621" label="8.41"/>
        <item color="#ba3388" alpha="255" value="8.80808087585483" label="8.81"/>
        <item color="#bf3984" alpha="255" value="9.208430554051855" label="9.21"/>
        <item color="#c43e7f" alpha="255" value="9.608800650960475" label="9.61"/>
        <item color="#c9447a" alpha="255" value="10.009170747869094" label="10"/>
        <item color="#cd4a76" alpha="255" value="10.409540844777712" label="10.4"/>
        <item color="#d24f71" alpha="255" value="10.80991094168633" label="10.8"/>
        <item color="#d6556d" alpha="255" value="11.210281038594948" label="11.2"/>
        <item color="#da5b69" alpha="255" value="11.610630716791974" label="11.6"/>
        <item color="#de6164" alpha="255" value="12.011000813700592" label="12"/>
        <item color="#e26660" alpha="255" value="12.411370910609213" label="12.4"/>
        <item color="#e66c5c" alpha="255" value="12.81174100751783" label="12.8"/>
        <item color="#e97257" alpha="255" value="13.21211110442645" label="13.2"/>
        <item color="#ed7953" alpha="255" value="13.612481201335067" label="13.6"/>
        <item color="#f07f4f" alpha="255" value="14.012851298243685" label="14"/>
        <item color="#f3854b" alpha="255" value="14.413200976440711" label="14.4"/>
        <item color="#f58c46" alpha="255" value="14.81357107334933" label="14.8"/>
        <item color="#f79342" alpha="255" value="15.21394117025795" label="15.2"/>
        <item color="#f99a3e" alpha="255" value="15.614311267166567" label="15.6"/>
        <item color="#fba139" alpha="255" value="16.014681364075184" label="16"/>
        <item color="#fca835" alpha="255" value="16.415051460983804" label="16.4"/>
        <item color="#fdaf31" alpha="255" value="16.81540113918083" label="16.8"/>
        <item color="#feb72d" alpha="255" value="17.21577123608945" label="17.2"/>
        <item color="#febe2a" alpha="255" value="17.616141332998065" label="17.6"/>
        <item color="#fdc627" alpha="255" value="18.016511429906686" label="18"/>
        <item color="#fcce25" alpha="255" value="18.416881526815306" label="18.4"/>
        <item color="#fbd724" alpha="255" value="18.817251623723923" label="18.8"/>
        <item color="#f8df25" alpha="255" value="19.21760130192095" label="19.2"/>
        <item color="#f6e826" alpha="255" value="19.617971398829567" label="19.6"/>
        <item color="#f3f027" alpha="255" value="20.018341495738188" label="20"/>
        <item color="#f0f921" alpha="255" value="20.418711592646805" label="20.4"/>
        <rampLegendSettings direction="0" useContinuousLegend="1" orientation="2" suffix="" maximumLabel="" prefix="" minimumLabel="">
          <numericFormat id="basic">
            <Option type="Map">
              <Option value="" type="QChar" name="decimal_separator"/>
              <Option value="6" type="int" name="decimals"/>
              <Option value="0" type="int" name="rounding_type"/>
              <Option value="false" type="bool" name="show_plus"/>
              <Option value="true" type="bool" name="show_thousand_separator"/>
              <Option value="false" type="bool" name="show_trailing_zeros"/>
              <Option value="" type="QChar" name="thousand_separator"/>
            </Option>
          </numericFormat>
        </rampLegendSettings>
      </colorrampshader>
      <vector-arrow-settings arrow-head-length-ratio="0.4" arrow-head-width-ratio="0.15">
        <shaft-length method="minmax" min="0.8" max="10"/>
      </vector-arrow-settings>
      <vector-streamline-settings seeding-method="0" seeding-density="0.15"/>
      <vector-traces-settings maximum-tail-length="100" particles-count="1000" maximum-tail-length-unit="0"/>
    </vector-settings>
    <mesh-settings-native color="0,0,0,255" line-width-unit="MM" enabled="0" line-width="0.26"/>
    <mesh-settings-edge color="0,0,0,255" line-width-unit="MM" enabled="0" line-width="0.26"/>
    <mesh-settings-triangular color="0,0,0,255" line-width-unit="MM" enabled="0" line-width="0.26"/>
    <averaging-3d method="0">
      <multi-vertical-layers-settings end-layer-index="1" start-layer-index="1"/>
    </averaging-3d>
  </mesh-renderer-settings>
  <mesh-simplify-settings reduction-factor="10" enabled="0" mesh-resolution="5"/>
  <blendMode>0</blendMode>
  <layerOpacity>1</layerOpacity>
</qgis>
