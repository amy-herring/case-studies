
<cfset whatever = "../">
<cfsetting showdebugoutput="no">
<cfset UMList = "fq,hq,dq,jq,kq,mq,lq,um,fs">
<cfset VTList = "vt,va">

<cfinclude template="../includes/checkURL.cfm">
<cfinclude template="../functions/funcs_GetCountrySectionCount.cfm">
<script src="../scripts/imgscale.js"></script>
<!---<cfif IsDefined("url.print")>
	<cfinclude template="../connections/udfs.cfm">
	<cfinclude template="../connections/GlobalVariables.cfm">
	<script type="text/javascript" src="../js/jquery-1.8.3.min.js"></script>
	<script type="text/javascript" src="../js/jquery.main.js"></script>
    <script type="text/javascript" src="../scripts/wfb_scripts.js" charset="utf-8"></script>
    

<cfelse>
	<link rel="stylesheet" type="text/css" href="../styles/wfb_styles.css">
</cfif>--->
<!---	<script type="text/javascript" src="../scripts/osx.js"></script>
	<script type="text/javascript" src="../scripts/osx/jquery.simplemodal.js"></script>
	
	<link rel="stylesheet" type="text/css" href="../scripts/osx/osx.css">
---><cfset regioncode = Lcase(getRegionCodeFromcountrycode(countrycode))>

<cfinclude template="../functions/funcs_regionname.cfm">
<cfinclude template="../functions/funcs_regionborder.cfm">
<cfinclude template="../functions/funcs_getCountryImageCount.cfm">

<!--- Check to see if call is coming from FORM or if countrycode is being passed via URL --->
<cfif IsDefined("FORM.CountrySelected")>
	<cfset countrycode = FORM.CountrySelected>
	<cfelseif IsDefined("URL.countrycode")>
	<cfset countrycode = URL.countrycode>
	<cfelse>
	<cfset countrycode = "NA">
</cfif>

<!--- If this country is one of the Iles Eparses Island group, then just redirect to ZZ.HTML --->
<cfif ListFind(IlesEparses, Lcase(countrycode))>
	<cfset regioncode = "afr">
</cfif>

<!--- Set the session variable to remember the countrycode --->
<cfset session.countrycode = "#countrycode#">
<cfquery name="Getcountrycodes" datasource="#APPLICATION.dsn#" >
	SELECT 	CODEVALUE, CODEDESC 
	FROM 	FACTBOOK.LOOKUP 
	WHERE 	PUBYEAR = #pubyear# 
	AND 		CODETYPE = 'GEO' 
	AND 		ACTIVE = 'Y'  
	ORDER BY	CODESEQ
</cfquery>

<!--- Define  the query to lookup the country name from the country code --->
<cfquery name="Getcountryname" datasource="#APPLICATION.dsn#" >
	SELECT 		CODEDESC 
	FROM 		FACTBOOK.LOOKUP 
	WHERE 		PUBYEAR = #PubYear# 
	AND 		CODETYPE = 'GEO' 
	AND 		UPPER(CODEVALUE) = '#uCase(countrycode)#'
</cfquery>
<cfset countryname = "#Getcountryname.CODEDESC#">
<cfquery name="GetFlagDesc" datasource="#APPLICATION.dsn#" >
	SELECT 		TEXT, SUBFIELD 
	FROM 		factbook.summary 
	WHERE 		PUBYEAR = #PubYear# 
	AND 		FIELDKEY = 2081 
	AND 		UPPER(GEOCODE) = '#Ucase(countrycode)#'
</cfquery>

<cfquery name="GetAffiliation" datasource="#APPLICATION.dsn#" >
	SELECT 		TEXT 
	FROM 		FACTBOOK.SUMMARY 
	WHERE 		PUBYEAR = #PubYear# 
	AND	 		FIELDKEY = 2005 
	AND 		UPPER(GEOCODE) = '#uCase(countrycode)#'
</cfquery>

<cfquery name="GetLastUpdatedDate" datasource="#APPLICATION.dsn#" >
	SELECT 		MAX(FACTBOOK.FACTS.LAST_UPDATE) AS UPDATEDDATE
	FROM 		FACTBOOK.FACTS
	WHERE 		FACTBOOK.FACTS.PUBYEAR = #pubyear# 
	AND 		FACTBOOK.FACTS.GEOCODE = '#uCase(countrycode)#'
</cfquery>
<cfset UPDATEDDATE = GetLastUpdatedDate.UPDATEDDATE>
<cfquery name = "GetImageCount" datasource="wfb" >
	SELECT 		*
	FROM 		FACTBOOK.PHOTO_IMAGES
	WHERE 		GEOCODE = '#lcase(url.countrycode)#'
</cfquery>

<!---<cfdump var="#getImageCount#">
<cfabort>--->



<cfquery name="gettime" datasource="#APPLICATION.dsn#" maxrows="1">
			SELECT 		rc.region_code, rc.country_code, r.region_name, l.codedesc, l.codevalue, s.text
			FROM 		factbook.summary s, factbook.region_country rc, factbook.regions r, factbook.lookup l
			WHERE 		s.PUBYEAR = #PubYear# 
			and			l.codevalue = '#UCase(countrycode)#'
			AND			s.fieldkey = 2057
			and 		s.factseq = 6
			and			l.pubyear = 2014
			and			l.codetype = 'GEO'
			and 		rc.country_code = l.codevalue
			order by	region_code, codedesc
		</cfquery>
		<!---<cfloop query ="gettime">
		   <script>
            $(document).ready(function(){
					<cfset utc = listGetAt(text,1, '(')>
					<cfset utcLength = Len(utc)>
					<cfset utc1 = #RemoveChars(utc,1,3)#>
					<cfset utc2 = #ltrim(utc1)#>
					<cfset utc3 = #rtrim(utc2)#>
					<cfset utc4 = utc3 + 0>
					<cfset utc5 = utc4 + 1>
					$("#clock_<cfoutput>#codevalue#</cfoutput>").jClocksGMT({offset: "<cfoutput>#rtrim(utc5)#</cfoutput>", hour24: true});
            });
        	</script>
		</cfloop>--->



<!--- If this country is one of the Iles Eparses Island group, then just redirect to ZZ.HTML --->
<cfif ListFind(IlesEparses, Lcase(countrycode))>
	<cfset regioncode = "afr">
</cfif>

<!---<script>
				$('a').hide();
				
				$('a["href="]').click(function(e){
					$(this).attr("href","javascript:void(0)");
					$(this).css({"background-color":"##006","font-color":"##000"});
				})
				
				</script>--->

<!---
<div class="full" style="vertical-align: top; top: 0px; border-bottom: 1px solid #ccc; ">
	<table width="100%" border="0" cellpadding="5" cellspacing="0">
		<tr valign="top">
			<td class="pageTitle"></td>
			<td align="right"><span class="right"><a href="javascript:void(0);" class="button" id="newReview"><span>New Review</span></a></span></td>
		</tr>
	</table>
</div>--->


<cfoutput>
<cfif IsDefined("url.print")>
	<table width="100%" border="0" cellpadding="0" cellspacing="0">
		<cfelse>
	<table width="100%" border="0" cellpadding="0" cellspacing="0">
</cfif>

		<cfif #regioncode# is "oce" or #countrycode# is "xx">
			<cfif #countrycode# EQ "xx">
				<tr class="#regioncode#_dark">
					<td height="25"  align="left" style="padding-left:3px;"><div class="region_name1">&nbsp;#countryname#</div></td>
			<cfelse>
					<tr>
						<td height="25" class="#regioncode#_dark"><div class="region1">
						<cfif isDefined("staticOut")>
							<cfif #regionCode# is "oce"> 
								<cfif prefix EQ "wfbext_"> 
								 #sRegionPageStatic_ext#
								<cfelse>
								#sRegionpageStatic_int#
								</cfif>
							<cfelse>
								<cfif prefix EQ "wfbext_">
								#sRegionPage_ext#
								<cfelse>
								#sRegionpage_int#
								</cfif>
							</cfif>
							<span style="color: ##FFFFFF;">#region#</a> <strong>:: </strong></span>
						<cfelse>
							<cfif #prefix# EQ "wfbext_"><a href="#sRegionPage_ext#">#region#</a><strong><cfelse>#region#</cfif> :: </strong>
						</cfif>
							<span class="region_name1">#CountryName#</span> </div></td>
			</cfif>
		<cfelse>
				<tr class="#regioncode#_dark">
					<td  valign="middle">
						<table border="0" cellpadding="0" cellspacing="0">
							<tr>
								<td <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle">
									<div class="region1">
										<a href="<cfif isDefined("staticOut")> <cfif prefix EQ "wfbext_">#sRegionPageStatic_ext#<cfelse>#sRegionpageStatic_int#</cfif><cfelse><cfif prefix EQ "wfbext_">#sRegionPage_ext#<cfelse>#sRegionpage_int#</cfif></cfif>" style="color: ##FFFFFF;">#region#</a> <strong>:: </strong><span class="region_name1">#countryname#</span> 
									</div>
									<cfif #GetAffiliation.TEXT# NEQ "">
										<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
									</cfif>
								</td>
							</tr>
						</table>
					</td>
				</cfif>
				<cfif isDefined("url.print")>
					<cfelse><td width="20" align="right" valign="middle" class="#regioncode#_dark"><a href="print/country/countrypdf_#countryCode#.pdf"><img src="../graphics/print.gif" style="padding: 3px;"></a></td>
				</tr></cfif>
		</table>
	<cfif regioncode EQ "oce"> 
	<cfif isDefined("url.print")>
	<cfelse>
		<table width="100%" border="0" cellspacing="0" cellpadding="0" style="background-image:url(../graphics/#regioncode#_lgmap_bkgrnd.jpg); background-repeat: repeat-x; background-position: top ;">
			<tr>
				<td colspan="3" align="center" valign="top" style="  ">
				<a href="javascript:void(0);" title="Click map to enlarge"> 
					<img src="../graphics/maps/#LCase(countrycode)#-map.gif" 
					border="0" 
					style="cursor:pointer; border: 1px solid ##CCC; background-color: white; "  
					id="mapDialog2_#countrycode#" 
					name="#countrycode#" 
					regioncode="#regioncode#" 
					countrycode="#countrycode#"  
					countryname="#countryname#" 
					flagsubfield="" 
					countryaffiliation=""
					flagdescription="" 
					flagdescriptionnote="" 
					region="#region#" 
					class="mapFit #regioncode#_lgflagborder"
					typeimage = "map"></a></td>
			</tr>
			<tr>
				<cfset currPath = GetDirectoryFromPath(GetCurrentTemplatePath())>
				<cfset i = #getImageCount.RecordCount#>
				<!---<cfif #i# LESS THAN 1>--->
				<cfif #getImageCount.RecordCount# LESS THAN 1>
						<td width="49%" height="15" align="left" valign="top" class="smalltext_nav" style="#textcolor#; letter-spacing: 1px; text-align: center; padding-bottom: 3px;">
				<cfelse>
					<td colspan="2" align="center" valign="top">
						<div class="smalltext_nav">
							<a href="javascript:void(0);" 
								name="#countrycode#" 
								regioncode="#regioncode#" 
								countrycode="#countrycode#"  
								countryname="#countryname#" 
								region="#region#" 
								border="0"
								class="photoDialog" > view <span style="#textcolor#; letter-spacing:1px;">
									<strong>#i#
										<cfif i GREATER THAN 1>
											photos of <br>#Getcountryname.CODEDESC#
										<cfelse>
											photo of <br>#Getcountryname.CODEDESC#
										</cfif>
									</strong>
							</span></a>
						</div>
					</td>
				</cfif>
			</tr>
		</table>
		</cfif>
		<cfelse>
		<cfif isDefined("url.print")>
			<cfelse>
				<table width="100%" border="0" cellspacing="0" cellpadding="0" style="background-image:url(../graphics/#regioncode#_lgmap_bkgrnd.jpg); background-repeat: repeat-x; background-position: top ;">
			<tr>
				<td width="323" align="center" valign="top" style="  "><table width="100%" align="center" style="border: 1px solid ##ccc; height: 195px;" >
						<tr>
							<td colspan="2" align="left" valign="middle" class="smalltext_nav" style="height: 12px;" >Page last updated on #DateFormat(UPDATEDDATE, "long")# </td>
						</tr>
						<tr>
							<td height="230" align="center" valign="middle" class="area" style="width: 50%; height: 120px;<cfif ListFind(NoFlagsList, Lcase(countrycode)) > display: none;</cfif>">
							
							
								<cfif ListFind(NoFlagsList, Lcase(countrycode)) >
									<cfset flagsubfield = "">
									<cfset countryAffiliation =  "">
									<cfset flagdescriptionnote = "">
									<cfset flagdescription = "">
								<cfelse>
								<cfset countryAffiliation =  "">
									<cfif #GetAffiliation.TEXT# NEQ "">
										<cfset countryAffiliation =  GetAffiliation.TEXT>
									</cfif>
									<cfloop query="GetFlagDesc">
										<cfset flagsubfield = "">
										<cfset flagdescriptionnote = "">
										<cfif Trim(LCase(GetFlagDesc.SUBFIELD)) IS  "note">
											<cfset flagsubfield = GetFlagDesc.SUBFIELD>
											<cfset flagdescriptionnote = HTMLEditFormat (GetFlagDesc.TEXT)>
											<cfelse>							
											<cfset flagdescription = HTMLEditFormat (GetFlagDesc.TEXT)>
										</cfif>
									</cfloop>
									<a href="javascript:void(0);" title="Click flag for description"> <img src="../graphics/flags/large/#lcase(countrycode)#-lgflag.gif" 
												border="0" 
												style="cursor:pointer; border: 1px solid ##CCC; "  
												id="flagDialog2_#countrycode#" 
												name="#countrycode#" 
												regioncode="#regioncode#" 
												countrycode="#countrycode#"  
												countryname="#countryname#" 
												flagsubfield="#flagsubfield#" 
												countryaffiliation="#countryaffiliation#"
												flagdescription="#flagdescription# #flagdescriptionnote#" 
												flagdescriptionnote="#flagdescriptionnote#" 
												region="#region#" 
												class="flagFit #regioncode#_lgflagborder"
												typeimage = "flag"></a>
								</cfif>
							</td>
							<td align="center" valign="middle" class="area" style="width: 50%; height: 120px;"><a href="javascript:void(0);" title="Click locator to enlarge"> <img src="../graphics/locator/#lcase(regioncode)#/#lcase(countrycode)#_large_locator.gif" 
											border="0" 
											style="cursor:pointer; border: 1px solid ##CCC;" 
											id="locatorDialog2_#countrycode#" 
											name="#countrycode#" 
											regioncode="#regioncode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											flagsubfield="" 
											countryaffiliation=""
											flagdescription="" 
											flagdescriptionnote="" 
											region="#region#" 
											class="locatorFit #regioncode#_lgflagborder"
											typeimage = "locator"></a></td>
						</tr>
					</table></td>
				<td width="1%" rowspan="2" align="center" valign="middle" bgcolor="##FFFFFF" style="border: 1px solid ##fff;">&nbsp;</td>
				<td rowspan="2" align="center" valign="middle" style="border: 1px solid ##E4D4D4;">
					<div align="center" valign="middle" > <a href="javascript:void(0);" title="Click map to enlarge"> <img src="../graphics/maps/#LCase(countrycode)#-map.gif" 
					border="0" 
					style="cursor:pointer; border: 1px solid ##CCC; display: block; "  
					id="mapDialog2_#countrycode#" 
					name="#countrycode#"  
					regioncode="#regioncode#" 
					countrycode="#countrycode#"  
					countryname="#countryname#" 
					flagsubfield="" 
					countryaffiliation=""
					flagdescription="" 
					flagdescriptionnote="" 
					region="#region#" 
					class="mapFit #regioncode#_lgflagborder"
					typeimage = "map"></a>
					</div></td>
			</tr>
			<tr>
			<cfif isDefined("url.print")>
				<cfelse>
				<td height="140" align="center" valign="top" class="photo_bkgrnd_static" bgcolor="##FFFFFF">
					<table width="100%" border="0" align="left" cellpadding="0" cellspacing="0">
						<tr>
							<td height="10" colspan="3"></td>
						</tr>
						<tr>
							
							<td <cfif isDefined("url.staticOut")>width="100%"<cfelse>width="54%"</cfif> rowspan="3" align="center" valign="middle" class="smalltext_nav" >				
								<cfset currPath = GetDirectoryFromPath(GetCurrentTemplatePath())>
								<cfset i = #getImageCount.RecordCount#>
								<cfif NOT isDefined("URL.print")>
									<cfif #getImageCount.RecordCount# GREATER THAN 0>
										<a href= "javascript:void(0);" title="Photos of #Getcountryname.CODEDESC#" > 
										<img src="../graphics/photo_on.gif" 
											name="#countrycode#" 
											regioncode="#regioncode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											region="#region#" 
											width="123" 
											height="81"  
											border="0"
											id="photoDialog"
											<cfif isDefined("url.staticOut")> style="padding-top:10px;"</cfif>
											/></a>
										<div class="smalltext_nav"> view <span style="#textcolor#; letter-spacing:1px;">
											<a href="javascript:void(0);" name="#countrycode#" 
											regioncode="#regioncode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											region="#region#" 
											width="123" 
											height="81"  
											border="0"
											class="photoDialog" ><strong>#i#
												<cfif #i# GREATER THAN 1>
													photos
													<cfelse>
													photo
												</cfif>
											</strong></a>
											</span>
											of <br>#countryname#
										</div>
										<cfelse>
										<img src="../graphics/photo_off.jpg" alt="No Photos available for #Getcountryname.CODEDESC#" name="Photos" width="123" height="81" border="0" id="Photos3" title="No Photos Avaliable for #Getcountryname.CODEDESC#"/>
										<div> <span class="photo_text1">&nbsp;&nbsp;no photos available</span></div>
									</cfif>
								</cfif>
							</td>
						</cfif>
						<cfif isDefined("url.staticOut") or isDefined("url.print")>
							<cfelse>
							<td align="center" valign="top">
									<div> <span class="photo_text" style="padding-bottom: 8px; color: ##A7976D;"> <a href="../docs/#prefix#submitphoto.cfm"> <img src="../graphics/camera_off.gif" alt="Submit Photos of #Getcountryname.CODEDESC#" name="Submit Photos" width="63" height="50" vspace="3" border="0" id="Submit Photos" title="Submit Photos of #Getcountryname.CODEDESC#" /></a><br />
										DO YOU HAVE<br />
										PHOTOS TO SUBMIT? </span></div>
									<div style="margin-top: 0px;"> <img src="../graphics/photo_arrow.gif" alt="Photo Arrow" width="10" height="7" style="vertical-align:bottom; padding-bottom:2px;" border="0"> <a href="../docs/#prefix#submitphoto.cfm" class="photo_text1" style="color:##999999; padding-right:10px;"><strong>CLICK HERE</strong></a></div>
							</td>
							</cfif>
						</tr>
						<tr>
							<td height="50%" align="center" valign="top" ></td>
						</tr>
					</table>
					</cfif>
				</td>
			</tr>
		</table>
	</cfif>
</cfoutput>
<cfset categoryStructArray = ArrayNew(1)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Intro">
<cfset categoryStruct.SectionName = "Introduction">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Geo">
<cfset categoryStruct.SectionName = "Geography">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "People">
<cfset categoryStruct.SectionName = "People and Society">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Govt">
<cfset categoryStruct.SectionName = "Government">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Econ">
<cfset categoryStruct.SectionName = "Economy">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Energy">
<cfset categoryStruct.SectionName = "Energy">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Comm">
<cfset categoryStruct.SectionName = "Communications">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Trans">
<cfset categoryStruct.SectionName = "Transportation">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Military">
<cfset categoryStruct.SectionName = "Military">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>
<cfset categoryStruct = StructNew()>
<cfset categoryStruct.CatCode = "Issues">
<cfset categoryStruct.SectionName = "Transnational Issues">
<cfset temp = ArrayAppend(categoryStructArray, categoryStruct)>

<cfquery name="newGetCountrySection" datasource="#APPLICATION.dsn#" >
	SELECT		f.catcode, COUNT(f.fielddesc) AS sectionCount 
	FROM		factbook.fields f, factbook.summary s 
	WHERE		f.pubyear = #pubyear#  AND 
				s.pubyear = f.pubyear AND 
				s.geocode = '#Ucase(countrycode)#' AND 
				f.active = 'Y' AND 
				f.fieldkey = s.fieldkey AND
				f.fieldkey <> 2262
	GROUP BY 	f.catcode
	ORDER BY	sectionCount
</cfquery>

<cfquery name="GetCountrySection" datasource="#APPLICATION.dsn#" >
	SELECT		f.fieldseq, f.catcode, s.geocode, s.factseq, f.fielddesc, s.fieldkey, s.subfield, s.text 
	FROM		factbook.fields f, factbook.summary s
	WHERE	 	f.pubyear = #pubyear# AND 
				s.pubyear = f.pubyear AND 
				s.geocode = '#uCase(countrycode)#' AND 
				f.active = 'Y' AND 
				f.fieldkey = s.fieldkey AND
				f.fieldkey <> 2262
				
	ORDER BY	f.fieldseq, s.factseq 
</cfquery>




<!--- Query moved from countrytemplateoutput.cfm.  It's an independent query.  Therefore it only needs to be run once.  --->
<cfquery name="GetNotesAndDefs" datasource="#APPLICATION.dsn#"  cachedwithin="#CreateTimeSpan(1,0,0,0)#">
	SELECT 		TERM 
	FROM 		FACTBOOK.NOTES_DEFS 
	ORDER BY 	TERM
</cfquery>
<!--- This function is actually used in countrytemplateoutput.cfm.  But, it's included here to avoid multiple definitions ---> 
	<cfinclude template="../functions/funcs_isCategoryCodeAvailable.cfm">
	<div id="countryInfo" <cfif NOT isDefined("url.print")>style="display: none;"</cfif>>
		<cfif isDefined("url.print")>
			<cfelse>
				<div class="wrapper">
					<div style="float:right" class="expand_all">
						<a href="javascript:void(0)" class="expand">EXPAND ALL</a><a href="javascript:void(0)" class="collapse" style="display: none;">COLLAPSE ALL</a> 
					</div>
				</div>
		</cfif>
		<cfloop from="1" to="#ArrayLen(categoryStructArray)#" index="ii">
			<cfset CatCode_FromCountryTemplate = categoryStructArray[ii].CatCode>
			<cfset SectionName = categoryStructArray[ii].SectionName>
			<cfinclude template="countrytemplateoutput.cfm">
		</cfloop>
		<cfif isDefined("url.print")>
			<cfelse>
				<div class="wrapper">
					<div style="float:right; margin-top: 0px;" class="expand_all">
						<a href="javascript:void(0)" class="expand">EXPAND ALL</a><a href="javascript:void(0)" class="collapse" style="display: none;">COLLAPSE ALL</a> 
					</div>
				</div>
		</cfif>
<cfoutput>
	<div id="flagDialog" style="display: none" title="The World Factbook"></div>
	<div id="photoDialogWindow" style="display:none;" title="The World Factbook">
		<!---<cfinclude template="wfb_photo_gallery/#countrycode#_photogallery.html">--->
	</div>
<!---		<div id="container" style="display: block; background-color: ##fff;">
		<div id="content"> 
			<!-- modal content -->
			<div id="osx-modal-content">
				<div id="osx-modal-data">
				<cfinclude template="wfb_photo_gallery/#countrycode#_photogallery.html">
					<p></p>
					<p>&nbsp;</p>
				</div>
			</div>
		</div>
	</div>--->
	
</cfoutput>
