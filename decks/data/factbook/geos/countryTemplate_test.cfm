<cfsetting showdebugoutput="no">
<style>
﻿.flag_border {
     width:80px;
     max-height: 55px;
     vertical-align: middle;
     display: table-cell;
}
</style>
<cfset UMList = "fq,hq,dq,jq,kq,mq,lq,um,fs">
<cfinclude template="../functions/funcs_GetCountrySectionCount.cfm">
<cfset regionCode = Lcase(getRegionCodeFromCountryCode(countryCode))>
<cfinclude template="../functions/funcs_regionname.cfm">
<cfinclude template="../functions/funcs_regionborder.cfm">
<cfinclude template="../functions/funcs_getCountryImageCount.cfm">

<!--- Check to see if call is coming from FORM or if CountryCode is being passed via URL --->
<cfif IsDefined("FORM.CountrySelected")>
	<cfset CountryCode = FORM.CountrySelected>
	<cfelseif IsDefined("URL.CountryCode")>
	<cfset CountryCode = URL.CountryCode>
	<cfelse>
	<cfset CountryCode = "NA">
</cfif>

<!--- If this country is one of the Iles Eparses Island group, then just redirect to ZZ.HTML --->
<cfif ListFind(IlesEparses, Lcase(CountryCode))>
	<cfset regionCode = "afr">
</cfif>

<!--- Set the session variable to remember the CountryCode --->
<cfset session.CountryCode = "#CountryCode#">

<!--- Define  the query to lookup the country name from the country code --->
<cfquery name="GetCountryName" datasource="#application.dsn#">
	SELECT 		CODEDESC 
	FROM 		FACTBOOK.LOOKUP 
	WHERE 		PUBYEAR = #PubYear# 
				AND 		CODETYPE = 'GEO' 
				AND 		UPPER(CODEVALUE) = '#uCase(CountryCode)#'
</cfquery>
<cfset CountryName = "#GetCountryName.CODEDESC#">

<!--- Define  the query to lookup the country affiliation from the summary table --->
<cfquery name="GetAffiliation" datasource="#application.dsn#">
	SELECT 		TEXT 
	FROM 		FACTBOOK.SUMMARY 
	WHERE 		PUBYEAR = #PubYear# 
	AND	 		FIELDKEY = 2005 
	AND 			UPPER(GEOCODE) = '#uCase(CountryCode)#'
</cfquery>
<cfquery name="GetLastUpdatedDate" datasource="#application.dsn#">
	SELECT 	MAX(FACTBOOK.FACTS.LAST_UPDATE) AS UPDATEDDATE
	FROM 	FACTBOOK.FACTS
	WHERE 	FACTBOOK.FACTS.PUBYEAR = #pubyear# AND 
			FACTBOOK.FACTS.GEOCODE = '#uCase(CountryCode)#'
</cfquery>
<!------------------------------------------------------------------------------------------------------------------begin of setting up dialogs----->

<cfset UPDATEDDATE = GetLastUpdatedDate.UPDATEDDATE>
<cfquery name="GetFlagDesc" datasource="#application.dsn#">
SELECT TEXT, SUBFIELD FROM factbook.summary WHERE PUBYEAR = #PubYear# AND FIELDKEY = 2081 AND UPPER(GEOCODE) = '#Ucase(CountryCode)#'
</cfquery>
<cfquery name="GetAffiliation" datasource="#application.dsn#">
	SELECT 		TEXT 
	FROM 		FACTBOOK.SUMMARY 
	WHERE 		PUBYEAR = #PubYear# 
	AND 			FIELDKEY = 2005 
	AND 			UPPER(GEOCODE) = '#uCase(CountryCode)#'
</cfquery>
<cfif #GetAffiliation.TEXT# EQ "">
	<cfset rowAbeg = "<div class='#regionCode#_dark'><table><tr><td style='height:25px'><span class='region_name1'>">
	<cfset rowA_Middle = "">
	<cfset rowAend = "</span></td></tr></table></div>">
	<cfelse>
	<cfset rowAbeg = "<div class='#regionCode#_dark'><table><tr><td style='height:30px'><span class='region_name1'>">
	<!--- country name goes here --->
	<cfif #GetAffiliation.TEXT# NEQ "">
		<cfset countryAffiliation =  "#GetAffiliation.TEXT#">
		<cfset rowA_Middle = "<br><span class='affiliation'>#countryAffiliation#</span>">
	</cfif>
	<cfset rowAend = "</span></td></tr></table></div>">
</cfif>
<cfset rowBbeg = "<table class='#regionCode#_medium' cellspacing='0' cellpadding='20' border = '0' style='background:url(../graphics/#regionCode#_lgmap_bkgrnd.jpg); border: 2px solid ##999;' width='100%'><tr><td align='center' valign='middle' style='width:500px;'>">
<cfset flagImage = "<img src = '../graphics/flags/large/#lcase(CountryCode)#-lgflag.gif' class='#regioncode#_lgflagborder flagFit2'>">
<cfset rowBend = "</td>">
<cfset rowBbeg1 = "<td id='flag_caption' style='vertical-align: top; padding: 15px; margin: 20px;'><table width='238' border='0' cellpadding='0' cellspacing='0' style='margin: 1px; vertical-align: top;'><tr><td height='20' class='#regionCode#_medium' style='font-size:11px; padding-left:5px; font-weight: bold; border:1px solid ##FFFFFF;'>Flag Description</td>
</tr><tr>
<td valign='top'><div class='photogallery_captiontext' style='height: 375px; background-color: ##FFF;'><span class='category_data'>">
<!--- flag description in here --->
<cfset rowBend2 = "</span></td></tr></table></td></tr></table>">
<cfset flagDescription = "#getFlagDesc.text#">
<cfset tempflagDescription = Replace(#flagDescription#,'"','&quot;','all')>
<cfset countryMap = "<img src = '../graphics/maps/#lcase(CountryCode)#-map.gif'>">
<cfset countryLocator = "<img src = '../graphics/locator/#regionCode#/#lcase(CountryCode)#_large_locator.gif'>">
<cfset countryFlag = "<img src = '../graphics/flags/#lcase(CountryCode)#_large_locator.gif'>">

<!------------------------------------------------------------------------------------------------------------------end of setting up dialogs----->

<cfoutput>

<table width="100%" border="0" cellpadding="0" cellspacing="0">
	<cfif #regionCode# is "oce" or #countryCode# is "xx">
		<cfif #countryCode# EQ "xx">
			<tr>
				<td height="25" class="#regionCode#_dark" align="left" style="padding-left:3px;"><div class="region_name1">&nbsp;#CountryName#</div></td>
				<cfelse>
				<tr>
			
			<td height="25" class="#regionCode#_dark"><div class="region1">
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
						<a href="<cfif #prefix# EQ "wfbext_">#sRegionPage_ext#<cfelse>#sRegionpage_int#</cfif>">#region#</a><strong> :: </strong>
					</cfif>
					<span class="region_name1">#CountryName#</span> </div></td>
		</cfif>
		<cfelse>
			<tr>
		
		<td class="#regionCode#_dark" valign="middle"><table border="0" cellpadding="0" cellspacing="0">
				<tr>
					<td <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle"><div class="region1"><a href="<cfif isDefined("staticOut")> <cfif prefix EQ "wfbext_">#sRegionPageStatic_ext#<cfelse>#sRegionpageStatic_int#</cfif><cfelse><cfif prefix EQ "wfbext_">#sRegionPage_ext#<cfelse>#sRegionpage_int#</cfif></cfif>" style="color: ##FFFFFF;">#region#</a> <strong>:: </strong><span class="region_name1">#CountryName#</span> </div>
						<cfif #GetAffiliation.TEXT# NEQ "">
							<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
						</cfif></td>
				</tr>
			</table></td>
	</cfif>
	<!--- Section to printing --->
	<td width="80" align="right" valign="middle" class="#regionCode#_dark"><cfif isDefined("staticOut")>
			<cfif isDefined("URL.countryCode")>
				<cfif NOT isDefined("URL.print")>
					<div id="print_country"> <a href="countrytemplate_#URL.countryCode##globalStaticOutputExtension#" target="_blank" onClick="MM_openBrWindow('countrytemplate_#URL.countryCode##globalStaticOutputExtension#','','status=no,scrollbars=yes,resizable=yes'); return false" title="Print Page" > <img src="../graphics/print.gif" alt="Print #CountryName#" width="25" height="18" border="0"></a> </div>
				</cfif>
			</cfif>
			<cfelse>
			<cfif isDefined("URL.countryCode")>
				<cfif NOT isDefined("URL.print")>
					<div id="print_country" align="right"> <a href="../geos/countrytemplate.cfm?countryCode=#URL.countryCode#&regionCode=#regionCode#&print=" target="_blank" onClick="MM_openBrWindow('../geos/countrytemplate.cfm?countryCode=#URL.countryCode#&regionCode=#regionCode#&print=','','status=no,scrollbars=yes,resizable=yes');return false" style="color:##FFFFFF;text-decoration: none;" title="<span style='font-weight: bold; height: 24px; width:24px; font-size: 11px;'>Print Country Data Page for #CountryName#</span>" > <img src="../graphics/print.gif" width="25" height="18" border="0"></a></div>
				</cfif>
			</cfif>
		</cfif></td>
		</tr>
	
</table>
<cfif regionCode EQ "oce">
	<table width="100%" height="307" border="0" cellpadding="0" cellspacing="0" class="#regionCode#_map_bkgrnd">
			<tr>
		
			<td valign="top">
		<table width="100%" height="100%" border="0" cellpadding="0" cellspacing="0">
			<tr>
				<td height="12" align="left" valign="middle" class="smalltext_nav_country" style="padding-left:7px;"><cfoutput>page last updated on #DateFormat(UPDATEDDATE, "long")#</cfoutput></td>
			</tr>
			<tr>
					<td align="center" valign="middle">
						<div style="height: 300px; width: 650px;" class="mapDialog_#countryCode#">
							<a href="javascript:void(0);" title="CLICK MAP TO ENLARGE">
							<img src="../graphics/maps/#LCase(CountryCode)#-map.gif" border="0" style="cursor:pointer; border: 1px solid ##CCC; background-color: white;" class="mapFit"></a>
						</div>
						<div class="mapDialog" style="display: none;" title="The World Factbook :: Oceans : #CountryName#">
							<table width="100%" height="100%" border="0" cellpadding="0" cellspacing="0">
								<cfif #countryCode# EQ "xx">
									<tr>
										<td height="25" width="800" class="#regionCode#_dark" align="left" style="padding-left:3px;"><div class="region_name1">&nbsp;#CountryName#</div></td>
										<cfelse>
										<tr class="#regionCode#_dark">
										<td width="800" <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle"><div class="region1">#region# <strong>:: </strong><span class="region_name1">#CountryName#</span></div>
										<cfif #GetAffiliation.TEXT# NEQ "">
											<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
										</cfif></td>
								</cfif>
							<td align="right" valign="middle" class="#regionCode#_dark" >
							<cfif isDefined("URL.print")>
							<a href="##" onclick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a>
								</td>
							<td class="#regionCode#_dark" width="40" align="right"><a href="##" onclick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
								<cfelse>
								<a href="##" onclick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a>
								</td>
							
								<td class="#regionCode#_dark" width="40" align="right"><a href="##" onclick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
							</tr>
						</cfif>
						<tr>
							<td height="3" colspan="3"></td>
						</tr>
						<tr style="background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg)">
							<td align="center" valign="middle" colspan="3" <cfif #GetAffiliation.TEXT# EQ "">style="padding: 13px 0px;"<cfelse>style="padding: 8px 0px;"</cfif> ><table border="0" cellpadding="0" cellspacing="0">
									<tr>
										<td id="#lcase(regionCode)#_lgmapborder"><img src="../graphics/maps/#lcase(CountryCode)#-map.gif"></td>
									</tr>
									<tr>
										<td></td>
									</tr>
								</table></td>
						</tr>
						<tr class="#regionCode#_dark">
							<td  height="2" colspan="3"></td>
						</tr>
						<tr>
							<td align="right" valign="bottom" bgcolor="##FFFFFF" height="2" colspan="3"></td>
						</tr>
					</table>
				</div>
			
			<cfset currPath = GetDirectoryFromPath(GetCurrentTemplatePath())>
			<cfset i = getCountryImageCount(countryCode)>
			<cfif #i# LESS THAN 1>
				<tr>
					<td width="49%" height="15" align="left" valign="top" class="smalltext_nav" style="#textcolor#; letter-spacing: 1px; text-align: center; padding-bottom: 3px;"><cfif NOT isDefined("URL.print")>
							click map to enlarge <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/></span>
						</cfif></td>
				</tr>
				<cfelse>
				<tr>
					<td height="15" align="right" valign="top" class="smalltext_nav" style="#textcolor#; letter-spacing: 1px; text-align: center; padding-bottom: 3px;"><cfif isDefined("staticOut")>
							<div align="center"> view <span style="#textcolor#; letter-spacing:1px;"><a href="../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#" onClick="MM_openBrWindow('../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false") target="_blank" style="#textcolor#; letter-spacing:1px;"><strong>#i#
								<cfif #i# GREATER THAN 1>
									photos
									<cfelse>
									photo
								</cfif>
								</strong></a> of the #GetCountryName.CODEDESC#</span> <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/></span>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;
								<cfif NOT isDefined("URL.print")>
									click map to enlarge <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/></span>
								</cfif>
							</div>
							<cfelse>
							<div align="center"> view <span style="#textcolor#; letter-spacing:1px;"><a href="../photoUploadResize/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&regionCode=#regionCode#" onClick="MM_openBrWindow('../photoUploadResize/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&amp;regionCode=#regionCode#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false") target="_blank" style="#textcolor#; letter-spacing:1px;"><strong>#i#
								<cfif #i# GREATER THAN 1>
									photos
									<cfelse>
									photo
								</cfif>
								</strong></a> of the #GetCountryName.CODEDESC#</span> <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/></span>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;
								<cfif NOT isDefined("URL.print")>
									click map to enlarge <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/></span>
								</cfif>
							</div>
						</cfif></td>
				</tr>
			</cfif>
		</table>
			</td>
		
			</tr>
		
	</table>
	<cfelse>
	<table width="100%" border="0" cellspacing="0" cellpadding="0" >
		<tr>
			<td width="316" height="182" valign="bottom" background="../graphics/#Lcase(regionCode)#_flag_loc_bkgrnd.jpg" style="background-repeat:no-repeat; background-position:top left" border="0"><table width="100%" height="180" border="0" align="center" cellpadding="0" cellspacing="0">
					<tr>
						<td height="10" colspan="2" valign="top" class="smalltext_nav_country" style="padding-left:7px;"><cfoutput>page last updated on #DateFormat(UPDATEDDATE, "long")#</cfoutput></td>
					</tr>
					<tr>
						<cfif ListFind(NoFlagsList, Lcase(CountryCode)) >
							<!--- No processing --->
							<cfelse>
							<td width="50%" align="center" valign="middle" >
								<table width="100%" border="0" cellspacing="3" cellpadding="0" align="center" valign="top" >
									<tr>
										<td height="95" align="center" valign="middle" border="0" id="flagBox" style="display: none;">
											<a href="javascript:void(0);" class="flagDialog_#countryCode#" >
												<div style="height: 200px; width: 200px;" ><img src="../graphics/flags/large/#lcase(CountryCode)#-lgflag.gif" border="0" style="cursor:pointer; border: 1px solid ##CCC;" id="flagFit_countryPage"></div>
											</a>
											<div class="flagDialog" style="display: none; background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg)" title="The World Factbook" >
												<table width="100%" border="0" cellspacing="0" cellpadding="0" >
												 <tr class="#regionCode#_dark">
     									<td width="100%" <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle"><div class="region1">#region# <strong>:: </strong><span class="region_name1">#CountryName#</span></div>
										   <cfif #GetAffiliation.TEXT# NEQ "">
											<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
										   </cfif>
										   </td>     
										   <td align="right" valign="middle" class="#regionCode#_dark" >
										   		<a href="##" onClick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a></td><td class="#regionCode#_dark" width="40" align="right"><a href="##" onClick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
										    </tr>
													<tr>
														<td  <cfif GetAffiliation.TEXT is "">height="429"<cfelse>height="425"</cfif> align="center" valign="middle" style="padding: 1px 0px;"><img src="../graphics/flags/large/#lcase(CountryCode)#-lgflag.gif" alt="#CountryName#" name="flagborder" id="flagborder" title="#CountryName#" class="#regionCode#_lgflagborder"/></td>
														<td width="265" valign="middle" id="flag_caption"><table width="238" border="0" cellpadding="0" cellspacing="0" style="margin: 1px;">
																<tr>
																	<td height="20" class="#regionCode#_medium" style="font-size:10px; padding-left:5px; font-weight:bold; border:1px solid ##FFFFFF;">Flag Description</td>
																</tr>
																<tr>
																	<td valign="top">
																		<div class="photogallery_captiontext" style="height: 375px; background-color: white;">
																			<span class="category_data">
																				<cfloop query="GetFlagDesc">
																					<cfif Trim(LCase(GetFlagDesc.SUBFIELD)) IS  "note">
																						<div id ="return">&nbsp;</div>
																						<strong>#(GetFlagDesc.SUBFIELD)#</strong>:
																					</cfif>
																					#(GetFlagDesc.TEXT)#<br />
																				</cfloop>
																			</span>
																		</div>
																	</td>
																</tr>
															</table>
														</td>
													</tr>
												</table>
											</div>
										</td>
									</tr>
								</table></td>
						</cfif>
						
							<td width="50%" align="center" valign="top" id="locatorBox" style="display: none;">
							<a href="javascript:void(0);" class="locatorDialog_#countryCode#" >
								<div style="height: 200px; width: 200px;" ><img src="../graphics/locator/#regionCode#/#lcase(CountryCode)#_large_locator.gif" border="0" style="cursor:pointer; border: 1px solid ##CCC; background-color: white;" id="locator_countryPage"></div>
							</a>
							<div class="locatorDialog" style="display: none; background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg)" title="The World Factbook" >
								<table width="100%" height="100%" border="0" cellpadding="0" cellspacing="0">
								    <tr class="#regionCode#_dark">
     									<td width="800" <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle"><div class="region1">#region# <strong>:: </strong><span class="region_name1">#CountryName#</span></div>
										   <cfif #GetAffiliation.TEXT# NEQ "">
											<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
										   </cfif>
										   </td>     
										   <td align="right" valign="middle" class="#regionCode#_dark" >
											  <cfif isDefined("URL.print")>
											   <a href="##" onClick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a></td><td class="#regionCode#_dark" width="40" align="right"><a href="##" onClick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
												<cfelse>
											    <a href="##" onClick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a></td><td class="#regionCode#_dark" width="40" align="right"><a href="##" onClick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
											  </cfif>
									    </tr>
									    <tr>
										 <td colspan="3" height="3"></td>
									    </tr>
									    <tr <cfif #GetAffiliation.TEXT# EQ "">height="542"<cfelse>height="538"</cfif> >
										 <td colspan="5" align="center" valign="middle" style="background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg);padding: 2px 0px;">
										 <table border="0" cellpadding="5" cellspacing="5" >
											<tr>
											  <cfif regionCode EQ "refmaps">
											    <td class="#regionCode#_lglocatorborder"><img src="../graphics/ref_maps/jpg/#refmap#"/></td>
											    <cfelse>
											    <td class="#regionCode#_lglocatorborder"><img src="../graphics/locator/#regionCode#/#lcase(CountryCode)#_large_locator.gif"/></td>
											  </cfif>
											</tr>
											<tr>
											  <td colspan="5"></td>
											</tr>
										   </table></td>
									    </tr>
									    <tr>
										 <td colspan="5"><table width="100%" height="100%" border="0" align="center" cellpadding="0" cellspacing="0" >
											<tr class="#regionCode#_dark">
											  <td colspan="5" height="2"></td>
											</tr>
											<tr>
											  <td colspan="5" align="right" valign="bottom" bgcolor="##FFFFFF" height="2"></td>
											</tr>
										   </table></td>
									    </tr>
									  </table>
							  </div>
						</td>
					</tr>
					<tr>
						<td height="15" colspan="2" align="center" valign="middle" class="smalltext_nav" style="#textcolor#; letter-spacing:1px; line-height: 10px;">Click flag or map to enlarge <span class="smalltext_nav1"><img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" style="padding-bottom: 1px;" border="0"/></span></td>
					</tr>
				</table></td>
			<td width="8" height="275" rowspan="3">&nbsp;</td>
			<td width="314" height="275" rowspan="3" valign="middle" style="background-image: ('../graphics/#LCase(regionCode)#_map_bkgrnd.jpg')">
				<table width="95%" height="302" border="0" align="center" cellpadding="0" cellspacing="0" >
					<tr>
						<td height="290" align="center" valign="top" id="mapBox" style="display: none;">
							<div style="height: 300px; width: 300px;" class="mapDialog_#countryCode#">
								<a href="javascript:void(0);" title="CLICK MAP TO ENLARGE">
								<img src="../graphics/maps/#LCase(CountryCode)#-map.gif" border="0" style="cursor:pointer; border: 1px solid ##CCC; background-color: white;" class="mapFit"></a>
							</div>
							<div class="mapDialog" title="The World Factbook" style="background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg);display: none;">
							<table width="100%" height="100%" border="0" cellpadding="0" cellspacing="0">
								<cfif #countryCode# EQ "xx">
									<tr>
										<td height="25" width="800" class="#regionCode#_dark" align="left" style="padding-left:3px;"><div class="region_name1">&nbsp;#CountryName#</div></td>
										<cfelse>
										<tr class="#regionCode#_dark">
										<td width="800" <cfif #GetAffiliation.TEXT# EQ "">height="25"<cfelse>height="30"</cfif> valign="middle"><div class="region1">#region# <strong>:: </strong><span class="region_name1">#CountryName#</span></div>
										<cfif #GetAffiliation.TEXT# NEQ "">
											<div class="affiliation"><em>#GetAffiliation.TEXT#</em></div>
										</cfif></td>
								</cfif>
							<td align="right" valign="middle" class="#regionCode#_dark" >
							<cfif isDefined("URL.print")>
							<a href="##" onclick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a>
								</td>
							<td class="#regionCode#_dark" width="40" align="right"><a href="##" onclick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
								<cfelse>
								<a href="##" onclick="window.print();return;false"><img src="../graphics/print.gif" alt="Print Page" title="Print Page" width="25" height="18" border="0" style="color:##FFFFFF;text-decoration: none;" align="right"></a>
								</td>
							
								<td class="#regionCode#_dark" width="40" align="right"><a href="##" onclick="window.print();return;false"><span class="smalltext_nav" style="color:##FFFFFF;"><strong> PRINT</strong></span></a></td>
							</tr>
						</cfif>
						<tr>
							<td height="3" colspan="3"></td>
						</tr>
						<tr style="background:url(../graphics/#lcase(regionCode)#_lgmap_bkgrnd.jpg)">
							<td align="center" valign="middle" colspan="3" <cfif #GetAffiliation.TEXT# EQ "">style="padding: 13px 0px;"<cfelse>style="padding: 8px 0px;"</cfif> ><table border="0" cellpadding="0" cellspacing="0">
									<tr>
										<td id="#lcase(regionCode)#_lgmapborder"><img src="../graphics/maps/#lcase(CountryCode)#-map.gif"></td>
									</tr>
									<tr>
										<td></td>
									</tr>
								</table></td>
						</tr>
						<tr class="#regionCode#_dark">
							<td  height="2" colspan="3"></td>
						</tr>
						<tr>
							<td align="right" valign="bottom" bgcolor="##FFFFFF" height="2" colspan="3"></td>
						</tr>
					</table>
				</div>
						</td>
					</tr>
					<tr>
						<td height="15" colspan="2" align="center" valign="bottom"  class="smalltext_nav" style="#textcolor#; letter-spacing:1px; line-height: 10px; "><cfif NOT isDefined("URL.print")>
								Click map to enlarge <span class="smalltext_nav1"> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" style="padding-bottom: 1px;" border="0"/> </span>
							</cfif></td>
					</tr>
				</table></td>
		</tr>
		<tr>
			<td width="314" align="left" valign="top" <cfif isDefined("URL.print")><cfelse><cfif isDefined("URL.staticOut")>class="photo_bkgrnd_static"<cfelse>class="photo_bkgrnd"</cfif></cfif>><cfset currPath = GetDirectoryFromPath(GetCurrentTemplatePath())>
				<cfset i = getCountryImageCount(countryCode)>
				<cfif NOT isDefined("URL.StaticOut")>
					<table width="314" height="123" border="0" cellpadding="0" cellspacing="0">
						<tr>
							<td align="center" valign="middle"><table width="100%" border="0" align="left" cellpadding="0" cellspacing="0">
									<tr>
										<td height="10" colspan="3"></td>
									</tr>
									<tr>
										<td width="54%" rowspan="3" align="center" valign="middle" class="smalltext_nav"><cfif NOT isDefined("URL.print")>
												<cfif #i# GREATER THAN 0>
													<cfif isDefined("staticOut")>
														<a href="../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#lcase(CountryCode)#_1#globalStaticOutputExtension#" onClick="MM_openBrWindow('../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#lcase(CountryCode)#_1#globalStaticOutputExtension#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false")> <img src="../graphics/photo_on.jpg" alt="Photos of <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" name="Photos" width="123" height="81"  border="0" id="Photos"  title="Photos of #GetCountryName.CODEDESC#"/> </a>
														<cfelse>
														<a href="../photoUploadResize/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&regionCode=#regionCode#" onClick="MM_openBrWindow('../photoUploadResize/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&amp;regionCode=#regionCode#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false") target="_blank" > <img src="../graphics/photo_on.jpg" alt="Photos of <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" name="Photos" width="123" height="81" border="0" id="Photos" title="Photos of #GetCountryName.CODEDESC#"/>
														<div> view <span style="#textcolor#; letter-spacing:1px;"><strong>#i#
															<cfif #i# GREATER THAN 1>
																photos
																<cfelse>
																photo
															</cfif>
															</strong></span> <span style="#textcolor#; letter-spacing:1px;"></span> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/> </div>
														</a>
													</cfif>
													<cfelse>
													<img src="../graphics/photo_off.jpg" alt="No Photos available for <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" name="Photos" width="123" height="81" border="0" id="Photos" title="No Photos Avaliable for #GetCountryName.CODEDESC#"/>
													<div> <span class="photo_text1">&nbsp;&nbsp;no photos available</span></div>
												</cfif>
											</cfif></td>
									</tr>
									<tr>
										<td align="center" valign="top"><cfif NOT isDefined("URL.print")>
												<div> <span class="photo_text" style="padding-bottom: 8px; color: ##A7976D;"> <a href="../docs/#prefix#submitphoto.cfm" onMouseOut="MM_swapImgRestore()" onMouseOver="MM_swapImage('Submit Photos','','../graphics/camera_on.gif',1)"> <img src="../graphics/camera_off.gif" alt="Submit Photos of #GetCountryName.CODEDESC#" name="Submit Photos" width="63" height="50" vspace="3" border="0" id="Submit Photos" title="Submit Photos of #GetCountryName.CODEDESC#" /> </a><br />
													DO YOU HAVE<br />
													PHOTOS TO SUBMIT? </span> </div>
												<div style="margin-top: 0px;"> <img src="../graphics/photo_arrow.gif" alt="Photo Arrow" width="10" height="7" style="vertical-align:bottom; padding-bottom:2px;" border="0"> <a href="../docs/#prefix#submitphoto.cfm" class="photo_text1" style="color:##999999; padding-right:10px;"><strong>CLICK HERE</strong></a> </div>
											</cfif></td>
									</tr>
									<tr>
										<td height="50%" align="center" valign="top" ></td>
									</tr>
								</table></td>
						</tr>
					</table>
					<cfelse>
					<cfif  isDefined("URL.print")>
						<table cellpadding="0" cellspacing="0" width="290" height="123" border="0">
							<tr>
								<td></td>
							</tr>
						</table>
						<cfelse>
						<table cellpadding="0" cellspacing="0" width="290" height="123" border="0">
							<tr>
								<td align="center" valign="middle"><table width="290" border="0" align="left" cellpadding="0" cellspacing="0">
										<tr>
											<td width="150" rowspan="4" align="center" valign="bottom"><cfif #i# GREATER THAN 0>
													<cfif isDefined("staticOut")>
														<a href="../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#" onClick="MM_openBrWindow('../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false")> <img src="../graphics/photo_on.gif" alt="Photos of <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" name="Photos" width="123" height="89"  border="0" id="Photos" title="Photos of #GetCountryName.CODEDESC#" /> </a>
														<cfelse>
														<a href="../photo_gallery/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&regionCode=#regionCode#" onClick="MM_openBrWindow('../photoUploadResize/photo_gallery_A1.cfm?CountryCode=#Lcase(CountryCode)#&amp;regionCode=#regionCode#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false") target="_blank" > <img src="../graphics/photo_on.gif" alt="Photos of <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" name="Photos" width="123" height="89" border="0" id="Photos2" style="padding-top:5px;" title="Photos of #GetCountryName.CODEDESC#"/> </a>
													</cfif>
													<cfelse>
													<img src="../graphics/photo_off.gif" alt="No Photos available for <cfoutput>#GetCountryName.CODEDESC#</cfoutput>" width="123" height="89" style="padding:5px;" title="No Photos Avaliable for #GetCountryName.CODEDESC#" border="0">
												</cfif></td>
										</tr>
										<cfif #i# GREATER THAN 0>
											<tr>
												<td colspan="2" align="left" valign="top"><table border="0" cellpadding="0" cellspacing="0">
														<tr>
															<td height="20" class="photo_text1" >&nbsp;</td>
														</tr>
														<tr>
															<td valign="top" class="photo_text1"><table border="0" cellpadding="0" cellspacing="0" class="photo_text">
																	<tr>
																		<td><img src="../graphics/left_arrow.gif" alt="Navigation Arrow" width="11" height="9" border="0" style="padding-top: 2px;"/></td>
																		<td height="12" valign="top" style="margin:0px; padding:0px;">view&nbsp;<a href="../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#" onClick="MM_openBrWindow('../photo_gallery/#Lcase(CountryCode)#/photo_gallery_A1_#LCase(CountryCode)#_1#globalStaticOutputExtension#','','status=no,scrollbars=no,resizable=no,width=850,height=580'); return false")><span style="#textcolor#; letter-spacing:1px; text-decoration:none;"><strong>#i#
																			<cfif #i# GREATER THAN 1>
																				photos
																				<cfelse>
																				photo
																			</cfif>
																			</strong></span> <img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" width="14" height="12" style="padding-bottom: 1px;" title="Opens in New Window" border="0"/></a></td>
																	</tr>
																	<tr>
																		<td colspan="2" style="padding-left:13px;">of&nbsp;#GetCountryName.CODEDESC#</td>
																	</tr>
																</table></td>
														</tr>
													</table></td>
											</tr>
											<cfelse>
											<tr>
												<td colspan="2" align="left" valign="top"><table cellpadding="0" cellspacing="0">
														<tr>
															<td height="27" class="photo_text1" >&nbsp;</td>
														</tr>
														<tr>
															<td class="photo_text1" >no photos available of #GetCountryName.CODEDESC#</td>
														</tr>
													</table></td>
											</tr>
										</cfif>
									</table></td>
							</tr>
						</table>
					</cfif>
				</cfif></td>
		</tr>
	</table>
</cfif>
</div>
<!---<table width="100%" border="0" cellpadding="2" cellspacing="2">
  <tr>
    <cfif  NOT isDefined("URL.print")>
      <td height="10" align="right" valign="middle" class="smalltext_nav"><a style="cursor:pointer;" onClick="expandAllSections( ); return false;" alt="Expand All" title="Expand All">Expand All</a> | <a style="cursor:pointer;" onClick="collapseAllSections( ); return false;" alt="Collapse All" title="Collapse All">collapse All </a></td>
    </cfif>
  </tr>
</table>---> 
</cfoutput> 

<!--- ****************************************************************************************************************************************************************** --->
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
<cfquery name="newGetCountrySection" datasource="#application.dsn#">
	SELECT	f.catcode, COUNT(f.fielddesc) AS sectionCount 
	FROM		factbook.fields f, factbook.summary s 
	WHERE	f.pubyear = #pubyear#  AND 
			s.pubyear = f.pubyear AND 
			s.geocode = '#Ucase(CountryCode)#' AND 
			f.active = 'Y' AND 
			f.fieldkey = s.fieldkey 
	GROUP BY 	f.catcode
	ORDER BY	sectionCount
</cfquery>
<cfoutput>
	<cfquery name="GetCountrySection" datasource="#application.dsn#">
		SELECT	f.fieldseq, f.catcode, s.geocode, s.factseq, f.fielddesc, s.fieldkey, s.subfield, s.text 
		FROM		factbook.fields f, factbook.summary s
		WHERE 	f.pubyear = #pubyear# AND 
				s.pubyear = f.pubyear AND 
            		s.geocode = '#uCase(CountryCode)#' AND 
            		f.active = 'Y' AND 
            		f.fieldkey = s.fieldkey 
		ORDER BY	f.fieldseq, s.factseq 
	</cfquery>
	
	<!--- Query moved from countrytemplateoutput.cfm.  It's an independent query.  Therefore it only needs to be run once.  --->
	<cfquery name="GetNotesAndDefs" datasource="#application.dsn#" cachedwithin="#CreateTimeSpan(1,0,0,0)#">
		SELECT 		TERM 
		FROM 		FACTBOOK.NOTES_DEFS 
		ORDER BY 		TERM
	</cfquery>
	
	<!--- This function is actually used in countrytemplateoutput.cfm.  But, it's included here to avoid multiple definitions --->
	<cfinclude template="../functions/funcs_isCategoryCodeAvailable.cfm">
	<table width="100%" border="0" cellpadding="0" cellspacing="0">
			<tr>
		
			<td>
		
		<!---<cfloop from="1" to="#ArrayLen(categoryStructArray)#" index="ii">
			<cfset CatCode_FromCountryTemplate = categoryStructArray[ii].CatCode>
			<cfset SectionName = categoryStructArray[ii].SectionName>
			<cfinclude template="countryTemplateOutput_test.cfm">
		</cfloop>--->
			</td>
		
			</tr>
		
	</table>
</cfoutput>
<table width="100%" border="0" cellpadding="2" cellspacing="0">
	<tr>
		<cfif  NOT isDefined("URL.print")>
			<td height="10" align="right" valign="bottom" class="smalltext_nav"><a style="cursor:pointer;" onClick="expandAllSections( ); return false;" alt="Expand All" title="Expand All">Expand All</a> | <a style="cursor:pointer;" onClick="collapseAllSections( ); return  false;" alt="Collapse All" title="Collapse All">collapse All</a></td>
		</cfif>
	</tr>
</table>
<cfif (NOT isDefined("cookie.LASTCRNTYCODE")) OR (NOT (URL.countryCode IS cookie.LASTCRNTYCODE))>
	<script language="javascript" type="text/javascript">
	var cookieExpdate = new Date();
	cookieExpdate.setDate(cookieExpdate.getDate() + 7);
		
	// RAN: Session cookie-only change
<!---	document.cookie = "LASTCRNTYCODE=" + escape("<cfoutput>#URL.countryCode#</cfoutput>") + ";expires=" + cookieExpdate.toGMTString() + ";path=" + "/";--->
	document.cookie = "LASTCRNTYCODE=" + escape("<cfoutput>#URL.countryCode#</cfoutput>") + ";path=" + "/" + ";secure";
</script>
</cfif>
