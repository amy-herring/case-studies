<!---
<cfquery name="GetCountrySection" datasource="wfb">
	SELECT 	fieldseq, factseq, fielddesc, factbook.summary.fieldkey, subfield, text 
	FROM 		factbook.fields, factbook.summary 
	WHERE 		factbook.fields.pubyear = #PubYear# 
	AND 		factbook.summary.pubyear = #PubYear# 
	AND			catcode = '#CatCode#' 
	AND			geocode = '#CountryCode#' 
	AND			active = 'Y' 
	AND			factbook.fields.fieldkey = factbook.summary.fieldkey 
	ORDER BY	fieldseq, factseq 
</cfquery>

<link rel="stylesheet" type="text/css" href="../styles/print.css" media="print"> --->
<!---<cfif GetCountrySection.RecordCount EQ 0>--->
<script type="text/javascript" src="../scripts/jquery.popupWindow.js"></script>
<cfif NOT isCategoryCodeAvailable(CatCode_FromCountryTemplate, GetCountrySection)>
	<cfelse>
	<!--- Load the results of the query into an array for parsing --->
	<cfset SectionArray = ArrayNew(2)>
	<cfloop query="GetCountrySection">
		<cfif GetCountrySection.catcode IS CatCode_FromCountryTemplate>
			<cfset SectionArray[ArrayLen(SectionArray)+1][1]= GetCountrySection.FIELDDESC>
			<cfset SectionArray[ArrayLen(SectionArray)][2]= GetCountrySection.SUBFIELD>
			<cfset SectionArray[ArrayLen(SectionArray)][3]= GetCountrySection.TEXT>
			<cfset SectionArray[ArrayLen(SectionArray)][4]= GetCountrySection.FIELDKEY>
		</cfif>
	</cfloop>
	<!--- Query moved to countrytemplate(Combined).cfm 
	<cfquery name="GetNotesAndDefs" datasource="wfb">
	SELECT 		TERM 
	FROM 		FACTBOOK.NOTES_DEFS 
	ORDER BY 	TERM
	</cfquery>
	--->
	<div id="CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput>" class="CollapsiblePanel" style="width:100%; ">
		<cfif NOT isDefined("URL.print")>
			<!---<div id="CollapsiblePanelTab" tabindex="0" style="vertical-align:middle;">--->
			<div class="wrapper">
			<h2 class="question" style="background-color: yellow;"><a href="javascript:void(0);"><cfoutput>#SectionName#</cfoutput></span> ::</span><span class="region"><cfoutput>#CountryName#</cfoutput></span></a></h2>
		<div class="answer" align="left">
			<div class="box" style="padding: 0px; margin: 0px;">
				<ul style="text-align: left;padding: 0px;margin: 0px;width: 100%;">
			
			
			<!---<table border="0" cellspacing="0" cellpadding="0" width="638" height="23" style="background-image: url(../graphics/<cfoutput>#regionCode#</cfoutput>_medium.jpg);">
				<tr>
					<td class="CollapsiblePanelTab" style="vertical-align:middle;padding-left:8px;" alt="Expand/Collapse <cfoutput>#SectionName#</cfoutput>" title="Expand/Collapse <cfoutput>#SectionName#</cfoutput>"><a name="#SectionName#"></a><span class="category"><cfoutput>#SectionName#</cfoutput></a> ::<span class="region" style="font-weight: normal;"><cfoutput>#CountryName#</cfoutput></span></span>
					</td>
				</tr>
			</table>--->
			<cfelse>
			<h2 class="question"><a href="javascript:void(0);"><cfoutput>#SectionName#</cfoutput></span> ::</span><span class="region"><cfoutput>#CountryName#</cfoutput></span></a></h2>
		<div class="answer" align="left">
			<div class="box" style="padding: 0px; margin: 0px;">
				<ul style="text-align: left;padding: 0px;margin: 0px;width: 100%;">
					
			
			<!---<table border="0" cellspacing="0" cellpadding="0" width="638" height="23" style="background-image: url(../graphics/<cfoutput>#regionCode#</cfoutput>_medium.jpg)">
				<tr>
					<td><span class="category" style="vertical-align:middle;padding-left:8px;"  alt="Expand/Collapse <cfoutput>#SectionName#</cfoutput>" title="Expand/Collapse <cfoutput>#SectionName#</cfoutput>"> <span class="category" style="vertical-align:middle;padding-left:8px;"><cfoutput>#SectionName#</cfoutput></span> ::</span><span class="region"><cfoutput>#CountryName#</cfoutput></span></td>
				</tr>
			</table>--->
		</cfif>
	<!--- beginning of panels ----------------------------------------------------------------------->

					<table border="0" cellspacing="0" cellpadding="0"  style="width: 100%; margin-left: 0px; border: 1px solid red;">
						<tr>
							<td style="padding: 0; margin: 0px;" align="left">
								<table width="100%" border="0" align="left" cellpadding="0" cellspacing="0">
								<!--- Output the first element of the array ---> 
								<cfoutput>
									<cfloop query="GetNotesAndDefs">
										<cfif #GetNotesAndDefs.TERM# EQ #SectionArray[1][1]#>
											<cfset newRow = currentRow>
											<cfset newRow1 = newRow - 1>
											<cfif newRow1 LTE 0>
												<cfset newRow1 = "top">
											</cfif>
										</cfif>
									</cfloop>
									<tr class="#regionCode#_light" >	
									<td width="450" height="20">
										<div class="category" style="padding-left:5px;" id="field"> <cfoutput>
												<cfif isDefined("staticOut")>
													<cfif #SectionArray[1][4]# EQ 2203>
														#SectionArray[1][1]#:
														<cfelse>
														<a href="../docs/notesanddefs#globalStaticOutputExtension####SectionArray[1][4]#" alt="Definitions and Notes: #SectionArray[1][1]#" title="Definitions and Notes: #SectionArray[1][1]#"> #SectionArray[1][1]#</a>:
													</cfif>
													<cfelse>
													<cfif #SectionArray[1][4]# EQ 2203>
														#SectionArray[1][1]#:
														<cfelse>
														<a href="../docs/#prefix#notesanddefs.cfm?fieldkey=#SectionArray[1][4]#&countryName=#CountryName#&countryCode=#countryCode#&term=#SectionArray[1][1]#&regionCode=#regionCode####SectionArray[1][4]#"  alt="Definitions and Notes: #SectionArray[1][1]#" title="Definitions and Notes: #SectionArray[1][1]#"> #SectionArray[1][1]#</a>:
													</cfif>
												</cfif>
											</cfoutput> </div>
									</td>
									<cfif NOT isDefined("URL.print")>					
									<td align="right">	
										<!---Create the link to the definition for this Field - except for fields in the NoLinksList --->
										<cfif ListFind(NoLinksList, "#SectionArray[1][4]#") is 0>
											<cfset anchorRow = GetFieldListing(SectionArray[1][4],countrycode,pubyear)>
											<cfif isDefined("staticOut")>
												<a onClick="hideTip()" href="../fields/#SectionArray[1][4]##globalStaticOutputExtension#<cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode#</cfif>" onMouseOver="doTooltip(event,0)" onMouseOut="hideTip()"><img src="../graphics/field_listing_on.gif" border="0" alt="Field info displayed for all countries in alpha order." style="text-decoration:none;"></a>
												<cfelse>
												<cfif ListFind(RankOrderList, SectionArray[1][4]) GT 0>
													<cfif #countryCode# EQ "XX">
														<cfelseif #countryCode# NEQ "XX">
														<cfset rankAnchorRow = GetRankOrderNumber(SectionArray[1][4],countrycode,pubyear)>
													</cfif>
												</cfif>
												<a onClick="hideTip()" href="../fields/#prefix#fieldstemplate.cfm?FieldKey=#SectionArray[1][4]#&countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#&<cfif ListFind(RankOrderList, #SectionArray[1][4]#)><cfif countryCode NEQ "XX">rankAnchorRow=#rankAnchorRow#</cfif></cfif><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode##anchorrow#</cfif>" onMouseOver="doTooltip(event,0)" onMouseOut="hideTip()"><img src="../graphics/field_listing_on.gif" border="0" alt="Field info displayed for all countries in alpha order." style="text-decoration:none;"></a> 
											</cfif>
										</cfif>
										<!---Create a link to the Google search page on Intelink --->
										<cfset SearchTerm = CountryName & " " & SectionArray[1][1]>
										<!---Replace the dashes in the search string with nulls --->
										<cfset SearchTerm = Replace(SearchTerm,"- ","","All")>
										<!---  Replace the blanks in the search string with + signs --->
										<cfset SearchTerm = Replace(SearchTerm," ","+","All")>
										<cfif isDefined("URL.staticOut")>
											<cfif isDefined("URL.forInternalDynamic")>
												<a href="http://home.ismc.ic.gov/search/?q=#SearchTerm#" onMouseOver="doTooltip(event,2)" onMouseOut="hideTip()"> <img src="../graphics/intelink_search.gif"  alt="Intel Link Search" style="text-decoration: none;"> </a>
											</cfif>
											<cfelse>
											<cfset SearchTerm = Replace(SearchTerm,"(","","All")>
											<cfset SearchTerm = Replace(SearchTerm,")","","All")>
											<a href="../#prefix#search_results.cfm?q=#URLEncodedFormat(SearchTerm)#&site=WORLDFACTBOOK&client=WORLDFACTBOOK&proxystylesheet=WORLD_FACTBOOK&output=xml_no_dtd&restrict=WORLDFACTBOOK"					onMouseOver="doTooltip(event,1)" onMouseOut="hideTip()"><img src="../graphics/goog.gif"  alt="Google Search" style="text-decoration: none;"></a>
								</td>
							</cfif>
						</tr>
					</cfif>
						<tr>
							<td id="data" colspan="2" style="vertical-align:middle;">
				
								<!--- Display the Subfield (if it is not blank) and the Text for this field --->
								<cfif #SectionArray[1][2]# is not "">
									<div class="category">#SectionArray[1][2]#: <span class="category_data" style="font-weight:normal;">#SectionArray[1][3]#</span> </div>
									<cfelse>
									<div class="category_data">#SectionArray[1][3]#</div>
								</cfif>
								<!---<span style="background-color:##000000; color:##FFFFFF; font-size:36px">SectionArray[1][4]: #SectionArray[1][4]# (#RankOrderList#)</span>--->
								<cfif ListFind(RankOrderList, #SectionArray[1][4]#)>
									<cfif #countryCode# EQ "XX">
										<cfelseif #countryCode# NEQ "XX">
										<cfset anchorRow = GetRankOrderNumber(SectionArray[1][4],countrycode,pubyear)>
										<cfif #anchorRow# EQ -999>
											<!---<span class="category">no information is available to be ranked</span>--->
											<cfelse>
											<cfif isDefined("URL.staticOut")>
												<span class="category" style="padding-left:7px;">country comparison to the world:</span> <span class="category_data"> <a href="../rankorder/#SectionArray[1][4]#rank#globalStaticOutputExtension#?countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#&rank=#anchorRow#<!---<cfif anchorRow EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=###countryCode##anchorRow#</cfif>---><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode#</cfif>" onMouseDown="" title="Country comparison to the world" alt="Country comparison to the world">#anchorRow#</a> </span>
												<cfelse>
												<span class="category" style="padding-left:7px;">country comparison to the world:</span><span class="category_data"> <a href="../rankorder/#prefix#rankorderguidetemplate.cfm?FieldKey=#SectionArray[1][4]#&countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=#anchorRow####countryCode##anchorrow#</cfif>" onMouseDown=""  title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
											</cfif>
										</cfif>
									</cfif>
								</cfif>
							</cfoutput> 
			<!--- Output the rest of the elements of the array --->
					<cfloop index="Counter" from=2 to="#ArrayLen(SectionArray)#">
				<cfoutput>
					<cfif #SectionArray[Counter][1]# eq #SectionArray[Counter - 1][1]#>
						<!--- <br /><i>#SectionArray[Counter][2]#:</i> #SectionArray[Counter][3]# Dennis changed this 5 September 2008 0840--->
						<cfif #SectionArray[Counter][2]# is not "">
							<div class="category" style="padding-top: 2px;">
								<cfif FindNoCase("note",#SectionArray[Counter][2]#,1)>
									<em>#SectionArray[Counter][2]#:</em>
									<cfelse>
									#SectionArray[Counter][2]#:
								</cfif>
								<span class="category_data" style="font-weight:normal; vertical-align:top;">#SectionArray[Counter][3]# </span></div>
							<cfelse>
							<div class="category_data" style="padding-top: 3px;">#SectionArray[Counter][3]# </div>
						</cfif>
						<cfif #SectionArray[Counter][2]# EQ "cabinet">
							<cfif isDefined("URL.staticOut")>
								<span class="category" style="padding-left:7px;font-weight: normal;">(For more information visit the <a href="/library/publications/world-leaders-1/index.html" target="_blank">World Leaders website</a>&nbsp;<img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/>)</span>
								<cfelse>
								<span class="category" style="padding-left:7px;font-weight: normal;">(For more information visit the <a href="http://leadershipprofiles.cia.ic.gov/cos_all.htm" target="_blank">Chiefs of State website</a>&nbsp;<img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/>)</span>
							</cfif>
							<cfelse>
						</cfif>
						<!--- Otherwise close the cell and the row and start a new row and cell --->
						<cfelse>
					</td>
				</tr>
				<tr>
							<td height="10"></td>
						</tr>
						<cfloop query="GetNotesAndDefs">
							<cfif #GetNotesAndDefs.TERM# EQ #SectionArray[Counter][1]#>
								<cfset newRow = currentRow>
								<cfset newRow1 = newRow - 1>
								<cfif newRow1 LTE 0>
									<cfset newRow1 = "top">
								</cfif>
							</cfif>
						</cfloop>
						<tr class="#regionCode#_light">
							<td width="450" height="20"><!--- Category headings --->
								<div class="category" style="padding-left:5px;" id="field"> <cfoutput>
										<cfif isDefined("staticOut")>
											<a href="../docs/notesanddefs#globalStaticOutputExtension####SectionArray[counter][4]#" alt="Definitions and Notes: #SectionArray[counter][1]#" title="Definitions and Notes: #SectionArray[counter][1]#"> #SectionArray[counter][1]#</a>:
											<cfelse>
											<a href="../docs/#prefix#notesanddefs.cfm?fieldkey=#SectionArray[counter][4]#&countryName=#CountryName#&countryCode=#countryCode#&term=#SectionArray[counter][1]#&regionCode=#regionCode####SectionArray[counter][4]#"  alt="Definitions and Notes: #SectionArray[counter][1]#" title="Definitions and Notes: #SectionArray[counter][1]#">#SectionArray[counter][1]#</a>:
										</cfif>
									</cfoutput> </div>
							</td>
							<cfif NOT isDefined("URL.print")>
								<!--- Field and Google Icons --->
								<td align="right"><!---Create the link to the definition for this Field - except for fields in the NoLinksList ---> 
									<!---Create the link to the listing of all countries for this Field  - except for fields in the NoLinksList --->
									<cfif ListFind(NoLinksList, "#SectionArray[Counter][4]#") is 0>
										<cfset anchorRow = GetFieldListing(SectionArray[counter][4],countrycode,pubyear)>
										<cfif isDefined("staticOut")>
											<!---                      <a onclick="hideTip()" href="../fields/#SectionArray[counter][4]##globalStaticOutputExtension#?countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#&<cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode#</cfif>" onmouseover="doTooltip(event,0)" onmouseout="hideTip()"><img src="../graphics/field_listing_on.gif" border="0" alt="Field info displayed for all countries in alpha order."></a>---> 
											<a onClick="hideTip()" href="../fields/#SectionArray[counter][4]##globalStaticOutputExtension#<cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode#</cfif>" onMouseOver="doTooltip(event,0)" onMouseOut="hideTip()"><img src="../graphics/field_listing_on.gif" border="0" alt="Field info displayed for all countries in alpha order."></a>
											<cfelse>
											<!---<cfset anchorRow = GetFieldListing(SectionArray[counter][4],countrycode,pubyear)>--->
											<cfif ListFind(RankOrderList, SectionArray[counter][4]) GT 0>
												<cfif #countryCode# EQ "XX">
													<cfelseif #countryCode# NEQ "XX">
													<cfset rankAnchorRow = GetRankOrderNumber(SectionArray[counter][4],countrycode,pubyear)>
												</cfif>
											</cfif>
											<a onClick="hideTip()" href="../fields/#prefix#fieldstemplate.cfm?FieldKey=#SectionArray[counter][4]#&countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#&<cfif ListFind(RankOrderList, #SectionArray[counter][4]#)><cfif countryCode NEQ "XX">rankAnchorRow=#rankAnchorRow#</cfif></cfif><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode##anchorrow#</cfif>" onMouseOver="doTooltip(event,0)" onMouseOut="hideTip()"><img src="../graphics/field_listing_on.gif" border="0"  alt="Field info displayed for all countries in alpha order." style="text-decoration:none;"></a>
										</cfif>
									</cfif>
									<!---Create a link to the Google search page on Intelink --->
									<cfset SearchTerm = CountryName & " " & SectionArray[Counter][1]>
									<!---Replace the dashes in the search string with nulls --->
									<cfset SearchTerm = Replace(SearchTerm,"- ","","All")>
									<!---Replace the blanks in the search string with + signs --->
									<cfset SearchTerm = Replace(SearchTerm," ","+","All")>
									<cfif isdefined("URL.staticOut")>
										<cfif isDefined("URL.forInternalDynamic")>
											<a href="http://home.ismc.ic.gov/search/?q=#SearchTerm#" onMouseOver="doTooltip(event,2)" onMouseOut="hideTip()"> <img src="../graphics/intelink_search.gif"  alt="Intel Link Search" style="text-decoration: none;"> </a>
										</cfif>
										<cfelse>
										<cfset SearchTerm = Replace(SearchTerm,"(","","All")>
										<cfset SearchTerm = Replace(SearchTerm,")","","All")>
										<a href="../#prefix#search_results.cfm?q=#URLEncodedFormat(SearchTerm)#&site=WORLDFACTBOOK&client=WORLDFACTBOOK&proxystylesheet=WORLD_FACTBOOK&output=xml_no_dtd&restrict=WORLDFACTBOOK" onMouseOver="doTooltip(event,1)" onMouseOut="hideTip()"> <img src="../graphics/goog.gif"  alt="Google Search" style="text-decoration: none;"></a>
									</cfif>
								</td>
							</cfif>
						</tr>
						<tr height="22">
							<td colspan="2" id="data"><!--- If this is the Climate field 2059, link to the current weather --->
								<cfif isDefined("URL.staticOut")>
									<cfelse>
									<cfif SectionArray[Counter][1] EQ "Climate">
										<span class="category_data"> <a href="http://www.ismc.ic.gov/weather/locations.aspx?country=#CountryName#" target="_blank" >Current Weather</a> </span>
									</cfif>
								</cfif>
								
								<!--- Display the Subfield (if it is not blank) and the Text for this field --->
								<cfif #SectionArray[Counter][2]# is not "">
									<div class="category">#SectionArray[Counter][2]#: <span class="category_data" style="font-weight:normal; vertical-align:bottom;">#SectionArray[Counter][3]#</span></div>
									
									<!--- THIS IS WHERE THE AUDIO PORTION FOR THE NATIONAL ANTHEMS  ------------------------------------------>
									<cfif #SectionArray[counter][1]# EQ "National anthem">
										<cfquery name="getMP3" datasource="wfb">
											SELECT * 
											FROM anthems
											WHERE geocode = '#UCase(countryCode)#'
										</cfquery>
										<cfset thisPath = ExpandPath("*.*")>
										<cfset thisDirectory = GetDirectoryFromPath(thisPath)>
										<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../anthems/" & UCase(countryCode) & ".mp3">
										<cfif FileExists(thePath)>
										<link rel="stylesheet" type="text/css" href="../styles/jquery.ui.core.css"/>
										<p><a href="../anthems/#UCase(countryCode)#.mp3" class="playAnthem" name="#countryName#"><img src="../graphics/play_anthem.png"></a></p>
										<script type="text/javascript">
											$('.playAnthem').popupWindow({
												centerBrowser:1,
												height:50,
												width:300,
												location:0, // determines whether the address bar is displayed {1 (YES) or 0 (NO)}.
												menubar:0, // determines whether the menu bar is displayed {1 (YES) or 0 (NO)}.
												resizable:0, // whether the window can be resized {1 (YES) or 0 (NO)}. Can also be overloaded using resizable.
												scrollbars:0, // determines whether scrollbars appear on the window {1 (YES) or 0 (NO)}.
												status:0, // whether a status line appears at the bottom of the window {1 (YES) or 0 (NO)}.
												//windowName: "#countryName#" , // name of window set from the name attribute of the element that invokes the click
												windowURL: 0, // url used for the popup
												top:0, // top position when the window appears.
												toolbar:0
											});
										</script>
											<!---<div id="mediaplayer" style="height: 23px;"></div>
											<script type="text/javascript" src="jwplayer/jwplayer.js"></script> 
											<script type="text/javascript">
												jwplayer("mediaplayer").setup({
													'flashplayer' : "jwplayer/jwplayer.flash.swf",
													'file': "../anthems/#UCase(countryCode)#.mp3",
													'controlbar': 'none',
													'width': '370',
													'height': '23',
													plugins: {
														viral: { onpause: false,
														oncomplete: false,
														allowmenu: false}
													}
												});
											</script>
--->											<cfelse>
										</cfif>
									</cfif>
									<!------------------------------------------------------------------------------------------------------------> 
									
									<!------------------------------------------------------------------------------------------------------------>
									<cfelse>
									<cfif #SectionArray[Counter][1]# EQ "Map references">
										<cfset MapName = #regionCode#>
										<div class="category_data">
											<cfinclude template="../docs/refmaplink.cfm">
										</div>
										<cfelse>
										<div class="category_data">#SectionArray[Counter][3]#</div>
									</cfif>
								</cfif>
								<!--- defniition word #SectionArray[Counter][1]# ---->
								<cfif #countryCode# EQ "XX">
									<cfelseif #countryCode# NEQ "XX">
									<cfif ListFind(RankOrderList, #SectionArray[counter][4]#)>
										<cfset anchorRow = GetRankOrderNumber(SectionArray[counter][4],countrycode,pubyear)>
										<cfif #anchorRow# EQ -999>
											<!---<span class="category">no information is available to be ranked</span>--->
											<cfelse>
											<cfif isDefined("URL.staticOut")>
												<span class="category" style="padding-left:7px;">country comparison to the world:</span> <span class="category_data"> <a href="../rankorder/#SectionArray[Counter][4]#rank#globalStaticOutputExtension#?countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#&rank=#anchorRow#<!---<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=###countryCode##anchorRow#</cfif>---><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countryCode#</cfif>" onMouseDown=""  title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
												<cfelse>
												<span class="category" style="padding-left:7px;">country comparison to the world:</span><span class="category_data"> <a href="../rankorder/#prefix#rankorderguidetemplate.cfm?FieldKey=#SectionArray[Counter][4]#&countryName=#CountryName#&countryCode=#countryCode#&regionCode=#regionCode#<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=#anchorRow####countryCode##anchorrow#</cfif>" onMouseDown=""  title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
											</cfif>
										</cfif>
									</cfif>
								</cfif></cfif>
							<cfset thisPath = ExpandPath("*.*")>
							<cfset thisDirectory = GetDirectoryFromPath(thisPath)>
							<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../population/" & UCase(countryCode) & "_popgraph #PubYear#.bmp">
							<cfif FileExists(thePath)>
								<cfif isDefined("URL.staticOut")>
									 <cfif FindNoCase("note",#SectionArray[Counter][2]#,1)>
										<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
											<div class="category"><span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a onClick="hideTip();MM_openBrWindow('../population/populationtemplate_#ucase(CountryCode)##globalStaticOutputExtension#','#CountryName# Anthem','status=no,scrollbars=no,resizable=no,width=850,height=490');return false" href="../population/populationtemplate_#ucase(countryCode)##globalStaticOutputExtension#" onMouseOver="doTooltip(event,3)" onMouseOut="hideTip()" target="_blank"><img src="../graphics/poppyramid_icon.jpg" width="44" height="33" /></a></div>
										</cfif>      
										<cfelseif FindNoCase("65 years and over",#SectionArray[Counter][2]#,1)>
										<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
												<div class="category"><span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a onClick="hideTip();MM_openBrWindow('../population/populationtemplate_#ucase(CountryCode)##globalStaticOutputExtension#','','status=no,scrollbars=no,resizable=no,width=850,height=490');return false" href="../population/populationtemplate_#ucase(CountryCode)##globalStaticOutputExtension#" onMouseOver="doTooltip(event,3)" onMouseOut="hideTip()" target="_blank"><img src="../graphics/poppyramid_icon.jpg" width="44" height="33" /></a></div>
										</cfif>
									</cfif>						 
								<cfelse>
									<cfif FindNoCase("note",#SectionArray[Counter][2]#,1)>
										<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
											<div class="category"><span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a onClick="hideTip();MM_openBrWindow('../population/populationtemplate.cfm?CountryCode=#ucase(CountryCode)#&amp;regionCode=#regionCode#','','status=no,scrollbars=no,resizable=no,width=850,height=490');return false" href="../population/#ucase(countryCode)#_popgraph #PubYear#.bmp" onMouseOver="doTooltip(event,3)" onMouseOut="hideTip()" target="_blank"><img src="../graphics/poppyramid_icon.jpg" width="44" height="33" /></a></div>
										</cfif>      
										<cfelseif FindNoCase("65 years and over",#SectionArray[Counter][2]#,1)>
										<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
											<div class="category"><span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a onClick="hideTip();MM_openBrWindow('../population/populationtemplate.cfm?CountryCode=#ucase(CountryCode)#&amp;regionCode=#regionCode#','','status=no,scrollbars=no,resizable=no,width=850,height=490');return false" href="../population/#ucase(countryCode)#_popgraph #PubYear#.bmp" onMouseOver="doTooltip(event,3)" onMouseOut="hideTip()" target="_blank"><img src="../graphics/poppyramid_icon.jpg" width="44" height="33" /></a></div>
										</cfif>
									</cfif>
								</cfif>						
							</cfif>
				</cfoutput>
			</cfloop>
			<tr>
				<td class="category_data" style="padding-bottom: 5px;"></td>
			</tr>
			<tr>
				<td colspan="3">
					<cfif not isDefined("URL.print")>
						<cfoutput>
							<div id="backtotop">
								<table border="0" cellspacing="0" cellpadding="0" width="100%">
									<tr>
										<td height="25" class="dashed_top_grey_line">
											<div align="right"><a href="##top"><img src="../graphics/backtotop_icon.gif" alt="Back to Top"  border="0" style="padding:2px; cursor: pointer;" title="Back to Top"/></a></div>
										</td>
									</tr>
								</table>
							</div>
						</cfoutput>
					</cfif>
				</td>
			</tr>
		</table>
		
				</td>
		
		
				</tr>
		
	</table>
	</ul>
	</div>
	</div>
	</div>
</cfif>
</div>
<!--- World Factbook 2007/geos/countrytemplateoutput.cfm  --->
<cfif NOT isDefined("URL.print")>
	<script language="javascript" type="text/javascript">
<!--
//	var CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput> = new Spry.Widget.CollapsiblePanel("CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput>", null, "<cfoutput>#URL.countryCode#</cfoutput>");
var CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput> = new Spry.Widget.CollapsiblePanel("CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput>", null, "LASTCRNTYCODE", "<cfoutput>#URL.countryCode#</cfoutput>");
//-->
</script>
</cfif>
