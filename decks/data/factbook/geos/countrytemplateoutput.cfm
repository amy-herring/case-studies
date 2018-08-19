<cfset intelinkTooltip  = "<img src = '../graphics/field_listing_tooltip.gif'>">
<cfset googleTooltip = "<img src = '../graphics/google_tooltip.gif'>">
<cfset fieldTooltip = "<img src = '../graphics/field_listing_tooltip.gif'>">
<cfset populationTooltip = "<img src = '../graphics/populationpyramid_tooltip.gif'>">
<cfset fertilityTooltip = "<img src = '../graphics/fertilitypyramid_tooltip.gif'>">
<cfset areacomparisonTooltip = "<img src = '../graphics/areacomparison_tooltip.gif'>">
<script src="jClocksGMT-master/js/jClocksGMT.js"></script>
<script src="jClocksGMT-master/js/jquery.rotate.js"></script>
<link rel="stylesheet" href="jClocksGMT-master/css/jClocksGMT.css">
<cfif NOT isCategoryCodeAvailable(CatCode_FromCountryTemplate, GetCountrySection)>
<cfelse>
<!--- Load the results of the query into an array for parsing --->
<cfset SectionArray = ArrayNew(2)>
<cfloop query="GetCountrySection">
	<cfif GetCountrySection.catcode IS CatCode_FromCountryTemplate>
		<cfset SectionArray[ArrayLen(SectionArray)+1][1]= GetCountrySection.FIELDDESC>
		<cfset SectionArray[ArrayLen(SectionArray)][2]= GetCountrySection.SUBFIELD>
		<cfset SectionArray[ArrayLen(SectionArray)][3]= GetCountrySection.TEXT>
		<cfset SectionArray[ArrayLen(SectionArray)][4]= GetCountrySection.fieldkey>
	</cfif>
</cfloop>


		
<script>
	<cfif NOT isDefined("URL.print")>
		$(document).ready(function() { 
			<cfif #regionCode# EQ 'ant'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#e3dbe3',"border-bottom":"2px solid white","cursor":"pointer"}); // ant 
			<cfelseif #regionCode# EQ 'afr'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#d9e4cd',"border-bottom":"2px solid white","cursor":"pointer"}); // afr 
			<cfelseif #regionCode# EQ 'aus'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#cce0eb',"border-bottom":"2px solid white","cursor":"pointer"}); // aus 
			<cfelseif #regionCode# EQ 'eur'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#f1e3d0',"border-bottom":"2px solid white","cursor":"pointer"}); // eur 			
			<cfelseif #regionCode# EQ 'cam'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#cce5e5',"border-bottom":"2px solid white","cursor":"pointer"}); // cam 			
			<cfelseif #regionCode# EQ 'cas'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#ebd8d8',"border-bottom":"2px solid white","cursor":"pointer"}); // cas 			
			<cfelseif #regionCode# EQ 'eas'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#efdfd6',"border-bottom":"2px solid white","cursor":"pointer"}); // eas
			<cfelseif #regionCode# EQ 'mde'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#ebe0cc',"border-bottom":"2px solid white","cursor":"pointer"}); // mde
			<cfelseif #regionCode# EQ 'noa'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#ccdae5',"border-bottom":"2px solid white","cursor":"pointer"}); // noa
			<cfelseif #regionCode# EQ 'oce'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#cce3e7',"border-bottom":"2px solid white","cursor":"pointer"}); // oce
			<cfelseif #regionCode# EQ 'soa'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#e6e6d9',"border-bottom":"2px solid white","cursor":"pointer"}); // sao			
			<cfelseif #regionCode# EQ 'sas'>
				$('[id^="CollapsiblePanel1"] h2').css({'background-color':'#e4d4d4',"border-bottom":"2px solid white","cursor":"pointer"}); // sas			
			</cfif>
		});
	</cfif>
   </script>

<!---<cfquery name="getcountries" datasource="#APPLICATION.dsn#">
			SELECT 		s.text
			FROM 		factbook.summary s
			WHERE 		s.PUBYEAR = <cfoutput>#PubYear#</cfoutput> 
			AND			s.fieldkey = 2057
			and 		s.factseq = 6
			and 		s.geocode = '<cfoutput>#countrycode#</cfoutput>'
	</cfquery>
	<script>
		$(document).ready(function(){
			<cfoutput query ="getcountries">
				<cfset utc = listGetAt(text,1, '(')>
				<cfset utcLength = Len(utc)>
				<cfset utc1 = #RemoveChars(utc,1,3)#>
				<cfset utc2 = #ltrim(utc1)#>
				<cfset utc3 = #rtrim(utc2)#>
				$("##clock_#countrycode#").jClocksGMT({offset: "#rtrim(utc3)#", hour24: true});
			</cfoutput>
	
			//$('#clock_hou').jClocksGMT({offset: '-5', hour24: true});
			//$('#clock_dc').jClocksGMT({offset: '-5', digital: false});
			//$('#clock_india').jClocksGMT({offset: '+5.5'});
		});
        </script>--->

<div id="CollapsiblePanel1<cfoutput>_#CatCode_FromCountryTemplate#</cfoutput>" class="CollapsiblePanel" style="width:100%; ">
<div class="wrapper">
<h2 class="question question-back" ccode="<cfoutput>#countrycode#</cfoutput>" sectiontitle="<cfoutput>#SectionName#</cfoutput>"><a href="javascript:void();"><cfoutput>#SectionName#</cfoutput></span> ::</span><span class="region"><cfoutput>#countryname#</cfoutput></span></a></h2>
<div class="answer" align="left">
	<div class="box" style="padding: 0px; margin: 0px;">
		<ul style="text-align: left;padding: 0px;margin: 0px;width: 100%;">
			<table border="0" cellspacing="0" cellpadding="0"  style="width: 100%;">
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
					
					<td width="450" height="20"><div class="category" style="padding-left:5px;" id="field"> <cfoutput>
								<cfif isDefined("staticOut")>
									<cfif #SectionArray[1][4]# EQ 2203>
										#SectionArray[1][1]#:
										<cfelse>
										<a href="../docs/notesanddefs#globalStaticOutputExtension#?fieldkey=#SectionArray[1][4]#&alphaletter=#Left(SectionArray[1][1],1)#&term=#SectionArray[1][1]#" title="Notes and Definitions: #SectionArray[1][1]#"> #SectionArray[1][1]#</a>:
									</cfif>
									<cfelse>
									<cfif #SectionArray[1][4]# EQ 2203>
										#SectionArray[1][1]#:
										<cfelse>
										<a href="../docs/#prefix#notesanddefs.cfm?fieldkey=#SectionArray[1][4]#&alphaletter=#Left(SectionArray[1][1],1)#&term=#SectionArray[1][1]#" title="Notes and Definitions: #SectionArray[1][1]#"> #SectionArray[1][1]#</a>:
									</cfif>
								</cfif>
							</cfoutput> </div></td>
					<cfif NOT isDefined("URL.print")>
						<td align="right">
						
						<!---Create the link to the definition for this Field - except for fields in the NoLinksList --->
						<cfif ListFind(NoLinksList, "#SectionArray[1][4]#") is 0>
							<cfset anchorRow = GetFieldListing(SectionArray[1][4],countrycode,pubyear)>
							<cfif isDefined("staticOut")>
								<a href="../fields/#SectionArray[1][4]##globalStaticOutputExtension####countrycode#" title="#fieldTooltip#"> <img src="../graphics/field_listing_on.gif" border="0"  style="text-decoration:none;"> </a>
								<cfelse>
								<cfif ListFind(RankOrderList, SectionArray[1][4]) GT 0>
									<cfif #countrycode# EQ "XX">
										<cfelseif #countrycode# NEQ "XX">
										<cfset rankAnchorRow = GetRankOrderNumber(SectionArray[1][4],countrycode,pubyear)>
									</cfif>
								</cfif>
								<a href="../fields/#prefix#fieldstemplate.cfm?fieldkey=#SectionArray[1][4]#&countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#&<cfif ListFind(RankOrderList, #SectionArray[1][4]#)><cfif countrycode NEQ "XX">rankAnchorRow=#rankAnchorRow#</cfif></cfif><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countrycode##anchorrow#</cfif>" title="#fieldTooltip#"><img src="../graphics/field_listing_on.gif" border="0"  style="text-decoration:none;"></a>
							</cfif>
						</cfif>
						<!---Create a link to the Google search page on Intelink --->
						<cfset SearchTerm = countryname & " " & SectionArray[1][1]>
						<!---Replace the dashes in the search string with nulls --->
						<cfset SearchTerm = Replace(SearchTerm,"- ","","All")>
						<!---  Replace the blanks in the search string with + signs --->
						<cfset SearchTerm = Replace(SearchTerm," ","+","All")>
						<cfif isDefined("URL.staticOut")>
							<cfif isDefined("URL.forInternalDynamic")>
								<a href="http://home.ismc.ic.gov/search/?q=#SearchTerm#" title="#intelTooltip#"> <img src="../graphics/intelink_search.gif"  style="text-decoration: none;"> </a>
							</cfif>
							<cfelse>
							<cfset SearchTerm = Replace(SearchTerm,"(","","All")>
							<cfset SearchTerm = Replace(SearchTerm,")","","All")>
							<a href="../#prefix#search_results.cfm?q=#URLEncodedFormat(SearchTerm)#&site=WORLDFACTBOOK&client=WORLDFACTBOOK&proxystylesheet=WORLD_FACTBOOK&output=xml_no_dtd&restrict=WORLDFACTBOOK" title="#googleTooltip#"><img src="../graphics/goog.gif" style="text-decoration: none;"></a>
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
						<cfif #countrycode# EQ "XX">
							<cfelseif #countrycode# NEQ "XX">
							<cfset anchorRow = GetRankOrderNumber(SectionArray[1][4],countrycode,pubyear)>
							<cfif #anchorRow# EQ -999>
								<!---<span class="category">no information is available to be ranked</span>--->
								<cfelse>
								<cfif isDefined("URL.staticOut")>
									<span class="category" style="padding-left:7px;">country comparison to the world:</span> <span class="category_data"> <a href="../rankorder/#SectionArray[1][4]#rank#globalStaticOutputExtension#?countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#&rank=#anchorRow#<!---<cfif anchorRow EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=###countrycode##anchorRow#</cfif>---><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countrycode##anchorrow#</cfif>" onMouseDown="" title="Country comparison to the world" alt="Country comparison to the world">#anchorRow#</a> </span>
									<cfelse>
									<span class="category" style="padding-left:7px;">country comparison to the world:</span><span class="category_data"> <a href="../rankorder/#prefix#rankorderguidetemplate.cfm?fieldkey=#SectionArray[1][4]#&countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=#anchorRow####countrycode##anchorrow#</cfif>" onMouseDown=""  title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
								</cfif>
							</cfif>
						</cfif>
					</cfif>
				</cfoutput> 
				<!--- Output the rest of the elements of the array --->
				
				<cfloop index="Counter" from=2 to="#ArrayLen(SectionArray)#">
					<cfif FindNoCase("daylight saving time",#SectionArray[Counter][2]#,1)>
					<cfset dst = true>
					<cfelse>
					<cfset dst = true>
					</cfif>
				</cfloop>
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
								<!---used for prod ---> 
								<!---<cfquery name="AllChiefs" datasource="whiteboard" username="leadershippros" password="shln84edyae9">
							SELECT country_code
							FROM leadershippros.chiefs_of_state
							group by country_code
						</cfquery>---> 
								<!--- used for test --->
								<cfquery name="AllChiefs" datasource="lpro">
							SELECT country_code
							FROM leadershippros.chiefs_of_state
							group by country_code
						</cfquery>
								<cfset allchiefslist = ValueList(AllChiefs.country_code)>
								<cfif listfindNoCase(allchiefslist,#countrycode#,",")>
									<cfset chiefsLink_S ="/library/publications/world-leaders-1/#ucase(countrycode)#.html">
									<cfset chiefsLink_D ="https://cos.di.cia/index.cfm?country_code=#ucase(countrycode)#&codedesc=#CountryName#&viewmode=user">
									<cfelse>
									<cfset chiefsLink_S = "/library/publications/world-leaders-1/">
									<cfset chiefsLink_D = "https://cos.di.cia">
								</cfif>
								<cfif isDefined("URL.staticOut")>
									<span class="category" style="padding-left:7px;font-weight: normal;">(For more information visit the <a href="#chiefsLink_S#" target="_blank">World Leaders website</a>&nbsp;<img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/>)</span>
									<cfelse>
									<span class="category" style="padding-left:7px;font-weight: normal;">(For more information visit the <a href="#chiefsLInk_D#" target="_blank">Chiefs of State website</a>&nbsp;<img src="../graphics/#regionCode#_newwindow.gif" alt="Opens in New Window" title="Opens in New Window" border="0"/>)</span>
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
												<a href="../docs/notesanddefs#globalStaticOutputExtension#?fieldkey=#SectionArray[counter][4]#&alphaletter=#Left(SectionArray[counter][1],1)#&term=#SectionArray[counter][1]#" title="Notes and Definitions: #SectionArray[counter][1]#"> #SectionArray[counter][1]#:</a>
												<cfelse>
												<a href="../docs/#prefix#notesanddefs.cfm?fieldkey=#SectionArray[counter][4]#&alphaletter=#Left(SectionArray[counter][1],1)#&term=#SectionArray[counter][1]#" title="Notes and Definitions: #SectionArray[counter][1]#">#SectionArray[counter][1]#:</a>
											</cfif>
										</cfoutput> </div></td>
								<cfif NOT isDefined("URL.print")>
									<!--- Field and Google Icons --->
									<td align="right"><!---Create the link to the definition for this Field - except for fields in the NoLinksList ---> 
										<!---Create the link to the listing of all countries for this Field  - except for fields in the NoLinksList --->
										
										<cfif ListFind(NoLinksList, "#SectionArray[Counter][4]#") is 0>
											<cfset anchorRow = GetFieldListing(SectionArray[counter][4],countrycode,pubyear)>
											<cfif isDefined("staticOut")>
												<a href="../fields/#SectionArray[counter][4]##globalStaticOutputExtension#<cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countrycode#</cfif>" title="#fieldTooltip#"><img src="../graphics/field_listing_on.gif" border="0" ></a>
												<cfelse>
												<!---<cfset anchorRow = GetFieldListing(SectionArray[counter][4],countrycode,pubyear)>--->
												<cfif ListFind(RankOrderList, SectionArray[counter][4]) GT 0>
													<cfif #countrycode# NEQ "XX">
														<cfset rankAnchorRow = GetRankOrderNumber(SectionArray[counter][4],countrycode,pubyear)>
													</cfif>
												</cfif>
												<a href="../fields/#prefix#fieldstemplate.cfm?fieldkey=#SectionArray[counter][4]#&countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#&<cfif ListFind(RankOrderList, #SectionArray[counter][4]#)><cfif countrycode NEQ "XX">rankAnchorRow=#rankAnchorRow#</cfif></cfif><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countrycode##anchorrow#</cfif>" title="#fieldTooltip#"><img src="../graphics/field_listing_on.gif" border="0"  style="text-decoration:none;"></a>
											</cfif>
										</cfif>
										
										<!---Create a link to the Google search page on Intelink --->
										
										<cfset SearchTerm = countryname & " " & SectionArray[Counter][1]>
										
										<!---Replace the dashes in the search string with nulls --->
										
										<cfset SearchTerm = Replace(SearchTerm,"- ","","All")>
										
										<!---Replace the blanks in the search string with + signs --->
										
										<cfset SearchTerm = Replace(SearchTerm," ","+","All")>
										<cfif isdefined("URL.staticOut")>
											<cfif isDefined("URL.forInternalDynamic")>
												<a href="http://home.ismc.ic.gov/search/?q=#SearchTerm#"> <img src="../graphics/intelink_search.gif"  style="text-decoration: none;"  title="#intelinkTooltip#"> </a>
											</cfif>
											<cfelse>
											<cfset SearchTerm = Replace(SearchTerm,"(","","All")>
											<cfset SearchTerm = Replace(SearchTerm,")","","All")>
											<a href="../#prefix#search_results.cfm?q=#URLEncodedFormat(SearchTerm)#&site=WORLDFACTBOOK&client=WORLDFACTBOOK&proxystylesheet=WORLD_FACTBOOK&output=xml_no_dtd&restrict=WORLDFACTBOOK" title="#googleTooltip#"> <img src="../graphics/goog.gif" style="text-decoration: none;" ></a>
										</cfif></td>
								</cfif>
							</tr>
							<tr height="22">
							
							<td colspan="2" id="data">
							<!--- If this is the Climate field 2059, link to the current weather --->
							<cfif isDefined("URL.staticOut")>
								<cfelse>
								<cfif SectionArray[Counter][1] EQ "Climate">
									<span class="category_data"> <a href="http://www.ismc.ic.gov/weather/locations.aspx?country=#countryname#" target="_blank" >Current Weather</a> </span>
								</cfif>
							</cfif>
							
							<!--- Display the Subfield (if it is not blank) and the Text for this field --->
							<cfif #SectionArray[Counter][2]# is not "">
								<div class="category">#SectionArray[Counter][2]#: <span class="category_data" style="font-weight:normal; vertical-align:bottom;">#SectionArray[Counter][3]#</span></div>
								
								<!--- THIS IS WHERE THE AUDIO PORTION FOR THE NATIONAL ANTHEMS  ------------------------------------------>
								<cfif #SectionArray[counter][1]# EQ "National anthem">
									<cfquery name="getMP3" datasource="#APPLICATION.dsn#">
											SELECT * 
											FROM anthems
											WHERE geocode = '#UCase(countrycode)#'
										</cfquery>
									<cfset thisPath = ExpandPath("*.*")>
									<cfset thisDirectory = GetDirectoryFromPath(thisPath)>
									<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../anthems/" & UCase(countrycode) & ".mp3">
									<cfif FileExists(thePath)>
										<link rel="stylesheet" type="text/css" href="../styles/jquery.ui.core.css"/>
										<p><a href="../anthems/#UCase(countrycode)#.mp3" class="playAnthem" name="#countryname#" target="_new"><img src="../graphics/play_anthem.png"></a></p>
										<!---<script type="text/javascript">
											$('.playAnthem').popupWindow({
												centerBrowser:1,
												height:50,
												width:300,
												location:0, // determines whether the address bar is displayed {1 (YES) or 0 (NO)}.
												menubar:0, // determines whether the menu bar is displayed {1 (YES) or 0 (NO)}.
												resizable:0, // whether the window can be resized {1 (YES) or 0 (NO)}. Can also be overloaded using resizable.
												scrollbars:0, // determines whether scrollbars appear on the window {1 (YES) or 0 (NO)}.
												status:0, // whether a status line appears at the bottom of the window {1 (YES) or 0 (NO)}.
												//windowName: "#countryname#" , // name of window set from the name attribute of the element that invokes the click
												windowURL: 0, // url used for the popup
												top:0, // top position when the window appears.
												toolbar:0
											});
										</script>--->
										<cfelse>
									</cfif>
								</cfif>
								<!------------------------------------------------------------------------------------------------------------>
								<cfelse>
								<cfif #SectionArray[Counter][1]# EQ "Map references">
									<cfset MapName = #regionCode#>
									<div class="category_data">
										<cfinclude template="../docs/refmaplink.cfm">
									</div>
									<cfelseif #SectionArray[Counter][1]# EQ "Area - comparative">						
										<cfset thisPath = ExpandPath("*.*")>
										<cfset thisDirectory = GetDirectoryFromPath(thisPath)>
										<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../graphics/areacomparison/" & UCase(countrycode) & "_area #pubyear#.jpg" >
										<cfif FileExists(thePath)>
											<cfset areaComparativeText  = #SectionArray[Counter][3]#>
											<cfelse>
											<div class="category_data">#SectionArray[Counter][3]#</div>
										</cfif>								
									<cfelse>
									<div class="category_data">#SectionArray[Counter][3]#</div>
								</cfif>
							</cfif>
							<!--- defniition word #SectionArray[Counter][1]# ---->
							<cfif #countrycode# EQ "XX">
								<cfset flagsubfield = "">
								<cfset countryaffiliation = "">
								<cfset FLAGDESCRIPTION  = "">
								<cfset FLAGDESCRIPTIONNOTE   = "">
								<cfelseif #countrycode# NEQ "XX">
								<cfif ListFind(RankOrderList, #SectionArray[counter][4]#)>
									<cfset anchorRow = GetRankOrderNumber(SectionArray[counter][4],countrycode,pubyear)>
									<cfif #anchorRow# EQ -999>
										<!---<span class="category">no information is available to be ranked</span>--->
										<cfelse>
										<cfif isDefined("URL.staticOut")>
											<span class="category" style="padding-left:7px;">country comparison to the world:</span> <span class="category_data"> <a href="../rankorder/#SectionArray[Counter][4]#rank#globalStaticOutputExtension#?countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#&rank=#anchorRow#<!---<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=###countrycode##anchorRow#</cfif>---><cfif #anchorRow# EQ -999>###anchorRow#<cfelse>###countrycode#</cfif>" onMouseDown=""  title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
											<cfelse>
											<span class="category" style="padding-left:7px;">country comparison to the world:</span><span class="category_data"> <a href="../rankorder/#prefix#rankorderguidetemplate.cfm?fieldkey=#SectionArray[Counter][4]#&countryname=#countryname#&countrycode=#countrycode#&regionCode=#regionCode#<cfif #anchorRow# EQ -999>&anchorRow=#anchorRow#<cfelse>&anchorRow=#anchorRow####countrycode##anchorrow#</cfif>" onMouseDown="" title="Country comparison to the world" alt="Country comparison to the world"> #anchorRow# </a> </span>
										</cfif>
									</cfif>
								</cfif>
							</cfif>
						</cfif>
						<cfset thisPath = ExpandPath("*.*")>
						<cfset thisDirectory = GetDirectoryFromPath(thisPath)>
						<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../graphics/population/" & UCase(countrycode) & "_popgraph #pubyear#.bmp" >
	
						<!--- beginning of clock for local country time --->
						<!---<cfif #SectionArray[Counter][1]# EQ "Capital">
							<cfif dst EQ false>
								<cfif #SectionArray[counter][2]# EQ "time difference">
									<cfoutput>
										<div id="clock_#UCase(countrycode)#" class="clock_container" style="padding-top: 5px;">
											<div class="lbl">#countryname#</div>
											<div class="clockHolder">
												<div class="rotatingWrapper"><img class="hour" src="jClocksGMT-master/images/clock_hour.png" /></div>
												<div class="rotatingWrapper"><img class="min" src="jClocksGMT-master/images/clock_min.png" /></div>
												<div class="rotatingWrapper"><img class="sec" src="jClocksGMT-master/images/clock_sec.png" /></div>
												<img class="clock" src="jClocksGMT-master/images/clock_face.png" />
											</div>
											<div class="digital">
												<span class="hr"></span><span class="minute"></span> <span class="period"></span>
											</div>
										</div>
									</cfoutput>
								</cfif>
							<cfelseif dst EQ true>
								<cfif #SectionArray[counter][2]# EQ "daylight saving time">
									<cfoutput>
										<div id="clock_#UCase(countrycode)#" class="clock_container" style="padding-top: 10px;">
											<div class="lbl">#countryname#</div>
											<div class="clockHolder">
												<div class="rotatingWrapper"><img class="hour" src="jClocksGMT-master/images/clock_hour.png" /></div>
												<div class="rotatingWrapper"><img class="min" src="jClocksGMT-master/images/clock_min.png" /></div>
												<div class="rotatingWrapper"><img class="sec" src="jClocksGMT-master/images/clock_sec.png" /></div>
												<img class="clock" src="jClocksGMT-master/images/clock_face.png" />
											</div>
											<div class="digital">
												<span class="hr"></span><span class="minute"></span> <span class="period"></span>
											</div>
										</div>
									</cfoutput>
								</cfif>
							</cfif>--->
							<!---<cfelseif dst EQ false>
								<cfif #SectionArray[counter][2]# EQ "time difference">
									<cfoutput>
										<div id="clock_#UCase(countrycode)#" class="clock_container" style="padding-top: 15px;">
											<div class="lbl">#countryname#</div>
											<div class="clockHolder">
												<div class="rotatingWrapper"><img class="hour" src="jClocksGMT-master/images/clock_hour.png" /></div>
												<div class="rotatingWrapper"><img class="min" src="jClocksGMT-master/images/clock_min.png" /></div>
												<div class="rotatingWrapper"><img class="sec" src="jClocksGMT-master/images/clock_sec.png" /></div>
												<img class="clock" src="jClocksGMT-master/images/clock_face.png" />
											</div>
											<div class="digital">
												<span class="hr"></span><span class="minute"></span> <span class="period"></span>
											</div>
										</div>
									</cfoutput>	
								</cfif>
							</cfif>
						</cfif>	--->	
								<!--- END of clock for local country time --->								
						<cfif isDefined("url.print")>
							<cfelse>
							<cfif FileExists(thePath)>
								<!--- POPULATION GRAPH --->
								<cfif FindNoCase("note",#SectionArray[Counter][2]#,1)>
									<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
										<div class="category">
										<span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a href="javascript:void();" title="#populationTooltip#"> <img src="../graphics/poppyramid_icon.jpg" 
											border="0" 
											style="cursor:pointer; border: 0px solid ##CCC;" 
											id="flagDialog2_#countrycode#" 
											name="#countrycode#" 
											regioncode="#regionCode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											flagsubfield="#flagsubfield#" 
											countryaffiliation="#countryaffiliation#"
											flagdescription="#flagdescription#" 
											flagdescriptionnote="#flagdescriptionnote#" 
											region="#region#" 
											typeimage="population"
											> </a>
									</cfif>
									<cfelseif FindNoCase("65 years and over",#SectionArray[Counter][2]#,1)>
									<cfif #SectionArray[Counter][1]# EQ 'Age structure'>
										<div class="category">
										<span style="margin-bottom:0px; vertical-align:bottom;">population pyramid:</span> <a href="javascript:void();" title="#populationTooltip#"> <img src="../graphics/poppyramid_icon.jpg" 
											border="0" 
											style="cursor:pointer; border: 0px solid ##CCC;" 
											id="flagDialog2_#countrycode#" 
											name="#countrycode#" 
											regioncode="#regionCode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											flagsubfield="#flagsubfield#" 
											countryaffiliation="#countryaffiliation#"
											flagdescription="#flagdescription#" 
											flagdescriptionnote="#flagdescriptionnote#" 
											region="#region#" 
											typeimage="population"
											> </a>
									</cfif>
									<cfelse>
								</cfif>
								<!--- TOTAL FERTILITY GRAPH ---> 
								<!---<cfif  SectionArray[Counter][1] EQ 'Total fertility rate'>
											<div class="category"><span style="margin-bottom:0px; vertical-align:bottom;">fertility graph:</span>
											<a href="javascript:void();" title="#fertilityTooltip#">
											<img src="../graphics/poppyramid_icon.jpg" 
											border="0" 
											style="cursor:pointer; border: 0px solid ##CCC;" 
											id="flagDialog2_#countrycode#" 
											name="#countrycode#" 
											regioncode="#regionCode#" 
											countrycode="#countrycode#"  
											countryname="#countryname#" 
											flagsubfield="#flagsubfield#" 
											countryaffiliation="#countryaffiliation#"
											flagdescription="#flagdescription#" 
											flagdescriptionnote="#flagdescriptionnote#" 
											region="#region#" 
											typeimage="fertility"
											>
											</a>--->
								
							</cfif>
							<!--- AREA COMPARISON GRAPH --->
							<cfset thePath =  GetDirectoryFromPath(GetTemplatePath()) & "../graphics/areacomparison/" & UCase(countrycode) & "_area #pubyear#.jpg" >
							<cfif FileExists(thePath)>
								<cfif  SectionArray[Counter][1] EQ 'Area'>
									<cfset areaComparativeText = SectionArray[Counter][1]>
								</cfif>
								<cfif  SectionArray[Counter][1] EQ 'Area - comparative'>
									<div class="category">
									<span style="margin-bottom:0px; vertical-align:bottom;">Area comparison map:</span> <a href="javascript:void();" title="#areacomparisonTooltip#"> <img src="../graphics/areacomparison_icon.jpg" 
												border="0" 
												style="cursor:pointer; border: 0px solid ##CCC;" 
												id="flagDialog2_#countrycode#" 
												name="#countrycode#" 
												regioncode="#regionCode#" 
												countrycode="#countrycode#"  
												countryname="#countryname#" 
												flagsubfield="#flagsubfield#" 
												countryaffiliation="#countryaffiliation#"
												flagdescription="#areaComparativeText#" 
												flagdescriptionnote="#flagdescriptionnote#" 
												region="#region#" 
												typeimage="areacomparison"
												> </a>
								</cfif>
							</cfif>
						</cfif>
					</cfoutput>
				</cfloop>
				<tr>
					<td class="category_data" style="padding-bottom: 5px;"></td>
				</tr>
			</table>
		</ul>
	</div>
	</cfif>
</div>
