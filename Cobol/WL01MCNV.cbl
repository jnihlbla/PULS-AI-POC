000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             WL01MCNV.                                        
000400 AUTHOR.                 KJELL                                            
000500     DATE-WRITTEN.       MARS 2012                                        
000600*                                                                         
000700*                                                                         
000800*    FUNCTION:                                                            
000900*      TRANSLATE WEB MESSAGE NUMBERS TO CORRESPONDING TEXT                
001000*      TO BE DIPLAYED ON AN IMS SCREEN. IT IS USED WHEN PROGRAMS          
001100*      ARE ACTIVATED IN BOTH THE CLASSIC IMS ENVIRONMENT AND              
001200*      THE PULS *DC WEB ENVIRONMENT.                                      
001300*                                                                         
001400*      NOTE: THIS SUBPROGRAM SHOULD ONLY BE USED IN CONNECTION            
001500*      WITH THE PULS DC WEB APPLICATION! THE MESSAGE CODES ARE            
001600*      NOT APPLICABLE IN OTHER WEB APPLICATIONS.                          
001700*                                                                         
001800*    CALLED BY:                                                           
001900*          CALL WL01MCNV USING MCNV-AREA                                  
002000*                                                                         
002100*    INPUT ARGUMENTS:                                                     
002200*       MCNV-IDSPRAK    ISO LANGUAGE CODE FOR THE OUTPUT                  
002300*                       MESSAGE TEXT. ONLY SV OR EN ALLOWED               
002400*                                                                         
002500*       MCNV-IDMSG-INFO MESSAGE CODE. THE TEXT WILL BE PLACED             
002600*                       IN MCNV-TEMFSINF                                  
002700*                                                                         
002800*       MCNV-IDMSG-ERROR MESSAGE CODE. THE TEXT WILL BE PLACED            
002900*                       IN MCNV-TEMFSFEL                                  
003000*                                                                         
003100*       MCNV-IDLEMT-ERROR OPTIONAL DATA ITEM NAME. SOME MESSAGES          
003200*                       HAVE A VARIABLE PORTION THAT CONTAINS             
003300*                       A FIELD NAME.                                     
003400*                                                                         
003500*    OUTPUT ARGUMENTS:                                                    
003600*       MCNV-TEMFSINF   OUTPUT INFRMATIONAL MESSAGE TEXT                  
003700*                                                                         
003800*       MCNV-TEMFSFEL   OUTPUT ERROR MESSAGE TEXT                         
003900*                                                                         
004000     EJECT                                                                
004100                                                                          
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  PROGRAM-NAMN            PIC X(8)  VALUE 'WL01MCNV'.                  
004600                                                                          
004700 77  WMEDKONV                PIC X(8)  VALUE 'WMEDKONV'.                  
004800                                                                          
004900 01  -COPY  WMEDAREA                                                      
005000                                                                          
005100 01  INDX                 PIC S9(4) BINARY   VALUE ZERO.                  
005200 01  MAX-INDX             PIC S9(4) BINARY   VALUE 543.                   
005300                                                                          
005400 01  MESSAGE-TABLE.                                                       
005500*       'AAA = NEW WEB MSG CODE                                 '.        
005600*       '   XXXXXXXXXXXXXXXX = OPTIONAL ITEM NAME               '.        
005700*       '                   BBB = OLD MEDKONV MSG CODE          '.        
005800*       '                       ********************************'.        
005900     03 PIC X(55) VALUE                                                   
006000        '020*               001 CORRECT HIGHLIGHT FIELDS        '.        
006100     03 PIC X(55) VALUE                                                   
006200        '046                002 CONFLICTING ACTIONS             '.        
006300     03 PIC X(55) VALUE                                                   
006400        '013                003 PRESS PF11 TO UPDATE            '.        
006500     03 PIC X(55) VALUE                                                   
006600        'AAA                004 SPECIFY ONE CHOICE (SEE BELOW)  '.        
006700     03 PIC X(55) VALUE                                                   
006800        '025KEY             005 KEYS ARE MISSING                '.        
006900     03 PIC X(55) VALUE                                                   
007000        '010*               006 THIS IS THE FIRST PAGE          '.        
007100     03 PIC X(55) VALUE                                                   
007200        '007*               007 UPDATE NOT ALLOWED              '.        
007300     03 PIC X(55) VALUE                                                   
007400        'AAA                008 ADJUSTMENTS IN ADVANCE NOT ALLOW'.        
007500     03 PIC X(55) VALUE                                                   
007600        'AAA                009 ADJUSTMENTS OLDER THAN TWO YEARS'.        
007700*    -- DO NOT CHANGE MSG BELOW. ELEMENT NAME MUST BE BLANK HERE          
007800*    -- ADD SPECIFIC TRANSLATIONS INSTEAD IF NEEDED.                      
007900     03 PIC X(55) VALUE                                                   
008000        '025                010 MISSING IN REGISTER             '.        
008100     03 PIC X(55) VALUE                                                   
008200        '014                011 PF11 AND NO INPUT               '.        
008300     03 PIC X(55) VALUE                                                   
008400        '041SALES INFO      012 SALES INFORMATION MISSING       '.        
008500     03 PIC X(55) VALUE                                                   
008600        '023KDBYTREF        013 WRONG CODE                      '.        
008700     03 PIC X(55) VALUE                                                   
008800        'AAA                014 WRONG LINE NUMBER               '.        
008900     03 PIC X(55) VALUE                                                   
009000        '023KDVVKL          015 UNKNOWN VOLUME VALUE CLASS      '.        
009100     03 PIC X(55) VALUE                                                   
009200        'AAA                016 REQUIRES BOTH VALUES            '.        
009300     03 PIC X(55) VALUE                                                   
009400        '025IDARTNR         017 PART MISSING                    '.        
009500     03 PIC X(55) VALUE                                                   
009600        '223                018 PART SUPERSEDED                 '.        
009700     03 PIC X(55) VALUE                                                   
009800        'AAA                019 ERROR. REJECTED BY DISPATCHER   '.        
009900     03 PIC X(55) VALUE                                                   
010000        '024                020 FIELDS ARE NOT NUMERIC          '.        
010100     03 PIC X(55) VALUE                                                   
010200        '362                021 HAZARDOUS GOODS                 '.        
010300     03 PIC X(55) VALUE                                                   
010400        'AAA                022 UNFINISHED CUSTOMS ID: FINISH IT'.        
010500     03 PIC X(55) VALUE                                                   
010600        '025IDBORD          023 TABLE MISSING                   '.        
010700     03 PIC X(55) VALUE                                                   
010800        'AAA                024 DEFAULT TABLE INSERTED          '.        
010900     03 PIC X(55) VALUE                                                   
011000        '041KDORDBEK        025 ORDER CONF. CODE MISSING        '.        
011100     03 PIC X(55) VALUE                                                   
011200        '041IDDC            026 DC MISSING                      '.        
011300     03 PIC X(55) VALUE                                                   
011400        'AAA                027 ONLY PRC OR PRINTER IS VALID    '.        
011500     03 PIC X(55) VALUE                                                   
011600        '025ITEMS           028 ITEMS FOR ENTERED DC MISSING    '.        
011700     03 PIC X(55) VALUE                                                   
011800        '025ITEMS           029 ITEMS MISSING                   '.        
011900     03 PIC X(55) VALUE                                                   
012000        'AAA                030 JOINED ORDER                    '.        
012100     03 PIC X(55) VALUE                                                   
012200        'AAA                031 ENTER THE SAME PRINTER ID       '.        
012300     03 PIC X(55) VALUE                                                   
012400        'AAA                032 ONLY PRINTER P AND X ALLOWED    '.        
012500     03 PIC X(55) VALUE                                                   
012600        'AAA                033 ADVICE NOTE WITH INCORRECT LINES'.        
012700     03 PIC X(55) VALUE                                                   
012800        '004*               034 NO UPDATING DONE                '.        
012900     03 PIC X(55) VALUE                                                   
013000        '112                035 NO MORE ORDER PARTS             '.        
013100     03 PIC X(55) VALUE                                                   
013200        '025IDPLKLST        036 ORDER PARTS MISSING             '.        
013300     03 PIC X(55) VALUE                                                   
013400        'AAA                037 PICKING UNIT ON PRINTER QUEUE   '.        
013500     03 PIC X(55) VALUE                                                   
013600        'AAA                038 NON COMPL. PICKING UNIT ON PR. Q'.        
013700     03 PIC X(55) VALUE                                                   
013800        'AAA                039 PICKING UNIT NOT FILLED         '.        
013900     03 PIC X(55) VALUE                                                   
014000        '025IDDISTR-IDKUNDNR040 DISTRICT/CUSTOMER MISSING       '.        
014100     03 PIC X(55) VALUE                                                   
014200        'AAA                041 RATIONING FACTOR MISSING        '.        
014300     03 PIC X(55) VALUE                                                   
014400        'AAA                042 TRANSPORT IDENTITY NOT REGISTERE'.        
014500     03 PIC X(55) VALUE                                                   
014600        'AAA                043 ORDER EXISTS WITH LATER DEPARTUR'.        
014700     03 PIC X(55) VALUE                                                   
014800        'AAA                044 DIFFERENT TRANSPORT CATEGORIES O'.        
014900     03 PIC X(55) VALUE                                                   
015000        'AAA                045 ENTER DEPARTURE OR RFS          '.        
015100     03 PIC X(55) VALUE                                                   
015200        'AAA                046 ENTER EITHER DEPARTURE OR RFS   '.        
015300     03 PIC X(55) VALUE                                                   
015400        'AAA                047 TIME IS NOT WWDHHMM             '.        
015500     03 PIC X(55) VALUE                                                   
015600        '026CMD             048 ENTER CMD                       '.        
015700     03 PIC X(55) VALUE                                                   
015800        'AAA                049 WRONG TRANSPORT CATEGORY ON CHOS'.        
015900     03 PIC X(55) VALUE                                                   
016000        '025349             050 DIRECT DELIVERY PART MISSING    '.        
016100     03 PIC X(55) VALUE                                                   
016200        'AAA                051 PART NOT MARKED FOR DIRECT DELIV'.        
016300     03 PIC X(55) VALUE                                                   
016400        'AAA                052 ORDER IS CANCELLED              '.        
016500     03 PIC X(55) VALUE                                                   
016600        'AAA                053 ORDER NOT COMPLETED             '.        
016700     03 PIC X(55) VALUE                                                   
016800        '041IDORDER         054 ORDER MISSING                   '.        
016900     03 PIC X(55) VALUE                                                   
017000        'AAA                055 TRANSPORT NOT BOOKED            '.        
017100     03 PIC X(55) VALUE                                                   
017200        'AAA                056 THERE ARE NO MORE LINES         '.        
017300     03 PIC X(55) VALUE                                                   
017400        'AAA                057 ORDER IS COMPLETED              '.        
017500     03 PIC X(55) VALUE                                                   
017600        'AAA                058 ORDER CONFIRMATIONS MISSING     '.        
017700     03 PIC X(55) VALUE                                                   
017800        '025VOR LINES       059 LINES MISSING ON THE VOR QUEUE  '.        
017900     03 PIC X(55) VALUE                                                   
018000        'AAA                060 SPECIFY EITHER F OR N           '.        
018100     03 PIC X(55) VALUE                                                   
018200        'AAA                061 NOT THE SAME DISTRICT/CUSTOMER  '.        
018300     03 PIC X(55) VALUE                                                   
018400        'AAA                062 ORDER HEAD CANNOT BE ALTERED    '.        
018500     03 PIC X(55) VALUE                                                   
018600        '041IDKUNDNR        063 CUSTOMER INFORMATION IS MISSING '.        
018700     03 PIC X(55) VALUE                                                   
018800        'AAA                064 A NEW TRANSPORT CANNOT BE GIVEN '.        
018900     03 PIC X(55) VALUE                                                   
019000        'AAA                065 ORDER NO. ALREADY EXISTS        '.        
019100     03 PIC X(55) VALUE                                                   
019200        'AAA                066 CANCELLATION NOT POSSIBLE       '.        
019300     03 PIC X(55) VALUE                                                   
019400        'AAA                067 ALL LINES PRINTED               '.        
019500     03 PIC X(55) VALUE                                                   
019600        'AAA                068 FREIGHT CODE CANNOT BE ALTERED  '.        
019700     03 PIC X(55) VALUE                                                   
019800        'AAA                069 TPO WITHIN FREEZE TIME          '.        
019900     03 PIC X(55) VALUE                                                   
020000        'AAA                070 DISTRICT/CUSTOMER ALREADY EXISTS'.        
020100     03 PIC X(55) VALUE                                                   
020200        'AAA                071 NO MORE ORDERS AT THIS DEPARTURE'.        
020300     03 PIC X(55) VALUE                                                   
020400        'AAA                072 THE ORDER IS NOT CORRECT        '.        
020500     03 PIC X(55) VALUE                                                   
020600        '025IDPRC           073 PRC MISSING                     '.        
020700     03 PIC X(55) VALUE                                                   
020800        'AAA                074 NO MORE CARRIERS                '.        
020900     03 PIC X(55) VALUE                                                   
021000        '041P-TIME TABLE    075 P-TIME TABLE MISSING            '.        
021100     03 PIC X(55) VALUE                                                   
021200        'AAA                076 RECORD IDENTITY NOT SUPPORTED   '.        
021300     03 PIC X(55) VALUE                                                   
021400        'AAA                077 ADDITION NOT ALLOWED            '.        
021500     03 PIC X(55) VALUE                                                   
021600        '041TRANSACTION     078 TRANSACTION MISSING             '.        
021700     03 PIC X(55) VALUE                                                   
021800        '390REPORT          212 REPORT HAS WRONG STATUS         '.        
021900     03 PIC X(55) VALUE                                                   
022000        'AAA                080 EMPTY ROW INDICATED             '.        
022100     03 PIC X(55) VALUE                                                   
022200        '379                081 PRESS PF4 FOR PRINT-OUT         '.        
022300     03 PIC X(55) VALUE                                                   
022400        'AAA                082 ORDER READY                     '.        
022500     03 PIC X(55) VALUE                                                   
022600        'AAA                083 OK TO CANCEL - PRESS PF11       '.        
022700     03 PIC X(55) VALUE                                                   
022800        'AAA                084 NOT CANCELLED - PRC TABLE EXISTS'.        
022900     03 PIC X(55) VALUE                                                   
023000        'AAA                085 NOT CANCELLED - ORDER PARTS IN Q'.        
023100     03 PIC X(55) VALUE                                                   
023200        '025KDFRAKT         086 FREIGHT CODE MISSING ON CUST. FI'.        
023300     03 PIC X(55) VALUE                                                   
023400        '041TRP INFO        087 TRANSPORT INFORMATION IS MISSING'.        
023500     03 PIC X(55) VALUE                                                   
023600        '360                088 PART NO. BELONGS TO OTHER COMPAN'.        
023700     03 PIC X(55) VALUE                                                   
023800        'AAA                089 PART NO. BELONGS TO OTHER PRICE '.        
023900     03 PIC X(55) VALUE                                                   
024000        'AAA                090 PART NO. BELONGS TO OTHER FAMILY'.        
024100     03 PIC X(55) VALUE                                                   
024200        '025FAMILY          091 FAMILY MISSING                  '.        
024300     03 PIC X(55) VALUE                                                   
024400        '023IDLEVNR         092 WRONG SUPPLIER NO.              '.        
024500     03 PIC X(55) VALUE                                                   
024600        'AAA                093 WRONG ORDER SCREEN CHOSEN       '.        
024700     03 PIC X(55) VALUE                                                   
024800        'AAA                094 FORMAL ERROR IN ORDER HEAD      '.        
024900     03 PIC X(55) VALUE                                                   
025000        'AAA                095 LOGICAL ERROR IN ORDER HEAD     '.        
025100     03 PIC X(55) VALUE                                                   
025200        'AAA                096 PROFORMA RELEASED               '.        
025300     03 PIC X(55) VALUE                                                   
025400        'AAA                097 MORE THAN ONE FUNCTION SELECTED '.        
025500     03 PIC X(55) VALUE                                                   
025600        'AAA                098 PICKING LIST AND INTERVAL NOT AL'.        
025700     03 PIC X(55) VALUE                                                   
025800        'AAA                099 ORDER PRINTING                  '.        
025900     03 PIC X(55) VALUE                                                   
026000        'AAA                100 CAMPAIGN STARTED; UPDATING NOT A'.        
026100     03 PIC X(55) VALUE                                                   
026200        '001                101 UPDATING OK                     '.        
026300     03 PIC X(55) VALUE                                                   
026400        'AAA                102 CONCERN DISTRICT MARKET         '.        
026500     03 PIC X(55) VALUE                                                   
026600        'AAA                103 TO SCREEN 3201                  '.        
026700     03 PIC X(55) VALUE                                                   
026800        'AAA                104 AT LEAST ONE PART NUMBER MUST RE'.        
026900     03 PIC X(55) VALUE                                                   
027000        '011                402 MORE LINES                      '.        
027100     03 PIC X(55) VALUE                                                   
027200        '011ENTER           169 FOR MORE INFORMATION, PRESS ENTE'.        
027300     03 PIC X(55) VALUE                                                   
027400        '011*               105 FOR MORE INFORMATION, PRESS PF8 '.        
027500     03 PIC X(55) VALUE                                                   
027600        '012*               106 LAST PAGE                       '.        
027700     03 PIC X(55) VALUE                                                   
027800        'AAA                107 NO ADJUSTMENTS ENTERED          '.        
027900     03 PIC X(55) VALUE                                                   
028000        '023MARKET          108 MARKET DOES NOT EXIST           '.        
028100     03 PIC X(55) VALUE                                                   
028200        'AAA                109 TYPE OF ASSORTMENT DOES NOT EXIS'.        
028300     03 PIC X(55) VALUE                                                   
028400        '023IDFKNGRP        110 FUNCTION GROUP DOES NOT EXIST   '.        
028500     03 PIC X(55) VALUE                                                   
028600        'AAA                111 SEASONAL INDEX DOES NOT EXIST   '.        
028700     03 PIC X(55) VALUE                                                   
028800        'AAA                112 AMOUNT NOT = 100                '.        
028900     03 PIC X(55) VALUE                                                   
029000        'AAA                113 PRESS PF1 FOR MORE INFORMATION  '.        
029100     03 PIC X(55) VALUE                                                   
029200        'AAA                114 TRANSACTION CANNOT BE PROCESSED '.        
029300     03 PIC X(55) VALUE                                                   
029400        '346                115 LAST PAGE ALREADY SHOWN         '.        
029500     03 PIC X(55) VALUE                                                   
029600        'AAA                116 PUT X FOR LIST(S) TO BE RESTARTE'.        
029700     03 PIC X(55) VALUE                                                   
029800        '025BACKUP PRINTER  117 BACK-UP PRINTER MISSING         '.        
029900     03 PIC X(55) VALUE                                                   
030000        '376                118 PRINTING REQUESTED              '.        
030100     03 PIC X(55) VALUE                                                   
030200        'AAA                119 THERE IS NO LIST TO BE RESTARTED'.        
030300     03 PIC X(55) VALUE                                                   
030400        'AAA                120 ALREADY RECEIVED                '.        
030500     03 PIC X(55) VALUE                                                   
030600        'AAA                121 RECORD RECEIVED                 '.        
030700     03 PIC X(55) VALUE                                                   
030800        'AAA                122 COMPETITOR INFO EXISTS          '.        
030900     03 PIC X(55) VALUE                                                   
031000        'AAA                123 IMPOSSIBLE TO GIVE PRIORITY     '.        
031100     03 PIC X(55) VALUE                                                   
031200        'AAA                124 ORDER ALREADY PRINTED           '.        
031300     03 PIC X(55) VALUE                                                   
031400        'AAA                125 ORDER LOCKED                    '.        
031500     03 PIC X(55) VALUE                                                   
031600        'AAA                126 WORKING KOPY MAY NOT BE UPDATED '.        
031700     03 PIC X(55) VALUE                                                   
031800        'AAA                127 TO CHANGE SCREENS; PRESS PF9    '.        
031900     03 PIC X(55) VALUE                                                   
032000        '041PRICE COMP.     128 PRICE COMPARISON MISSING        '.        
032100     03 PIC X(55) VALUE                                                   
032200        '025PART LIST       129 PART LIST MISSING               '.        
032300     03 PIC X(55) VALUE                                                   
032400        '015                130 PROCESS IN PROGRESS             '.        
032500     03 PIC X(55) VALUE                                                   
032600        'AAA                131 CHOICE TREATED ON ANOTHER TERMIN'.        
032700     03 PIC X(55) VALUE                                                   
032800        'AAA                132 CODE NOT REGISTERED             '.        
032900     03 PIC X(55) VALUE                                                   
033000        'AAA                133 TERMS OF PAYMENT NOT REGISTERED '.        
033100     03 PIC X(55) VALUE                                                   
033200        'AAA                134 TERMS OF PAYMENT ALREADY REGISTE'.        
033300     03 PIC X(55) VALUE                                                   
033400        '041ROUTINE         135 ROUTINE IS MISSING              '.        
033500     03 PIC X(55) VALUE                                                   
033600        '041JOB             136 JOB IS MISSING                  '.        
033700     03 PIC X(55) VALUE                                                   
033800        '025IDKUNDR         137 CUSTOMER MISSING                '.        
033900     03 PIC X(55) VALUE                                                   
034000        '041COMPET. COMP    138 COMPETITIVE COMPANY MISSING (G17'.        
034100     03 PIC X(55) VALUE                                                   
034200        'AAA                139 PRICE COMPARISON FOR CUST. EXIST'.        
034300     03 PIC X(55) VALUE                                                   
034400        'AAA                140 NO AUTOMATIC SPLIT              '.        
034500     03 PIC X(55) VALUE                                                   
034600        'AAA                141 NO PRICE COMPARISON CUSTOMER    '.        
034700     03 PIC X(55) VALUE                                                   
034800        'AAA                142 COMPETITOR ALREADY CHOSEN FOR CU'.        
034900     03 PIC X(55) VALUE                                                   
035000        'AAA                143 TO UPDATE COMPETITOR - PRESS PF1'.        
035100     03 PIC X(55) VALUE                                                   
035200        '013                144 PRESS ENTER/PF11/PF23 FOR UPDATE'.        
035300     03 PIC X(55) VALUE                                                   
035400        '297                145 FINANCIAL CUSTOMER MISSING      '.        
035500     03 PIC X(55) VALUE                                                   
035600        '025IDGMT           146 GOODS RECEIVER MISSING          '.        
035700     03 PIC X(55) VALUE                                                   
035800        'AAA                147 TERMS OF PAYMENT MISSING, CHECK '.        
035900     03 PIC X(55) VALUE                                                   
036000        '025KDVALISO        148 CURRENCY CODE MISSING           '.        
036100     03 PIC X(55) VALUE                                                   
036200        '041NAME/ADDR       149 NAME/ADDRESS MISSING            '.        
036300     03 PIC X(55) VALUE                                                   
036400        '025IDPARTNR        150 FINANCIAL CUSTOMER DATA MISSING '.        
036500     03 PIC X(55) VALUE                                                   
036600        'AAA                151 WRONG TYPE OF CURRENCY CODE     '.        
036700     03 PIC X(55) VALUE                                                   
036800        'AAA                152 CONFIRM DELETE - PRESS PF11     '.        
036900     03 PIC X(55) VALUE                                                   
037000        '025IDGMT WDB3      153 GOODS RECEIVER MISSING ON WDB3  '.        
037100     03 PIC X(55) VALUE                                                   
037200        '025IDGMT WDB5      154 GOODS RECEIVER MISSING ON WDB5  '.        
037300     03 PIC X(55) VALUE                                                   
037400        'AAA                155 REQ. RETAIL UPDATING STARTED    '.        
037500     03 PIC X(55) VALUE                                                   
037600        'AAA                156 ONLY CMD X FOR BO/TPO LINE      '.        
037700     03 PIC X(55) VALUE                                                   
037800        'AAA                157 COND. FOR CAMPAIGN NOT FULFILLED'.        
037900     03 PIC X(55) VALUE                                                   
038000        'AAA                158 COMPETITOR  S PART DELETED      '.        
038100     03 PIC X(55) VALUE                                                   
038200        'AAA                159 SEASON EXISTS IN CDC            '.        
038300     03 PIC X(55) VALUE                                                   
038400        'AAA                160 SEASON EXISTS IN DC             '.        
038500     03 PIC X(55) VALUE                                                   
038600        'AAA                161 SEASON EXISTS IN CDC AND DC     '.        
038700     03 PIC X(55) VALUE                                                   
038800        'AAA                162 TREND MAKES PB < 0              '.        
038900     03 PIC X(55) VALUE                                                   
039000        'AAA                163 VALID PB = 0                    '.        
039100     03 PIC X(55) VALUE                                                   
039200        '041RTN/JOB         164 ROUTINE OR JOB IS MISSING       '.        
039300     03 PIC X(55) VALUE                                                   
039400        '358                165 IR ALREADY REGISTERED           '.        
039500     03 PIC X(55) VALUE                                                   
039600        '041ADGMT           166 GOODS ADDRESS MISSING           '.        
039700     03 PIC X(55) VALUE                                                   
039800        'AAA                167 NOTHING AT ALL PRINTED          '.        
039900     03 PIC X(55) VALUE                                                   
040000        '379PF11            168 PRESS PF11 FOR PRINTING         '.        
040100     03 PIC X(55) VALUE                                                   
040200        'AAA                170 PRICE COMPARISON NOT RECEIVED   '.        
040300     03 PIC X(55) VALUE                                                   
040400        '041PRICE INFO      171 PRICE INFO FOR CUSTOMER MISSING '.        
040500     03 PIC X(55) VALUE                                                   
040600        'AAA                172 PRICE INFO FOR CUSTOMER DELETED '.        
040700     03 PIC X(55) VALUE                                                   
040800        'AAA                173 MORE THAN 999 LINES, RE-START   '.        
040900     03 PIC X(55) VALUE                                                   
041000        'AAA                174 NOT MORE THAN ONE FCNGROUP/TABLE'.        
041100     03 PIC X(55) VALUE                                                   
041200        'AAA                175 PART NO. ALREADY EXISTS IN TABLE'.        
041300     03 PIC X(55) VALUE                                                   
041400        '361                176 VER. NO. EXCEEDED               '.        
041500     03 PIC X(55) VALUE                                                   
041600        'AAA                177 CLEAR PART HISTORY              '.        
041700     03 PIC X(55) VALUE                                                   
041800        '367                178 BATCH ON CASE LEVEL             '.        
041900     03 PIC X(55) VALUE                                                   
042000        'AAA                179 DOES NOT MATCH ADV.             '.        
042100     03 PIC X(55) VALUE                                                   
042200        '374                180 PLEASE USE SCREEN 6122          '.        
042300     03 PIC X(55) VALUE                                                   
042400        '330                181 WRONG QUANTITY                  '.        
042500     03 PIC X(55) VALUE                                                   
042600        '354                182 MISCELLANEOUS CASE              '.        
042700     03 PIC X(55) VALUE                                                   
042800        '370                183 KIT AND PRIO TOGETHER NOT ALLOWE'.        
042900     03 PIC X(55) VALUE                                                   
043000        '337                184 ERROR ON LINE 1                 '.        
043100     03 PIC X(55) VALUE                                                   
043200        '334                185 KIT MARKED CASE                 '.        
043300     03 PIC X(55) VALUE                                                   
043400        '368                186 BATCH NOT ON CASE LEVEL         '.        
043500     03 PIC X(55) VALUE                                                   
043600        '178                187 UNPACKED CASES EXIST            '.        
043700     03 PIC X(55) VALUE                                                   
043800        'AAA                188 NOT MIXED CASE                  '.        
043900     03 PIC X(55) VALUE                                                   
044000        '357                189 QUALITY ERROR                   '.        
044100     03 PIC X(55) VALUE                                                   
044200        'AAA                190 WHOLE LOT NOT ON LOCATION       '.        
044300     03 PIC X(55) VALUE                                                   
044400        'AAA                191 PLEASE USE SCREEN 6141          '.        
044500     03 PIC X(55) VALUE                                                   
044600        '382                192 DEVIATIONS; PLEASE USE SCREEN 61'.        
044700     03 PIC X(55) VALUE                                                   
044800        '383                193 ONLY ONE KIND OF LABELS/MIN. LAB'.        
044900     03 PIC X(55) VALUE                                                   
045000        'AAA                194 WRONG INPUT FIELD               '.        
045100     03 PIC X(55) VALUE                                                   
045200        '384                195 TOO MANY LABELS                 '.        
045300     03 PIC X(55) VALUE                                                   
045400        '375                196 TROLLEY HAS LOC/ADDRESS         '.        
045500     03 PIC X(55) VALUE                                                   
045600        '371                197 SURPLUS DELIVERY                '.        
045700     03 PIC X(55) VALUE                                                   
045800        'AAA                198 PREL PRICES EXISTS! PRINT/RELEAS'.        
045900     03 PIC X(55) VALUE                                                   
046000        'AAA                199 MORE LABELS CAN BE PRINTED      '.        
046100     03 PIC X(55) VALUE                                                   
046200        '041QUEUE           200 QUEUE MISSING                   '.        
046300     03 PIC X(55) VALUE                                                   
046400        '030IDAVINR         201 ADVICE NOTE EXISTS, OK ?        '.        
046500     03 PIC X(55) VALUE                                                   
046600        'AAA                202 PRINT-OUT STARTED               '.        
046700     03 PIC X(55) VALUE                                                   
046800        '372                203 PRESS PF8 FOR NEXT BATCH        '.        
046900     03 PIC X(55) VALUE                                                   
047000        '365                204 PAPERS FOR GOODS NEEDED         '.        
047100     03 PIC X(55) VALUE                                                   
047200        'AAA                205 ALREADY PRINTED                 '.        
047300     03 PIC X(55) VALUE                                                   
047400        '013PF23            206 PRESS PF23 TO UPDATE            '.        
047500     03 PIC X(55) VALUE                                                   
047600        'AAA                207 NEW FGRP NOT THE SAME AS IN TABL'.        
047700     03 PIC X(55) VALUE                                                   
047800        '387                208 APPROVED QTY NOT EQUAL TO ORIGIN'.        
047900     03 PIC X(55) VALUE                                                   
048000        '388                209 CORE NO. ALREADY IN REPORT      '.        
048100     03 PIC X(55) VALUE                                                   
048200        '394                20A WARNING, OLD VIPS USER          '.        
048300     03 PIC X(55) VALUE                                                   
048400        '389*               210 REPORT NOT REGISTERED           '.        
048500     03 PIC X(55) VALUE                                                   
048600        '023IDARTNR-OBJ     211 THIS IS NOT AN EXCHANGE CORE NUM'.        
048700     03 PIC X(55) VALUE                                                   
048800        '390*               079 WRONG STATUS                    '.        
048900     03 PIC X(55) VALUE                                                   
049000        'AAA                213 FIN.CUSTOMER STOPPED            '.        
049100     03 PIC X(55) VALUE                                                   
049200        '041PART HISTORY    214 PART HISTORY MISSING            '.        
049300     03 PIC X(55) VALUE                                                   
049400        'AAA                215 RECEIVING CONTROL NOT READY     '.        
049500     03 PIC X(55) VALUE                                                   
049600        '363                216 (OUTCOME) HAS NOT BEEN TESTED   '.        
049700     03 PIC X(55) VALUE                                                   
049800        'AAA                217 SPECIAL COSTS - SEE REG. FILE 57'.        
049900     03 PIC X(55) VALUE                                                   
050000        'AAA                218 SEARCH THE TABLE BEFORE UPDATING'.        
050100     03 PIC X(55) VALUE                                                   
050200        'AAA                219 LINE QUANTITY WILL BE > 99      '.        
050300     03 PIC X(55) VALUE                                                   
050400        '223                220 THIS PART IS SUPERSEDED         '.        
050500     03 PIC X(55) VALUE                                                   
050600        '041SAMPLE PLAN     221 SAMPLE PLAN MISSING             '.        
050700     03 PIC X(55) VALUE                                                   
050800        'AAA                222 DEST PLACE MISSING              '.        
050900     03 PIC X(55) VALUE                                                   
051000        '025IDVAT           223 VAT NO. MISSING                 '.        
051100     03 PIC X(55) VALUE                                                   
051200        '351                224 QUANTITY IR. EXIST,    NO.      '.        
051300     03 PIC X(55) VALUE                                                   
051400        '352                225 TECHNICAL IR. EXIST,   NO.      '.        
051500     03 PIC X(55) VALUE                                                   
051600        'AAA                226 ENTER LOT NO.                   '.        
051700     03 PIC X(55) VALUE                                                   
051800        '373                227 NO DEVIATION ACC. TYPE 3 OR 77  '.        
051900     03 PIC X(55) VALUE                                                   
052000        '355                228 QUARANTINE; BACK TO CONTROL     '.        
052100     03 PIC X(55) VALUE                                                   
052200        'AAA                229 CASE HAS NOT BEEN REPORTED      '.        
052300     03 PIC X(55) VALUE                                                   
052400        'AAA                230 CASES LEFT ON TROLLEY/LOC       '.        
052500     03 PIC X(55) VALUE                                                   
052600        'AAA                231 PF4 AND EMPTY REGISTRATION LINE '.        
052700     03 PIC X(55) VALUE                                                   
052800        '335                232 IR MUST BE MANUALLY CHANGED     '.        
052900     03 PIC X(55) VALUE                                                   
053000        'AAA                233 WRONG SUPPL. NO./CASE NO.       '.        
053100     03 PIC X(55) VALUE                                                   
053200        'AAA                234 INSPECTION CODE MISSING         '.        
053300     03 PIC X(55) VALUE                                                   
053400        'AAA                235 QUAL.ASS./I.S.T. APPROVED UPDATE'.        
053500     03 PIC X(55) VALUE                                                   
053600        'AAA                236 PRICE AREA MISSING              '.        
053700     03 PIC X(55) VALUE                                                   
053800        'AAA                237 TRANSFER DISCOUNT MISSING       '.        
053900     03 PIC X(55) VALUE                                                   
054000        '401*               238 INVALID COMBINATION OF VALUES   '.        
054100     03 PIC X(55) VALUE                                                   
054200        'AAA                239 SECTION MISSING                 '.        
054300     03 PIC X(55) VALUE                                                   
054400        'AAA                240 START VALUE GREATER THAN END VAL'.        
054500     03 PIC X(55) VALUE                                                   
054600        'AAA                241 VARIANTS OF SAME TYPE NOT VALID '.        
054700     03 PIC X(55) VALUE                                                   
054800        'AAA                242 SHOULD T-BLOCK HEADING BE UPDATE'.        
054900     03 PIC X(55) VALUE                                                   
055000        '366                243 ONLY ONE MISCELLANEOUS CASE     '.        
055100     03 PIC X(55) VALUE                                                   
055200        'AAA                244 PART NOT ACTIVE                 '.        
055300     03 PIC X(55) VALUE                                                   
055400        'AAA                245 LINE ALREADY EXISTS. UPDATE NOT '.        
055500     03 PIC X(55) VALUE                                                   
055600        'AAA                246 QUEUED TO PRINTER               '.        
055700     03 PIC X(55) VALUE                                                   
055800        'AAA                247 RETURNORDER EXISTS              '.        
055900     03 PIC X(55) VALUE                                                   
056000        'AAA                248 NOT AVAILABLE FOR ORDERS        '.        
056100     03 PIC X(55) VALUE                                                   
056200        'AAA                249 DISPATCHER RESCHEDULE           '.        
056300     03 PIC X(55) VALUE                                                   
056400        'AAA                250 CASE NOT ALLOWED IN MIXED CASE  '.        
056500     03 PIC X(55) VALUE                                                   
056600        'AAA                251 CASE ALLREADY EXISTS IN MIXED CA'.        
056700     03 PIC X(55) VALUE                                                   
056800        'AAA                252 CASE HAS WRONG TRANSPORT NO.    '.        
056900     03 PIC X(55) VALUE                                                   
057000        '393                253 APPROVED QTY MAY NOT BE LESS THE'.        
057100     03 PIC X(55) VALUE                                                   
057200        'AAA                254 CORE ALREADY EXISTS WHIT ANOTHER'.        
057300     03 PIC X(55) VALUE                                                   
057400        'AAA                255 NO DANGEROUS CARGO IN MIXED CASE'.        
057500     03 PIC X(55) VALUE                                                   
057600        'AAA                256 DISTRICT NOT APPROVED FOR MIXED '.        
057700     03 PIC X(55) VALUE                                                   
057800        'AAA                257 MORE THEN 200 LINES. USE SCREEN '.        
057900     03 PIC X(55) VALUE                                                   
058000        'AAA                258 PART EXPIRE                     '.        
058100     03 PIC X(55) VALUE                                                   
058200        'AAA                259 REPLACING PART                  '.        
058300     03 PIC X(55) VALUE                                                   
058400        'AAA                260 LOCATION ADDED                  '.        
058500     03 PIC X(55) VALUE                                                   
058600        'AAA                261 LOCATION CHANGED                '.        
058700     03 PIC X(55) VALUE                                                   
058800        'AAA                262 LOCATION DELETED                '.        
058900     03 PIC X(55) VALUE                                                   
059000        'AAA                263 LOCATION EXISTS                 '.        
059100     03 PIC X(55) VALUE                                                   
059200        'AAA                264 IR HAS BEEN SENT TO SUPPLIER    '.        
059300     03 PIC X(55) VALUE                                                   
059400        'AAA                265 FIRST ACTIVATE PUB-CODES ON SCR '.        
059500     03 PIC X(55) VALUE                                                   
059600        'AAA                266 ALL LANGUAGES ALREADY TRANSLATED'.        
059700     03 PIC X(55) VALUE                                                   
059800        'AAA                267 ORDERING REGISTER IS EMPTIED    '.        
059900     03 PIC X(55) VALUE                                                   
060000        'AAA                268 BOTH SWEDISH AND ENGLISH IS MISS'.        
060100     03 PIC X(55) VALUE                                                   
060200        'AAA                269 MAIN EVENT MISSING              '.        
060300     03 PIC X(55) VALUE                                                   
060400        'AAA                270 SUB EVENT MISSING               '.        
060500     03 PIC X(55) VALUE                                                   
060600        'AAA                271 LEVEL MISSING                   '.        
060700     03 PIC X(55) VALUE                                                   
060800        'AAA                272 CLIENT/REC. TYPE MISSING        '.        
060900     03 PIC X(55) VALUE                                                   
061000        '025IDLEVNR         273 SUPPLIER IS MISSING             '.        
061100     03 PIC X(55) VALUE                                                   
061200        'AAA                274 GROUP ALREADY EXISTS            '.        
061300     03 PIC X(55) VALUE                                                   
061400        '025GROUP           275 GROUP IS MISSING                '.        
061500     03 PIC X(55) VALUE                                                   
061600        'AAA                276 PARTS LEFT IN GROUP             '.        
061700     03 PIC X(55) VALUE                                                   
061800        '025SUPPL GROUP     277 SUPPLIER GROUP IS MISSING       '.        
061900     03 PIC X(55) VALUE                                                   
062000        'AAA                278 PART ALREADY EXISTS             '.        
062100     03 PIC X(55) VALUE                                                   
062200        'AAA                279 PART MISSING IN GROUP           '.        
062300     03 PIC X(55) VALUE                                                   
062400        '025IDPRODNR        280 PROD NO MISSING                 '.        
062500     03 PIC X(55) VALUE                                                   
062600        'AAA                281 INVOICE ALREADY COMPLETED       '.        
062700     03 PIC X(55) VALUE                                                   
062800        'AAA                282 PUB CODE WRONG. GO TO 1514      '.        
062900     03 PIC X(55) VALUE                                                   
063000        'AAA                283 RETURN TO PAGE 1, PRESS ENTER   '.        
063100     03 PIC X(55) VALUE                                                   
063200        '353                284 SPLITTED ADVICED QTY - PF23 FOR '.        
063300     03 PIC X(55) VALUE                                                   
063400        '336                285 CALLS MISSING                   '.        
063500     03 PIC X(55) VALUE                                                   
063600        'AAA                286 SPLITTED ADVICED QTY            '.        
063700     03 PIC X(55) VALUE                                                   
063800        '046CHOICES         287 CONFLICTING CHOICES             '.        
063900     03 PIC X(55) VALUE                                                   
064000        'AAA                288 MAX VALUE EXCEEDED FOR THIS FIEL'.        
064100     03 PIC X(55) VALUE                                                   
064200        '364                289 IR NOT CREATED                  '.        
064300     03 PIC X(55) VALUE                                                   
064400        'AAA                290 UPD. PRICE FOR SUPPL.  THEN CONT'.        
064500     03 PIC X(55) VALUE                                                   
064600        'AAA                291 MATRIX CONFLICT                 '.        
064700     03 PIC X(55) VALUE                                                   
064800        'AAA                292 MATRIX CONFLICT-DANGEROUS GOODS '.        
064900     03 PIC X(55) VALUE                                                   
065000        '378                293 IR REMAINS AS ADM REP           '.        
065100     03 PIC X(55) VALUE                                                   
065200        '351                294 QTY IR EXISTS; ADM IR NOT CREATE'.        
065300     03 PIC X(55) VALUE                                                   
065400        '260                301 PRICE IS MISSING                '.        
065500     03 PIC X(55) VALUE                                                   
065600        '330                302 WRONG QUANTITY                  '.        
065700     03 PIC X(55) VALUE                                                   
065800        'AAA                303 PART INFORMATION MISSING        '.        
065900     03 PIC X(55) VALUE                                                   
066000        '345                304 WRONG COMMAND CODE              '.        
066100     03 PIC X(55) VALUE                                                   
066200        'AAA                305 PART MISSING IN DC              '.        
066300     03 PIC X(55) VALUE                                                   
066400        '349                306 DIRECTLY DELIVERED PART         '.        
066500     03 PIC X(55) VALUE                                                   
066600        'AAA                307 NOT APPROVED FOR REFILLING      '.        
066700     03 PIC X(55) VALUE                                                   
066800        'AAA                308 ONLY ORDERED PCS ARE REG.       '.        
066900     03 PIC X(55) VALUE                                                   
067000        'AAA                309 USE S TO SELECT A LINE          '.        
067100     03 PIC X(55) VALUE                                                   
067200        'AAA                310 ALREADY LOADED                  '.        
067300     03 PIC X(55) VALUE                                                   
067400        'AAA                311 NOT LOADED                      '.        
067500     03 PIC X(55) VALUE                                                   
067600        'AAA                312 REPORTING STARTED               '.        
067700     03 PIC X(55) VALUE                                                   
067800        'AAA                313 REPORTING DONE                  '.        
067900     03 PIC X(55) VALUE                                                   
068000        'AAA                314 USE SCREEN 4315. ORDER > 200 LIN'.        
068100     03 PIC X(55) VALUE                                                   
068200        'AAA                315 ORDER NOT PACKED. USE SCREEN 431'.        
068300     03 PIC X(55) VALUE                                                   
068400        'AAA                316 PART MISSING IN BUFFER REG.     '.        
068500     03 PIC X(55) VALUE                                                   
068600        'AAA                317 MIXED CASE ON OTHER TRANSPORT   '.        
068700     03 PIC X(55) VALUE                                                   
068800        'AAA                318 MIXED CASE HAS NO TRANSPORT     '.        
068900     03 PIC X(55) VALUE                                                   
069000        '025IDFAKT          320 INVOICE IS MISSING              '.        
069100     03 PIC X(55) VALUE                                                   
069200        '413                321 THE PART IS SCRAPED             '.        
069300     03 PIC X(55) VALUE                                                   
069400        '414                322 BASIC STOCK TO LOW              '.        
069500     03 PIC X(55) VALUE                                                   
069600        '415                439 WEIGHT MAY BE ADJUSTED          '.        
069700     03 PIC X(55) VALUE                                                   
069800        'AAA                323 USE SCREEN 4315 FOR THIS ORDER  '.        
069900     03 PIC X(55) VALUE                                                   
070000        'AAA                324 PRC9998 CAN NOT BE PRINTED MANUL'.        
070100     03 PIC X(55) VALUE                                                   
070200        'AAA                325 ORDER BELONG TO DIR.SUP. DONT PA'.        
070300     03 PIC X(55) VALUE                                                   
070400        'AAA                326 WRONG DISPOSITION CODE          '.        
070500     03 PIC X(55) VALUE                                                   
070600        'AAA                327 HANDLINGCOSTCODE NOT UPDATED    '.        
070700     03 PIC X(55) VALUE                                                   
070800        'AAA                328 UNALLOWED COMB. DISPCODE-RETURN '.        
070900     03 PIC X(55) VALUE                                                   
071000        'AAA                329 UNALLOWED COMB. DISPCODE-ADJUST '.        
071100     03 PIC X(55) VALUE                                                   
071200        'AAA                330 UNALLOWED COMB. DISPCODE-SCRAP  '.        
071300     03 PIC X(55) VALUE                                                   
071400        'AAA                331 PURCH./MATERIALCONTR.NAME/NO MIS'.        
071500     03 PIC X(55) VALUE                                                   
071600        'AAA                332 INFORMATION MISSING ON SCREEN 47'.        
071700     03 PIC X(55) VALUE                                                   
071800        'AAA                333 ALREADY CANCELLED, NO UPDATE    '.        
071900     03 PIC X(55) VALUE                                                   
072000        'AAA                334 SERIEL NUMBER MISSING           '.        
072100     03 PIC X(55) VALUE                                                   
072200        'AAA                335 MODEL IS MISSING PART NO.       '.        
072300     03 PIC X(55) VALUE                                                   
072400        'AAA                336 DO NOT MOVE SPACE TO FIELD      '.        
072500     03 PIC X(55) VALUE                                                   
072600        'AAA                337 NOT BOTH CASE CODE AND CASE DIME'.        
072700     03 PIC X(55) VALUE                                                   
072800        'AAA                338 PRC NOT PERMITTED FOR LDC.      '.        
072900     03 PIC X(55) VALUE                                                   
073000        'AAA                339 NOT ALLOWED TO CHANGE ATTENTION '.        
073100     03 PIC X(55) VALUE                                                   
073200        'AAA                340 FAX/MAIL MISSING                '.        
073300     03 PIC X(55) VALUE                                                   
073400        'AAA                341 SHIPMENT MISSING                '.        
073500     03 PIC X(55) VALUE                                                   
073600        'AAA                342 CONNECTED TO SPECIALCONTROL     '.        
073700     03 PIC X(55) VALUE                                                   
073800        'AAA                343 PRICE IS MISSING.  PROCESS ABORT'.        
073900     03 PIC X(55) VALUE                                                   
074000        'AAA                344 SUPPLIER ONLY VALID AT "STOCK UP'.        
074100     03 PIC X(55) VALUE                                                   
074200        'AAA                345 RETURN OK NOT REGISTERED        '.        
074300     03 PIC X(55) VALUE                                                   
074400        'AAA                346 ACCOUNT MISSING IN R3           '.        
074500     03 PIC X(55) VALUE                                                   
074600        'AAA                347 ANALYSIS-NO MISSING IN R3       '.        
074700     03 PIC X(55) VALUE                                                   
074800        'AAA                348 COSTCENTER MISSING IN R3        '.        
074900     03 PIC X(55) VALUE                                                   
075000        'AAA                349 DEVIATION VALUE TO HIGH         '.        
075100     03 PIC X(55) VALUE                                                   
075200        'AAA                350 ACCOUNT MUST BE REGISTERED      '.        
075300     03 PIC X(55) VALUE                                                   
075400        'AAA                351 COSTCENTER NOT ALLOWED          '.        
075500     03 PIC X(55) VALUE                                                   
075600        'AAA                352 ANALYSIS-NO NOT ALLOWED         '.        
075700     03 PIC X(55) VALUE                                                   
075800        'AAA                353 ANALYSIS-NO OR COSTCENTER MUST B'.        
075900     03 PIC X(55) VALUE                                                   
076000        'AAA                352 ANALYSIS-NO NOT ALLOWED         '.        
076100     03 PIC X(55) VALUE                                                   
076200        '395                354 DATABASE UNAVAILABLE            '.        
076300     03 PIC X(55) VALUE                                                   
076400        '396                355 TRY LATER OR NOTIFY IT CONTACT  '.        
076500     03 PIC X(55) VALUE                                                   
076600        '397                356 CHECK MISSING PARTS             '.        
076700     03 PIC X(55) VALUE                                                   
076800        '398                357 REPORT NOT FINISHED             '.        
076900     03 PIC X(55) VALUE                                                   
077000        '025IDARTNR-OBJ     358 EXCHANGE OBJECT MISSING         '.        
077100     03 PIC X(55) VALUE                                                   
077200        '022*               401 WRONG KEY                       '.        
077300     03 PIC X(55) VALUE                                                   
077400        'AAA                403 BACKORDER MISSING               '.        
077500     03 PIC X(55) VALUE                                                   
077600        '001                404 UPDATED                         '.        
077700     03 PIC X(55) VALUE                                                   
077800        '00A                405 USER NOT AUTHORIZED             '.        
077900     03 PIC X(55) VALUE                                                   
078000        '041PRIORITY        406 PRIORITY MISSING                '.        
078100     03 PIC X(55) VALUE                                                   
078200        'AAA                407 PRESS PF11 TO UPDATE            '.        
078300     03 PIC X(55) VALUE                                                   
078400        '024PRIORITY        408 PRIORITY NOT NUMERIC            '.        
078500     03 PIC X(55) VALUE                                                   
078600        '020                409 HIGHLIGHT FIELDS WRONG          '.        
078700     03 PIC X(55) VALUE                                                   
078800        '010                410 THIS IS THE FIRST PAGE          '.        
078900     03 PIC X(55) VALUE                                                   
079000        'AAA                411 MAX 25 DIFFERENT PRIORITIES     '.        
079100     03 PIC X(55) VALUE                                                   
079200        '025IDDISTR-IDKUNDNR412 DISTR/CUST. MISSING ON CUST. FIL'.        
079300     03 PIC X(55) VALUE                                                   
079400        '041INF             413 INFORMATION MISSING             '.        
079500     03 PIC X(55) VALUE                                                   
079600        '004                414 NO CHANGE, NO UPDATE DONE       '.        
079700     03 PIC X(55) VALUE                                                   
079800        'AAA                415 ORDER INFORMATION DELETED       '.        
079900     03 PIC X(55) VALUE                                                   
080000        '023???             416 WRONG SELECTION CODE            '.        
080100     03 PIC X(55) VALUE                                                   
080200        'AAA                417 ORDER HEADING MISSING           '.        
080300     03 PIC X(55) VALUE                                                   
080400        'AAA                418 DAY NOT REGISTERED              '.        
080500     03 PIC X(55) VALUE                                                   
080600        'AAA                419 SPLIT OF QTY BULK PACK NOT ALLOW'.        
080700     03 PIC X(55) VALUE                                                   
080800        'AAA                420 CUSTOMER NO. MUST BE REGISTERED '.        
080900     03 PIC X(55) VALUE                                                   
081000        'AAA                421 BO AVAILABILITY MISSING         '.        
081100     03 PIC X(55) VALUE                                                   
081200        'AAA                422 DELETE OF KIT LINE NOT ALLOWED  '.        
081300     03 PIC X(55) VALUE                                                   
081400        'AAA                423 WAREHOUSE NOT REGISTERED        '.        
081500     03 PIC X(55) VALUE                                                   
081600        '386                434 REFILL PART                     '.        
081700     03 PIC X(55) VALUE                                                   
081800        '023IDDC            440 WRONG DC                        '.        
081900     03 PIC X(55) VALUE                                                   
082000        '399*               490 CASE WITHOUT LINES              '.        
082100     03 PIC X(55) VALUE                                                   
082200        'AAA                491 ERROR IN SERIAL NUMBER          '.        
082300     03 PIC X(55) VALUE                                                   
082400        '023                492 INVALID VALUE                   '.        
082500     03 PIC X(55) VALUE                                                   
082600        'AAA                493 VALUE TOO LONG                  '.        
082700     03 PIC X(55) VALUE                                                   
082800        '024IDPRC           494 PRC NOT NUMERIC                 '.        
082900     03 PIC X(55) VALUE                                                   
083000        'AAA                495 WRONG MESSAGE RECORD TYPE       '.        
083100     03 PIC X(55) VALUE                                                   
083200        'AAA                496 WRONG SERIAL NUMBER             '.        
083300     03 PIC X(55) VALUE                                                   
083400        'AAA                497 WRONG MESSAGE TYPE              '.        
083500     03 PIC X(55) VALUE                                                   
083600        'AAA                498 MESSAGE IS ZERO                 '.        
083700     03 PIC X(55) VALUE                                                   
083800        'AAA                499 INVALID MESSAGE STRUCTURE       '.        
083900     03 PIC X(55) VALUE                                                   
084000        '403                500 CASE REPORTED, NOT DOWN PLACE:  '.        
084100     03 PIC X(55) VALUE                                                   
084200        '405                501 NEW CASE CREATED                '.        
084300     03 PIC X(55) VALUE                                                   
084400        '406                502 INVOICE WITHOUT ANY PARTS       '.        
084500     03 PIC X(55) VALUE                                                   
084600        '126VKART           503 WEIGHT MUST NOT BE ZERO         '.        
084700     03 PIC X(55) VALUE                                                   
084800        '407VKORDBTO-FAKT   504 INVOICE WEIGHT HAS BEEN UPDATED '.        
084900     03 PIC X(55) VALUE                                                   
085000        '407VKARTBTO-KOLLI  505 CASE WEIGHT HAS BEEN UPDATED    '.        
085100     03 PIC X(55) VALUE                                                   
085200        '408                506 DELETING ALL CASES IS NOT ALLOWE'.        
085300     03 PIC X(55) VALUE                                                   
085400        '409                507 CASE DELETED. PRESS ENTER FOR LA'.        
085500     03 PIC X(55) VALUE                                                   
085600        '410                508 PART REMOVED FROM THE CASE      '.        
085700     03 PIC X(55) VALUE                                                   
085800        '411                509 PART ADDED TO THE CASE          '.        
085900     03 PIC X(55) VALUE                                                   
086000        '412                510 PROFORMA PRINTING. PRESS ENTER  '.        
086100                                                                          
086200     03 PIC X(55) VALUE                                                   
086300        '400                524 CASE INVOICED OR RELEASED FOR LO'.        
086400     03 PIC X(55) VALUE                                                   
086500        '402                527 ORDER STATUS/CONTENT HAS CHANGED'.        
086600     03 PIC X(55) VALUE                                                   
086700        'AAA                601 NOT AUTHORIZED TO SCRAP         '.        
086800     03 PIC X(55) VALUE                                                   
086900        'AAA                602 NOT AUTHORIZED TO ATTEST        '.        
087000     03 PIC X(55) VALUE                                                   
087100        'AAA                603 LINES NEED TO BE APPROVED BY NEX'.        
087200     03 PIC X(55) VALUE                                                   
087300        'AAA                604 NOT AUTHORIZED TO APPROVE DISCR '.        
087400     03 PIC X(55) VALUE                                                   
087500        '377                605 PRIMARY/SPECIAL CONTROL NOT DONE'.        
087600     03 PIC X(55) VALUE                                                   
087700        'AAA                606 MISSING PACKING TYP ON 6157     '.        
087800     03 PIC X(55) VALUE                                                   
087900        '025IDORDNNR        701 ORDER MISSING                   '.        
088000     03 PIC X(55) VALUE                                                   
088100        'AAA                702 PACKING PER ORDER IN PROGRESS   '.        
088200     03 PIC X(55) VALUE                                                   
088300        'AAA                703 ORDER ALREADY REPORTED          '.        
088400     03 PIC X(55) VALUE                                                   
088500        'AAA                704 ONLY SPLIT WITH INTERVAL ALLOWED'.        
088600     03 PIC X(55) VALUE                                                   
088700        '025ADLAGOMR        705 AREA MISSING                    '.        
088800     03 PIC X(55) VALUE                                                   
088900        '025ADPLATS         706 LOCATION MISSING                '.        
089000     03 PIC X(55) VALUE                                                   
089100        'AAA                707 INTERV. OR PART OF INTERV. ALREA'.        
089200     03 PIC X(55) VALUE                                                   
089300        'AAA                708 ORDER MISSING OR READY          '.        
089400     03 PIC X(55) VALUE                                                   
089500        'AAA                709 ORDER NOT STARTED               '.        
089600     03 PIC X(55) VALUE                                                   
089700        'AAA                710 ORDER REGISTERED                '.        
089800     03 PIC X(55) VALUE                                                   
089900        'AAA                711 WRONG PACKER                    '.        
090000     03 PIC X(55) VALUE                                                   
090100        'AAA                712 NO LINES FOR THIS PACKER        '.        
090200     03 PIC X(55) VALUE                                                   
090300        'AAA                713 ORDER PART(S) REPORTED          '.        
090400     03 PIC X(55) VALUE                                                   
090500        '404                714 REPORTING IN PROGRESS           '.        
090600     03 PIC X(55) VALUE                                                   
090700        'AAA                715 NEW PACKER AND CANCEL NOT ALLOWE'.        
090800     03 PIC X(55) VALUE                                                   
090900        'AAA                716 ORDER HAS NOT BEEN SPLIT        '.        
091000     03 PIC X(55) VALUE                                                   
091100        'AAA                717 CASE HAS NOT BEEN REPORTED      '.        
091200     03 PIC X(55) VALUE                                                   
091300        'AAA                718 ORDER HAS BEEN REPORTED         '.        
091400     03 PIC X(55) VALUE                                                   
091500        'AAA                719 WRONG PACKER                    '.        
091600     03 PIC X(55) VALUE                                                   
091700        'AAA                720 PACKER  S ORDER PART FINISHED   '.        
091800     03 PIC X(55) VALUE                                                   
091900        '149*               721 CASE ALREADY REPORTED           '.        
092000     03 PIC X(55) VALUE                                                   
092100        'AAA                722 PACKER AND INTERVAL DO NOT MATCH'.        
092200     03 PIC X(55) VALUE                                                   
092300        'AAA                723 INTERV. OR PART OF INTERV. ALREA'.        
092400     03 PIC X(55) VALUE                                                   
092500        '168                724 ZERO NOT ALLOWED                '.        
092600     03 PIC X(55) VALUE                                                   
092700        '298                725 QUANTITY TOO BIG                '.        
092800     03 PIC X(55) VALUE                                                   
092900        '041KDKOLLI         726 CASE CODE MISSING               '.        
093000     03 PIC X(55) VALUE                                                   
093100        'AAA                727 ENTER GROSS WEIGHT              '.        
093200     03 PIC X(55) VALUE                                                   
093300        '226                728 ADD CASE INFORMATION            '.        
093400     03 PIC X(55) VALUE                                                   
093500        '023ADDRESS         729 WRONG ADDRESS                   '.        
093600     03 PIC X(55) VALUE                                                   
093700        '385*               730 ERROR IN ADDRESS HANDLING       '.        
093800     03 PIC X(55) VALUE                                                   
093900        'AAA                731 CASE AND INTERVAL TOGETHER NOT A'.        
094000     03 PIC X(55) VALUE                                                   
094100        'AAA                732 ERROR IN CASE INTERVAL          '.        
094200     03 PIC X(55) VALUE                                                   
094300        'AAA                733 ONLY ONE INTERVAL ALLOWED       '.        
094400     03 PIC X(55) VALUE                                                   
094500        'AAA                734 ONLY ONE LINE IN INTERVAL OK    '.        
094600     03 PIC X(55) VALUE                                                   
094700        'AAA                735 LINE QTY AND CASES NOT EVEN     '.        
094800     03 PIC X(55) VALUE                                                   
094900        'AAA                736 CASE IN INTERVAL ALREADY REPORTE'.        
095000     03 PIC X(55) VALUE                                                   
095100        'AAA                737 CASE INVOICED                   '.        
095200     03 PIC X(55) VALUE                                                   
095300        'AAA                738 WRONG INTERVAL INFORMATION      '.        
095400     03 PIC X(55) VALUE                                                   
095500        'AAA                739 ENTER BOTH LINE FROM AND LINE TO'.        
095600     03 PIC X(55) VALUE                                                   
095700        'AAA                740 QTY ONLY ALLOWED FOR SINGLE INTE'.        
095800     03 PIC X(55) VALUE                                                   
095900        'AAA                741 QUANTITY MAY NOT BE INCREASED   '.        
096000     03 PIC X(55) VALUE                                                   
096100        'AAA                742 CASE MAY NOT BE COMPLETELY EMPTY'.        
096200     03 PIC X(55) VALUE                                                   
096300        'AAA                743 ENTER LINE NUMBER AND ORIGIN    '.        
096400     03 PIC X(55) VALUE                                                   
096500        'AAA                744 BOTH LINE NO. AND ORIGIN REQUIRE'.        
096600     03 PIC X(55) VALUE                                                   
096700        'AAA                745 HIGHLIGHT LINES DO NOT BELONG TO'.        
096800     03 PIC X(55) VALUE                                                   
096900        'AAA                746 HIGHLIGHT LINES NOT READY       '.        
097000     03 PIC X(55) VALUE                                                   
097100        '023IDDISTR         747 WRONG DISTRICT                  '.        
097200     03 PIC X(55) VALUE                                                   
097300        '020                748 HIGHLIGHT FIELDS WRONG          '.        
097400     03 PIC X(55) VALUE                                                   
097500        '043*               749 WRONG KEY                       '.        
097600     03 PIC X(55) VALUE                                                   
097700        'AAA                750 ENTER VIA ANOTHER SCREEN        '.        
097800     03 PIC X(55) VALUE                                                   
097900        'AAA                751 WRONG LINES                     '.        
098000     03 PIC X(55) VALUE                                                   
098100        '003                752 LINE DELETED                    '.        
098200     03 PIC X(55) VALUE                                                   
098300        'AAA                753 WRONG ORIGIN                    '.        
098400     03 PIC X(55) VALUE                                                   
098500        'AAA                754 WRONG KEY - START FROM FIRST SCR'.        
098600     03 PIC X(55) VALUE                                                   
098700        'AAA                755 PACKING NOT POSSIBLE - BLOCK COD'.        
098800     03 PIC X(55) VALUE                                                   
098900        '001                756 UPDATE DONE                     '.        
099000     03 PIC X(55) VALUE                                                   
099100        'AAA                757 NO EMPTY SQUARE                 '.        
099200     03 PIC X(55) VALUE                                                   
099300        '041IDKOLLI         758 CASE MISSING                    '.        
099400     03 PIC X(55) VALUE                                                   
099500        'AAA                759 SQUARE ALREADY FULL             '.        
099600     03 PIC X(55) VALUE                                                   
099700        '041INF             760 INFORMATION MISSING             '.        
099800     03 PIC X(55) VALUE                                                   
099900        'AAA                761 ENTER MEASURES                  '.        
100000     03 PIC X(55) VALUE                                                   
100100        '025IDORDNR         762 DIST - CUST - ORDER MISSING     '.        
100200     03 PIC X(55) VALUE                                                   
100300        '024MEASURES        763 MEASURES MUST BE NUMERIC        '.        
100400     03 PIC X(55) VALUE                                                   
100500        '041ADDRESS         764 ADDRESS MISSING                 '.        
100600     03 PIC X(55) VALUE                                                   
100700        'AAA                765 PACKER AND LINE DO NOT MATCH    '.        
100800     03 PIC X(55) VALUE                                                   
100900        'AAA                766 LINE ALREADY REPORTED           '.        
101000     03 PIC X(55) VALUE                                                   
101100        'AAA                767 LINE PARTY REPORTED             '.        
101200     03 PIC X(55) VALUE                                                   
101300        '023IDARTNR         768 WRONG PART NUMBER               '.        
101400     03 PIC X(55) VALUE                                                   
101500        '025IDARTNR         769 PART NO. MISSING ON PARTS FILE  '.        
101600     03 PIC X(55) VALUE                                                   
101700        'AAA                770 NOT ALLOWED TO START SCREEN 4341'.        
101800     03 PIC X(55) VALUE                                                   
101900        '024FIELD           771 PROD NO./CASE NO. MUST BE NUMERI'.        
102000     03 PIC X(55) VALUE                                                   
102100        '347                772 WRONG PRINTER                   '.        
102200     03 PIC X(55) VALUE                                                   
102300        'AAA                773 DELIVERY NOTE ONLY FOR SWEDEN   '.        
102400     03 PIC X(55) VALUE                                                   
102500        'AAA                774 LINE ALREADY REPORTED BY ZERO HU'.        
102600     03 PIC X(55) VALUE                                                   
102700        'AAA                775 CASE INTERVAL ADDED             '.        
102800     03 PIC X(55) VALUE                                                   
102900        'AAA                776 CASE INTERVAL DELETED           '.        
103000     03 PIC X(55) VALUE                                                   
103100        '007                777 UPDATING NOT ALLOWED            '.        
103200     03 PIC X(55) VALUE                                                   
103300        '011*               778 MORE LINES                      '.        
103400     03 PIC X(55) VALUE                                                   
103500        '001                779 LINE UPDATED                    '.        
103600     03 PIC X(55) VALUE                                                   
103700        'AAA                780 BOTH ADD. AND CANCEL. NOT ALLOWE'.        
103800     03 PIC X(55) VALUE                                                   
103900        'AAA                781 DAILY ORDERS ONLY IN INTERVALS  '.        
104000     03 PIC X(55) VALUE                                                   
104100        'AAA                782 ORDER HAS BEEN SPLIT            '.        
104200     03 PIC X(55) VALUE                                                   
104300        'AAA                783 ORDER HAS NOT BEEN SPLIT        '.        
104400     03 PIC X(55) VALUE                                                   
104500        'AAA                784 SCROLLING NOT ALLOWED           '.        
104600     03 PIC X(55) VALUE                                                   
104700        'AAA                785 INTERVAL ALREADY PARTLY SPLIT   '.        
104800     03 PIC X(55) VALUE                                                   
104900        'AAA                786 NOT ALLOWED TO START SCREEN 4337'.        
105000     03 PIC X(55) VALUE                                                   
105100        'AAA                787 PF11 AND NEW KEYS NOT ALLOWED   '.        
105200     03 PIC X(55) VALUE                                                   
105300        'AAA                788 INQUIRE BEFORE PRESSING PF11    '.        
105400     03 PIC X(55) VALUE                                                   
105500        '004                789 NOTHING CHANGED; NO UPDATING    '.        
105600     03 PIC X(55) VALUE                                                   
105700        'AAA                790 CASE LABEL PRINTED              '.        
105800     03 PIC X(55) VALUE                                                   
105900        'AAA                791 ORDER NOT READY                 '.        
106000     03 PIC X(55) VALUE                                                   
106100        '041VKART           792 WEIGHT MISSING                  '.        
106200     03 PIC X(55) VALUE                                                   
106300        '041VLART           793 VOLUME MISSING                  '.        
106400     03 PIC X(55) VALUE                                                   
106500        '312                794 ORIGIN MISSING                  '.        
106600     03 PIC X(55) VALUE                                                   
106700        '023IDKOLLI         795 WRONG CASE                      '.        
106800     03 PIC X(55) VALUE                                                   
106900        'AAA                796 ONLY ONE PAGE BACKWARD POSSIBLE '.        
107000     03 PIC X(55) VALUE                                                   
107100        '041IDKOLLI-FOM     801 START CASENO MISSING            '.        
107200     03 PIC X(55) VALUE                                                   
107300        '025IDPRC           802 PRC-CHANNEL MISSING             '.        
107400     03 PIC X(55) VALUE                                                   
107500        'AAA                804 DEVIATION CONTROL IN PROGRESS   '.        
107600     03 PIC X(55) VALUE                                                   
107700        'AAA                807 LAST CASE NOT FINISHED          '.        
107800     03 PIC X(55) VALUE                                                   
107900        '028                826 TOO MANY ORDERLINES             '.        
108000     03 PIC X(55) VALUE                                                   
108100        'AAA                830 LINE ZEROED BY ZEROHUNTER       '.        
108110     03 PIC X(55) VALUE                                                   
108120        '831*               831 CASE IS PART OF MIXED CASE      '.        
108200     03 PIC X(55) VALUE                                                   
108300        'AAA                94A FORMAL ERROR DISTRICT NO.       '.        
108400     03 PIC X(55) VALUE                                                   
108500        'AAA                94B FORMAL ERROR CUSTOMER NO.       '.        
108600     03 PIC X(55) VALUE                                                   
108700        'AAA                94C FORMAL ERROR ORDER NO.          '.        
108800     03 PIC X(55) VALUE                                                   
108900        'AAA                94D FORMAL ERROR ORDER CLASS        '.        
109000     03 PIC X(55) VALUE                                                   
109100        'AAA                94E FORMAL ERROR FREIGHT CODE       '.        
109200     03 PIC X(55) VALUE                                                   
109300        'AAA                94F FORMAL ERROR BACKORDERED        '.        
109400     03 PIC X(55) VALUE                                                   
109500        'AAA                94G FORMAL ERROR NATIONAL SIGN      '.        
109600     03 PIC X(55) VALUE                                                   
109700        'AAA                94H FORMAL ERROR STOCKUPDATING      '.        
109800     03 PIC X(55) VALUE                                                   
109900        'AAA                94I FORMAL ERROR CAMPAIGN REF       '.        
110000     03 PIC X(55) VALUE                                                   
110100        'AAA                940 FORMAL ERROR ACCOUNT            '.        
110200     03 PIC X(55) VALUE                                                   
110300        'AAA                941 FORMAL ERROR ANALYS NO.         '.        
110400     03 PIC X(55) VALUE                                                   
110500        'AAA                942 FORMAL ERROR COST CENTER        '.        
110600     03 PIC X(55) VALUE                                                   
110700        'AAA                943 FORMAL ERROR COMPANY NO.        '.        
110800     03 PIC X(55) VALUE                                                   
110900        'AAA                944 FORMAL ERROR INVOICE TYPE       '.        
111000     03 PIC X(55) VALUE                                                   
111100        'AAA                945 FORMAL ERROR CONSOLIDATION      '.        
111200     03 PIC X(55) VALUE                                                   
111300        'AAA                946 FORMAL ERROR TPO TYPE           '.        
111400     03 PIC X(55) VALUE                                                   
111500        'AAA                947 FORMAL ERROR TPO DATE           '.        
111600     03 PIC X(55) VALUE                                                   
111700        'AAA                948 FORMAL ERROR RFS TYPE           '.        
111800     03 PIC X(55) VALUE                                                   
111900        'AAA                949 FORMAL ERROR CUSTOMS INV.       '.        
112000     03 PIC X(55) VALUE                                                   
112100        'AAA                95A LOGICAL ERROR DISTRICT NO.      '.        
112200     03 PIC X(55) VALUE                                                   
112300        'AAA                95B LOGICAL ERROR ORDER NO.         '.        
112400     03 PIC X(55) VALUE                                                   
112500        'AAA                95C LOGICAL ERROR ORDER CLASS       '.        
112600     03 PIC X(55) VALUE                                                   
112700        'AAA                95D LOGICAL ERROR ACCOUNT           '.        
112800     03 PIC X(55) VALUE                                                   
112900        'AAA                95E LOGICAL ERROR ANALYS NO.        '.        
113000     03 PIC X(55) VALUE                                                   
113100        'AAA                95F LOGICAL ERROR COST CENTER       '.        
113200     03 PIC X(55) VALUE                                                   
113300        'AAA                950 LOGICAL ERROR COMPANY NO.       '.        
113400     03 PIC X(55) VALUE                                                   
113500        'AAA                952 LOGICAL ERROR INVOICE TYPE      '.        
113600     03 PIC X(55) VALUE                                                   
113700        'AAA                953 LOGICAL ERROR TPO TYPE          '.        
113800     03 PIC X(55) VALUE                                                   
113900        'AAA                954 LOGICAL ERROR TPO DATE          '.        
114000     03 PIC X(55) VALUE                                                   
114100        '380                955 UPD NOT ALLOWED/ORIGIN EXIST    '.        
114200     03 PIC X(55) VALUE                                                   
114300        '381                956 REGISTRATION COMPLETED          '.        
114400     03 PIC X(55) VALUE                                                   
114500        '025*               010 MISSING IN REGISTER             '.        
114600 01  FILLER REDEFINES MESSAGE-TABLE.                                      
114700*    -- NOTE KEEP OCCURS SAME AS MAX-INDX                                 
114800     03 FILLER OCCURS 543.                                                
114900        05  IDMSG         PIC X(3).                                       
115000        05  IDELMT        PIC X(16).                                      
115100        05  MFSMED        PIC X(3).                                       
115200        05  MEDKONVTEXT   PIC X(33).                                      
115300                                                                          
115400 LINKAGE SECTION.                                                         
115500                                                                          
115600 01  LINK-AREA.                                                           
115700*    03 -COPY WL01MCNV                                                    
115800                                                                          
115900 PROCEDURE DIVISION  USING  LINK-AREA.                                    
116000                                                                          
116100     IF MCNV-IDSPRAK = 'SV'                                               
116200        MOVE 'S  '  TO  MED-IDSKYLT                                       
116300     ELSE                                                                 
116400        MOVE 'GB '  TO  MED-IDSKYLT                                       
116500     END-IF                                                               
116600                                                                          
116700     MOVE SPACE TO MED-MFSINF MCNV-MFSINF                                 
116800     MOVE SPACE TO MED-MFSFEL MCNV-MFSFEL                                 
116900                                                                          
117000     IF MCNV-IDMSG-INFO NOT = SPACE                                       
117100       MOVE 1 TO INDX                                                     
117200       PERFORM UNTIL INDX > MAX-INDX                                      
117300       OR (IDMSG(INDX) = MCNV-IDMSG-INFO AND                              
117400           (IDELMT(INDX) = MCNV-IDELMT-ERROR OR '*'))                     
117500                                                                          
117600         ADD 1 TO INDX                                                    
117700       END-PERFORM                                                        
117800       IF INDX <= MAX-INDX                                                
117900         MOVE MFSMED(INDX) TO MED-IDMFSINF                                
118000       ELSE                                                               
118100         MOVE '999'        TO MED-IDMFSINF                                
118200       END-IF                                                             
118300     END-IF                                                               
118400                                                                          
118500     IF MCNV-IDMSG-ERROR NOT = SPACE                                      
118600       MOVE 1 TO INDX                                                     
118700       PERFORM UNTIL INDX > MAX-INDX                                      
118800       OR (IDMSG(INDX) = MCNV-IDMSG-ERROR AND                             
118900           (IDELMT(INDX) = MCNV-IDELMT-ERROR OR '*'))                     
119000                                                                          
119100         ADD 1 TO INDX                                                    
119200       END-PERFORM                                                        
119300       IF INDX <= MAX-INDX                                                
119400         MOVE MFSMED(INDX) TO MED-IDMFSFEL                                
119500       ELSE                                                               
119600         MOVE '999'        TO MED-IDMFSFEL                                
119700       END-IF                                                             
119800     END-IF                                                               
119900                                                                          
120000     MOVE SPACE TO MED-MFSMED                                             
120100                                                                          
120200     CALL WMEDKONV USING  MED-WMEDAREA                                    
120300                                                                          
120400     MOVE MED-MFSINF TO MCNV-MFSINF                                       
120500     MOVE MED-MFSFEL TO MCNV-MFSFEL                                       
120600                                                                          
120700     MOVE ZERO TO RETURN-CODE                                             
120800     GOBACK                                                               
120900     .                                                                    
