000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W5603000.                                                
000004 AUTHOR.         INGVAR SKJELBRED.                                        
000005 DATE-WRITTEN.   96/11/18.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008                                                                          
000009*    FUNKTION:                                                            
000010*        BYTESDUBBLERING OCH UPPDATERING AV AVERAGE COST                  
000011*        FÖR POST A13 UPPDATERAS AVERAGE COST PÅ WDK7.                    
000012*                                                                         
000013*        PROGRAMMET LÄSER      WDK7                                       
000014*                                                                         
000015*    ABENDKODER:                                                          
000016*        U0016 -  . . . .                                                 
000017                                                                          
000018     SKIP3                                                                
000019 ENVIRONMENT DIVISION.                                                    
000020     SKIP2                                                                
000021 INPUT-OUTPUT SECTION.                                                    
000022                                                                          
000023 FILE-CONTROL.                                                            
000024     SKIP2                                                                
000025*          --- INFIL                                                      
000026     SELECT INFIL                      ASSIGN TO W56030D1.                
000027     SKIP2                                                                
000028*          --- UTFIL                                                      
000029     SELECT W56030                     ASSIGN TO W56030D2.                
000030     SKIP2                                                                
000031*          --- UTFIL2                                                     
000032     SELECT W56031                     ASSIGN TO W56030D3.                
000033     SKIP2                                                                
000034 DATA DIVISION.                                                           
000035     SKIP3                                                                
000036 FILE SECTION.                                                            
000037     SKIP3                                                                
000038 FD  INFIL                                                                
000039     RECORDING       V                                                    
000040     BLOCK CONTAINS  0.                                                   
000041                                                                          
000042*01  -COPY W510A01      -L.                                               
000043                                                                          
000044*01  -COPY W510AX2      -L.                                               
000045                                                                          
000046*01  -COPY W510A03      -L.                                               
000047                                                                          
000048*01  -COPY W510A04      -L.                                               
000049                                                                          
000050*01  -COPY W510A05      -L.                                               
000051                                                                          
000052*01  -COPY W510A06      -L.                                               
000053                                                                          
000054*01  -COPY W510A07      -L.                                               
000055                                                                          
000056*01  -COPY W510A08      -L.                                               
000057                                                                          
000058*01  -COPY W510A09      -L.                                               
000059                                                                          
000060*01  -COPY W510A10      -L.                                               
000061                                                                          
000062*01  -COPY W510A11      -L.                                               
000063                                                                          
000064*01  -COPY W510A12      -L.                                               
000065                                                                          
000066*01  -COPY W510A13      -L.                                               
000067                                                                          
000068*01  -COPY W510A14      -L.                                               
000069                                                                          
000070*01  -COPY W510A15      -L.                                               
000071                                                                          
000072*01  -COPY W510A16      -L.                                               
000073                                                                          
000074*01  -COPY W510A17      -L.                                               
000075                                                                          
000076*01  -COPY W510A18      -L.                                               
000077                                                                          
000078*01  -COPY W510A19      -L.                                               
000079     SKIP3                                                                
000080 FD  W56030                                                               
000081     RECORDING       V                                                    
000082     BLOCK CONTAINS  0.                                                   
000083                                                                          
000084*01  POST -COPY W510A01 -PRE  A01-  -L.                                   
000085                                                                          
000086*01  POST -COPY W510A02 -PRE  A02-  -L.                                   
000087                                                                          
000088*01  POST -COPY W510A03 -PRE  A03-  -L.                                   
000089                                                                          
000090*01  POST -COPY W510A04 -PRE  A04-  -L.                                   
000091                                                                          
000092*01  POST -COPY W510A05 -PRE  A05-  -L.                                   
000093                                                                          
000094*01  POST -COPY W510A06 -PRE  A06-  -L.                                   
000095                                                                          
000096*01  POST -COPY W510A07 -PRE  A07-  -L.                                   
000097                                                                          
000098*01  POST -COPY W510A08 -PRE  A08-  -L.                                   
000099                                                                          
000100*01  POST -COPY W510A09 -PRE  A09-  -L.                                   
000101                                                                          
000102*01  POST -COPY W510A10 -PRE  A10-  -L.                                   
000103                                                                          
000104*01  POST -COPY W510A11 -PRE  A11-  -L.                                   
000105                                                                          
000106*01  POST -COPY W510A12 -PRE  A12-  -L.                                   
000107                                                                          
000108*01  POST -COPY W510A13 -PRE  A13-  -L.                                   
000109                                                                          
000110*01  POST -COPY W510A14 -PRE  A14-  -L.                                   
000111                                                                          
000112*01  POST -COPY W510A15 -PRE  A15-  -L.                                   
000113                                                                          
000114*01  POST -COPY W510A16 -PRE  A16-  -L.                                   
000115                                                                          
000116*01  POST -COPY W510A17 -PRE  A17-  -L.                                   
000117                                                                          
000118*01  POST -COPY W510A18 -PRE  A18-  -L.                                   
000119                                                                          
000120*01  POST -COPY W510A19 -PRE  A19-  -L.                                   
000121     EJECT                                                                
000122                                                                          
000123     SKIP3                                                                
000124 FD  W56031                                                               
000125     LABEL RECORD STANDARD                                                
000126     RECORDING       F                                                    
000127     BLOCK CONTAINS  0.                                                   
000128     SKIP2                                                                
000129*01  POST -COPY W56031  -PRE  LIST-  -L.                                  
000130                                                                          
000131 WORKING-STORAGE SECTION.                                                 
000132     SKIP2                                                                
000133                                                                          
000134*    -- CHECKED BY WY2000                                                 
000135 77  IDPGM                       PIC X(8)    VALUE 'W5603000'.            
000136 01  CHKP-VAR.                                                            
000137     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000138     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000139     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000140     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000141     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000142     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
000143 77  JA                          PIC X       VALUE 'J'.                   
000144 77  NEJ                         PIC X       VALUE 'N'.                   
000145 77  W-INFIL-KVPOST-IN           PIC S9(7)   VALUE ZERO COMP-3.           
000146 77  IX                          PIC S9(7)   COMP-3 VALUE ZERO.           
000147 77  IX2                         PIC S9(7)   COMP-3 VALUE ZERO.           
000148 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
000149 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
000150     SKIP2                                                                
000151 01  FELTEXT.                                                             
000152     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000153     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000154                                                                          
000155 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
000156     88  END-OF-INFIL                        VALUE 'J'.                   
000157     EJECT                                                                
000158                                                                          
000159 01  W-PRKURS-FRAN           PIC S9(5)V9(2) VALUE +0     COMP-3.          
000160 01  W-PRKURS-TILL           PIC S9(5)V9(2) VALUE +0     COMP-3.          
000161 01  W-TILLOKDAT             PIC S9(6)      VALUE +0     COMP-3.          
000162     EJECT                                                                
000163 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000164 01  FILLER REDEFINES DAGENS-DATUM.                                       
000165     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000166     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000167     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000168     EJECT                                                                
000169*    ---- EGNA ARBETSAREOR                                                
000170     SKIP2                                                                
000171 01  W-IDDCTEXT-MSGI.                                                     
000172     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
000173     03  W-IDDC-MSGI         PIC X(2).                                    
000174*                                                                         
000175 01  WS-IDLEVNR                  PIC X(5)   VALUE SPACE.                  
000176*                                                                         
000177 01  DYNAMISKA-SUBPROGRAM.                                                
000178*                                                                         
000179     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000180     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000181     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000182     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000183     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000184     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
000185     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000186     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
000187     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000188     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000189     EJECT                                                                
000190*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
000191 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
000192*   -COPY W005WDK7                                                        
000193     EJECT                                                                
000194*---- VALID IDDC CODES                                                    
000195*                                                                         
000196*01  -COPY WWDC99                                                         
000197     EJECT                                                                
000198*01  -COPY W510CURR                                                       
000199     EJECT                                                                
000200*---- PARAMETRAR TILL ABEND                                               
000201 01  RETURKODER.                                                          
000202     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
000203     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
000204     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
000205                                                                          
000206*    --- PARAMETRAR TILL POSTSUM                                          
000207*                                                                         
000208*01  -COPY W0005   -PRE  POSTSUM-                                         
000209     EJECT                                                                
000210*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000211 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
000212*01 -COPY WMSGINIT                                                        
000213     EJECT                                                                
000214*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
000215 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
000216     SKIP2                                                                
000217*01  -COPY WDATAREA.                                                      
000218     EJECT                                                                
000219*- - - - - - - - - - - - - -  PARAMETRAR TILL DATUMKORT                   
000220 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000221                                                                          
000222*01  -COPY WDATKORT                                                       
000223     EJECT                                                                
000224*****************************************************************         
000225 01  WS-IDARTNR                PIC 9(9) COMP-3.                           
000226 01  TEST-IDARTNR              PIC 9(9) COMP-3.                           
000227*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
000228     EJECT                                                                
000229******************************************************************        
000230 01  FILLER                      PIC X(24)   VALUE 'W510AVG-AREA'.        
000231*01  -COPY W510AVG                                                        
000232     EJECT                                                                
000233 01  INFIL-AREA-START            PIC X(24)   VALUE                        
000234                                             'INFIL-AREA-START'.          
000235     SKIP2                                                                
000236 01  INFIL-AREA.                                                          
000237     03  INFIL-AREA-0.                                                    
000238         05  INFIL-IDPTYP        PIC X(3).                                
000239         05  FILLER              PIC X(400).                              
000240*   03  FILLER -COPY W510A01  -PRE IA01-  -RED  INFIL-AREA-0              
000241*   03  FILLER -COPY W510AX2  -PRE IAX2-  -RED  INFIL-AREA-0              
000242*   03  FILLER -COPY W510A03  -PRE IA03-  -RED  INFIL-AREA-0              
000243*   03  FILLER -COPY W510A04  -PRE IA04-  -RED  INFIL-AREA-0              
000244*   03  FILLER -COPY W510A05  -PRE IA05-  -RED  INFIL-AREA-0              
000245*   03  FILLER -COPY W510A06  -PRE IA06-  -RED  INFIL-AREA-0              
000246*   03  FILLER -COPY W510A07  -PRE IA07-  -RED  INFIL-AREA-0              
000247*   03  FILLER -COPY W510A08  -PRE IA08-  -RED  INFIL-AREA-0              
000248*   03  FILLER -COPY W510A09  -PRE IA09-  -RED  INFIL-AREA-0              
000249*   03  FILLER -COPY W510A10  -PRE IA10-  -RED  INFIL-AREA-0              
000250*   03  FILLER -COPY W510A11  -PRE IA11-  -RED  INFIL-AREA-0              
000251*   03  FILLER -COPY W510A12  -PRE IA12-  -RED  INFIL-AREA-0              
000252*   03  FILLER -COPY W510A13  -PRE IA13-  -RED  INFIL-AREA-0              
000253*   03  FILLER -COPY W510A14  -PRE IA14-  -RED  INFIL-AREA-0              
000254*   03  FILLER -COPY W510A15  -PRE IA15-  -RED  INFIL-AREA-0              
000255*   03  FILLER -COPY W510A16  -PRE IA16-  -RED  INFIL-AREA-0              
000256*   03  FILLER -COPY W510A17  -PRE IA17-  -RED  INFIL-AREA-0              
000257*   03  FILLER -COPY W510A18  -PRE IA18-  -RED  INFIL-AREA-0              
000258*   03  FILLER -COPY W510A19  -PRE IA19-  -RED  INFIL-AREA-0              
000259     EJECT                                                                
000260 01  UTFIL-AREA-START            PIC X(24)   VALUE                        
000261                                             'UTFIL-AREA-START'.          
000262     SKIP2                                                                
000263 01  UTFIL-AREA.                                                          
000264     03  UTFIL-AREA-0.                                                    
000265         05  UTFIL-IDPTYP        PIC X(3).                                
000266         05  FILLER              PIC X(400).                              
000267*   03  FILLER -COPY W510A01  -PRE UA01-  -RED  UTFIL-AREA-0              
000268*   03  FILLER -COPY W510A02  -PRE UA02-  -RED  UTFIL-AREA-0              
000269*   03  FILLER -COPY W510A03  -PRE UA03-  -RED  UTFIL-AREA-0              
000270*   03  FILLER -COPY W510A04  -PRE UA04-  -RED  UTFIL-AREA-0              
000271*   03  FILLER -COPY W510A05  -PRE UA05-  -RED  UTFIL-AREA-0              
000272*   03  FILLER -COPY W510A06  -PRE UA06-  -RED  UTFIL-AREA-0              
000273*   03  FILLER -COPY W510A07  -PRE UA07-  -RED  UTFIL-AREA-0              
000274*   03  FILLER -COPY W510A08  -PRE UA08-  -RED  UTFIL-AREA-0              
000275*   03  FILLER -COPY W510A09  -PRE UA09-  -RED  UTFIL-AREA-0              
000276*   03  FILLER -COPY W510A10  -PRE UA10-  -RED  UTFIL-AREA-0              
000277*   03  FILLER -COPY W510A11  -PRE UA11-  -RED  UTFIL-AREA-0              
000278*   03  FILLER -COPY W510A12  -PRE UA12-  -RED  UTFIL-AREA-0              
000279*   03  FILLER -COPY W510A13  -PRE UA13-  -RED  UTFIL-AREA-0              
000280*   03  FILLER -COPY W510A14  -PRE UA14-  -RED  UTFIL-AREA-0              
000281*   03  FILLER -COPY W510A15  -PRE UA15-  -RED  UTFIL-AREA-0              
000282*   03  FILLER -COPY W510A16  -PRE UA16-  -RED  UTFIL-AREA-0              
000283*   03  FILLER -COPY W510A17  -PRE UA17-  -RED  UTFIL-AREA-0              
000284*   03  FILLER -COPY W510A18  -PRE UA18-  -RED  UTFIL-AREA-0              
000285*   03  FILLER -COPY W510A19  -PRE UA19-  -RED  UTFIL-AREA-0              
000286*                                                                         
000287     EJECT                                                                
000288 01  FILLER                   PIC X(16) VALUE 'LIST-AREA'.                
000289*01  AREA  -PRE LIST- -COPY W56031                                        
000290     EJECT                                                                
000291 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000292     SKIP3                                                                
000293 01  NYCKLAR-TILL-DLI.                                                    
000294     03  W-IDARTNR-X.                                                     
000295         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000296     03  W-IDARTNR-ART-X.                                                 
000297         05  W-IDARTNR-ART       PIC S9(9)   VALUE ZERO COMP-3.           
000298     03  W-IDDC-X.                                                        
000299         05 W-IDDC               PIC X(2).                                
000300     03  W-IDLEVNR-X.                                                     
000301         05 W-IDLEVNR            PIC X(5)   VALUE SPACE.                  
000302     SKIP2                                                                
000303*    --- STATUS-KOD FRÅN IMS                                              
000304 01  STATUS-WS                   PIC XX.                                  
000305     88  SEGMENT-FINNS                       VALUE '  '.                  
000306     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000307     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000308     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000309     88  IMS-EJ-OK                           VALUE 'XD'.                  
000310     SKIP2                                                                
000311 01  GODK-STATUSKODER.                                                    
000312     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000313     SKIP3                                                                
000314 01  SSA1                        PIC X(64).                               
000315 01  SSA2                        PIC X(64).                               
000316 01  SSA3                        PIC X(64).                               
000317     EJECT                                                                
000318*    --- IMS FUNKTIONSKODER                                               
000319*01  -COPY W0003                                                          
000320     EJECT                                                                
000321*    ---  DLI INPUT-OUTPUT AREA                                           
000322 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
000323 01  DLI-IO-WDK711.                                                       
000324*    03  -COPY WDK711                                                     
000325     EJECT                                                                
000326 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
000327 01  DLI-IO-WLARTC01.                                                     
000328*    03  -COPY WDK601  -PRE ARTC-                                         
000329     EJECT                                                                
000330 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
000331 01  DLI-IO-WLARTC11.                                                     
000332*    03  -COPY WDK611  -PRE ARTC-                                         
000333     EJECT                                                                
000334 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC21'.                    
000335 01  DLI-IO-WLARTC21.                                                     
000336*    03  -COPY WDK621  -PRE ARTC-                                         
000337                                                                          
000338 LINKAGE SECTION.                                                         
000339                                                                          
000340*01  -COPY W0009   -PRE MSG-                                              
000350     EJECT                                                                
000351                                                                          
000352*01  -COPY W0009   -PRE USEA-                                             
000353                                                                          
000354*01  -COPY W0008  -PRE WDK7-                                              
000355     05  FILLER                  PIC X.                                   
000356     EJECT                                                                
000357*01  -COPY W0008  -PRE WDB6-                                              
000358     05  FILLER                  PIC X.                                   
000359     EJECT                                                                
000360*01  -COPY W0008  -PRE ARTC-                                              
000361     05  FILLER                  PIC X.                                   
000362     EJECT                                                                
000363*01  -COPY W0008  -PRE 9305-                                              
000364     05  FILLER                  PIC X.                                   
000365     EJECT                                                                
000366*01  -COPY W0008  -PRE AVG-WDB6-                                          
000367     05  FILLER                  PIC X.                                   
000368     EJECT                                                                
000369     EJECT                                                                
000370 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB                      
000380                           WDB6-PCB ARTC-PCB 9305-PCB                     
000381                           AVG-WDB6-PCB.                                  
000382 MAIN SECTION.                                                            
000383     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB                      
000384                           WDB6-PCB ARTC-PCB 9305-PCB                     
000385                           AVG-WDB6-PCB.                                  
000386                                                                          
000387     PERFORM A-INIT                                                       
000388     PERFORM S01-LAES-INFIL                                               
000389     PERFORM UNTIL END-OF-INFIL                                           
000390       IF CHKP-ANT > CHKP-MAX                                             
000391         PERFORM X-TAG-CHECKPOINT                                         
000392       END-IF                                                             
000393       PERFORM C-BEHANDLA-SKRIV-UTFIL                                     
000394       PERFORM S01-LAES-INFIL                                             
000395     END-PERFORM                                                          
000396                                                                          
000397                                                                          
000398     PERFORM Z-FINIT                                                      
000399                                                                          
000400     MOVE ZERO TO RETURN-CODE                                             
000401     GOBACK                                                               
000402     .                                                                    
000403     EJECT                                                                
000404 A-INIT SECTION.                                                          
000405     SKIP2                                                                
000406                                                                          
000407     ACCEPT DAGENS-DATUM FROM DATE                                        
000408     PERFORM IMS-RESTART                                                  
000409                                                                          
000410     OPEN INPUT INFIL                                                     
000411                                                                          
000412     OPEN OUTPUT W56030                                                   
000413                 W56031                                                   
000414                                                                          
000415     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000416     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
000417     MOVE D-AAR                 TO W-DATE-AAMM(1:2)                       
000418                                   AVG-TIAA                               
000419     MOVE D-MAANAD              TO W-DATE-AAMM(3:2)                       
000420                                   AVG-TIMM                               
000421     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
000422     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
000423     MOVE 'M'                   TO CURR-KDVALTYP                          
000424                                                                          
000425     .                                                                    
000426     EJECT                                                                
000427 C-BEHANDLA-SKRIV-UTFIL SECTION.                                          
000428** SKRIV-UTPOST, SEDAN I FÖREKOMMANDE FALL BYTESDUBBLERA                  
000429                                                                          
000430     PERFORM CB-FLYTTA-SKRIV-W56030                                       
000431                                                                          
000432     IF INFIL-IDPTYP = 'A03'                                              
000433     OR INFIL-IDPTYP = 'A06'                                              
000434     OR INFIL-IDPTYP = 'A07'                                              
000435     OR INFIL-IDPTYP = 'A08'                                              
000436     OR INFIL-IDPTYP = 'A09'                                              
000437     OR INFIL-IDPTYP = 'A11'                                              
000438     OR INFIL-IDPTYP = 'A12'                                              
000439     OR INFIL-IDPTYP = 'A13'                                              
000440     OR INFIL-IDPTYP = 'A18'                                              
000441     OR INFIL-IDPTYP = 'A19'                                              
000442        PERFORM CAB-FLYTTA-TEST-ARTIKEL                                   
000443        IF BYT19-BYTES                                                    
000444        OR BYT19-RADIO                                                    
000445           PERFORM CAC-FLYTTA-TILL-IDLEVNR                                
000446           IF BYT19-BYTES                                                 
000447              PERFORM CD-SKAPA-BYTESOBJEKT                                
000448           ELSE                                                           
000449              IF BYT19-RADIO                                              
000450                 PERFORM CE-SKAPA-RADIOOBJEKT                             
000451              END-IF                                                      
000452           END-IF                                                         
000453           PERFORM CAA-SKAPA-SOK-FALT                                     
000454           PERFORM CA-UPD-PRAVCOST-WDK7                                   
000455           PERFORM CB-FLYTTA-SKRIV-W56030                                 
000456        END-IF                                                            
000457     END-IF                                                               
000458                                                                          
000459     .                                                                    
000460     EJECT                                                                
000461 CA-UPD-PRAVCOST-WDK7 SECTION.                                            
000462                                                                          
000463     MOVE SPACE TO LIST-BEART                                             
000464     PERFORM IMS-GET-ARTC-WLARTC01                                        
000465     IF SEGMENT-SAKNAS                                                    
000466        CALL ABEND USING                                                  
000467                      RKOD-ABEND-UTAN-DUMP                                
000468     END-IF                                                               
000469                                                                          
000470     EVALUATE INFIL-IDPTYP                                                
000471        WHEN 'A03'                                                        
000472             PERFORM CAH-BEHANDLA-A03                                     
000473        WHEN 'A11'                                                        
000474*****************************************************************         
000475**** BINNING, LOCAL DELIVERIES,                         *********         
000476**** PRISET TAS FRÅN WDK621 SAKNAS WDK621 DÅ SKAPAS     *********         
000477**** ETT PRIS PÅ 0.01                                   *********         
000478*****************************************************************         
000479             PERFORM CAF-BEHANDLA-A11                                     
000480        WHEN 'A13'                                                        
000481*****************************************************************         
000482**** BINNING, VOR DELIVERIES ************************************         
000483*****************************************************************         
000484             PERFORM  CAG-BEHANDLA-A13                                    
000485        WHEN 'A06'                                                        
000486             IF SEGMENT-FINNS                                             
000487                MOVE ARTC-ART-KDPRODSL                                    
000488                               TO IA06-KDPRODSL                           
000489             END-IF                                                       
000490             PERFORM IMS-GET-ARTC-WLARTC11                                
000491             IF SEGMENT-FINNS                                             
000492                MOVE ARTC-CLAG-KDPSLLOC                                   
000493                               TO IA06-KDPSLLOC                           
000494             ELSE                                                         
000495                MOVE ZERO                                                 
000496                               TO IA06-KDPSLLOC                           
000497             END-IF                                                       
000498             PERFORM IMS-GET-WDK711                                       
000499             IF SEGMENT-FINNS                                             
000500                MOVE SLAG-PRAVCOST                                        
000501                               TO IA06-PRAVCOST                           
000502             ELSE                                                         
000503                MOVE ZERO      TO IA06-PRAVCOST                           
000504             END-IF                                                       
000505        WHEN 'A07'                                                        
000506             IF SEGMENT-FINNS                                             
000507                MOVE ARTC-ART-KDPRODSL                                    
000508                               TO IA07-KDPRODSL                           
000509             END-IF                                                       
000510             PERFORM IMS-GET-ARTC-WLARTC11                                
000511             IF SEGMENT-FINNS                                             
000512                MOVE ARTC-CLAG-KDPSLLOC                                   
000513                               TO IA07-KDPSLLOC                           
000514             ELSE                                                         
000515                MOVE ZERO                                                 
000516                               TO IA07-KDPSLLOC                           
000517             END-IF                                                       
000518             PERFORM IMS-GET-WDK711                                       
000519             IF SEGMENT-FINNS                                             
000520                MOVE SLAG-PRAVCOST                                        
000521                               TO IA07-PRAVCOST                           
000522             ELSE                                                         
000523                MOVE ZERO      TO IA07-PRAVCOST                           
000524             END-IF                                                       
000525        WHEN 'A08'                                                        
000526             IF SEGMENT-FINNS                                             
000527                MOVE ARTC-ART-KDPRODSL                                    
000528                               TO IA08-KDPRODSL                           
000529             END-IF                                                       
000530             PERFORM IMS-GET-ARTC-WLARTC11                                
000531             IF SEGMENT-FINNS                                             
000532                MOVE ARTC-CLAG-KDPSLLOC                                   
000533                               TO IA08-KDPSLLOC                           
000534             ELSE                                                         
000535                MOVE ZERO                                                 
000536                               TO IA08-KDPSLLOC                           
000537             END-IF                                                       
000538             PERFORM IMS-GET-WDK711                                       
000539             IF SEGMENT-FINNS                                             
000540                MOVE SLAG-PRAVCOST                                        
000541                               TO IA08-PRAVCOST                           
000542             ELSE                                                         
000543                MOVE ZERO      TO IA08-PRAVCOST                           
000544             END-IF                                                       
000545        WHEN 'A09'                                                        
000546             IF SEGMENT-FINNS                                             
000547                MOVE ARTC-ART-KDPRODSL                                    
000548                               TO IA09-KDPRODSL                           
000549             END-IF                                                       
000550             PERFORM IMS-GET-ARTC-WLARTC11                                
000551             IF SEGMENT-FINNS                                             
000552                MOVE ARTC-CLAG-KDPSLLOC                                   
000553                               TO IA09-KDPSLLOC                           
000554             ELSE                                                         
000555                MOVE ZERO                                                 
000556                               TO IA09-KDPSLLOC                           
000557             END-IF                                                       
000558             PERFORM IMS-GET-WDK711                                       
000559             IF SEGMENT-FINNS                                             
000560                MOVE SLAG-PRAVCOST                                        
000561                               TO IA09-PRAVCOST                           
000562             ELSE                                                         
000563                MOVE ZERO      TO IA09-PRAVCOST                           
000564             END-IF                                                       
000565        WHEN 'A12'                                                        
000566             IF SEGMENT-FINNS                                             
000567                MOVE ARTC-ART-KDPRODSL                                    
000568                               TO IA12-KDPRODSL                           
000569             END-IF                                                       
000570             PERFORM IMS-GET-ARTC-WLARTC11                                
000571             IF SEGMENT-FINNS                                             
000572                MOVE ARTC-CLAG-KDPSLLOC                                   
000573                               TO IA12-KDPSLLOC                           
000574             ELSE                                                         
000575                MOVE ZERO                                                 
000576                               TO IA12-KDPSLLOC                           
000577             END-IF                                                       
000578             PERFORM IMS-GET-WDK711                                       
000579             IF SEGMENT-FINNS                                             
000580                MOVE SLAG-PRAVCOST                                        
000581                               TO IA12-PRAVCOST                           
000582             ELSE                                                         
000583                MOVE ZERO      TO IA12-PRAVCOST                           
000584             END-IF                                                       
000585        WHEN 'A18'                                                        
000586             IF SEGMENT-FINNS                                             
000587                MOVE ARTC-ART-KDPRODSL                                    
000588                               TO IA18-KDPRODSL                           
000589             END-IF                                                       
000590             PERFORM IMS-GET-ARTC-WLARTC11                                
000591             IF SEGMENT-FINNS                                             
000592                MOVE ARTC-CLAG-KDPSLLOC                                   
000593                               TO IA18-KDPSLLOC                           
000594             ELSE                                                         
000595                MOVE ZERO                                                 
000596                               TO IA18-KDPSLLOC                           
000597             END-IF                                                       
000598             PERFORM IMS-GET-WDK711                                       
000599             IF SEGMENT-FINNS                                             
000600                MOVE SLAG-PRAVCOST                                        
000601                               TO IA18-PRAVCOST                           
000602             ELSE                                                         
000603                MOVE ZERO      TO IA18-PRAVCOST                           
000604             END-IF                                                       
000605        WHEN 'A19'                                                        
000606             IF SEGMENT-FINNS                                             
000607                MOVE ARTC-ART-KDPRODSL                                    
000608                               TO IA19-KDPRODSL                           
000609             END-IF                                                       
000610             PERFORM IMS-GET-ARTC-WLARTC11                                
000611             IF SEGMENT-FINNS                                             
000612                MOVE ARTC-CLAG-KDPSLLOC                                   
000613                               TO IA19-KDPSLLOC                           
000614             ELSE                                                         
000615                MOVE ZERO                                                 
000616                               TO IA19-KDPSLLOC                           
000617             END-IF                                                       
000618             PERFORM IMS-GET-WDK711                                       
000619             IF SEGMENT-FINNS                                             
000620                MOVE SLAG-PRAVCOST                                        
000621                               TO IA19-PRAVCOST                           
000622             ELSE                                                         
000623                MOVE ZERO      TO IA19-PRAVCOST                           
000624             END-IF                                                       
000625     END-EVALUATE                                                         
000626                                                                          
000627     .                                                                    
000628     EJECT                                                                
000629 CAA-SKAPA-SOK-FALT SECTION.                                              
000630                                                                          
000631     EVALUATE INFIL-IDPTYP                                                
000632       WHEN 'A03'                                                         
000633          MOVE IA03-IDARTNR    TO W-IDARTNR-ART                           
000634          MOVE IA03-IDDC-REC   TO W-IDDC                                  
000635          MOVE IA03-IDARTNR    TO WS-IDARTNR                              
000636                                  W-IDARTNR                               
000637       WHEN 'A06'                                                         
000638          MOVE IA06-IDARTNR    TO W-IDARTNR-ART                           
000639          MOVE IA06-IDDC-REC   TO W-IDDC                                  
000640          MOVE IA06-IDARTNR    TO WS-IDARTNR                              
000641                                  W-IDARTNR                               
000642       WHEN 'A07'                                                         
000643          MOVE IA07-IDARTNR    TO W-IDARTNR-ART                           
000644          MOVE IA07-IDDC-REC   TO W-IDDC                                  
000645          MOVE IA07-IDARTNR    TO WS-IDARTNR                              
000646                                  W-IDARTNR                               
000647       WHEN 'A08'                                                         
000648          MOVE IA08-IDARTNR    TO W-IDARTNR-ART                           
000649          MOVE IA08-IDDC-REC   TO W-IDDC                                  
000650          MOVE IA08-IDARTNR    TO WS-IDARTNR                              
000651                                  W-IDARTNR                               
000652       WHEN 'A09'                                                         
000653          MOVE IA09-IDARTNR    TO W-IDARTNR-ART                           
000654          MOVE IA09-IDDC-SEND  TO W-IDDC                                  
000655          MOVE IA09-IDARTNR    TO WS-IDARTNR                              
000656                                  W-IDARTNR                               
000657       WHEN 'A11'                                                         
000658          MOVE IA11-IDARTNR    TO W-IDARTNR-ART                           
000659          MOVE IA11-IDDC-REC   TO W-IDDC                                  
000660          MOVE IA11-IDARTNR    TO WS-IDARTNR                              
000661                                  W-IDARTNR                               
000662       WHEN 'A12'                                                         
000663          MOVE IA12-IDARTNR    TO W-IDARTNR-ART                           
000664          MOVE IA12-IDDC-REC   TO W-IDDC                                  
000665          MOVE IA12-IDARTNR    TO WS-IDARTNR                              
000666                                  W-IDARTNR                               
000667       WHEN 'A13'                                                         
000668          MOVE IA13-IDARTNR    TO W-IDARTNR-ART                           
000669          MOVE IA13-IDDC-REC   TO W-IDDC                                  
000670          MOVE IA13-IDARTNR    TO WS-IDARTNR                              
000671                                  W-IDARTNR                               
000672       WHEN 'A18'                                                         
000673          MOVE IA18-IDARTNR    TO W-IDARTNR-ART                           
000674          MOVE IA18-IDDC-REC   TO W-IDDC                                  
000675          MOVE IA18-IDARTNR    TO WS-IDARTNR                              
000676                                  W-IDARTNR                               
000677                                  W-IDARTNR                               
000678       WHEN 'A19'                                                         
000679          MOVE IA19-IDARTNR    TO W-IDARTNR-ART                           
000680          MOVE IA19-IDDC-REC   TO W-IDDC                                  
000681          MOVE IA19-IDARTNR    TO WS-IDARTNR                              
000682                                  W-IDARTNR                               
000683     END-EVALUATE                                                         
000684     .                                                                    
000685     EJECT                                                                
000686 CAB-FLYTTA-TEST-ARTIKEL SECTION.                                         
000687                                                                          
000688     EVALUATE INFIL-IDPTYP                                                
000689       WHEN 'A03'                                                         
000690          MOVE IA03-IDARTNR TO TEST-IDARTNR                               
000691       WHEN 'A06'                                                         
000692          MOVE IA06-IDARTNR TO TEST-IDARTNR                               
000693       WHEN 'A07'                                                         
000694          MOVE IA07-IDARTNR TO TEST-IDARTNR                               
000695       WHEN 'A08'                                                         
000696          MOVE IA08-IDARTNR TO TEST-IDARTNR                               
000697       WHEN 'A09'                                                         
000698          MOVE IA09-IDARTNR TO TEST-IDARTNR                               
000699       WHEN 'A11'                                                         
000700          MOVE IA11-IDARTNR TO TEST-IDARTNR                               
000701       WHEN 'A12'                                                         
000702          MOVE IA12-IDARTNR TO TEST-IDARTNR                               
000703       WHEN 'A13'                                                         
000704          MOVE IA13-IDARTNR TO TEST-IDARTNR                               
000705       WHEN 'A18'                                                         
000706          MOVE IA18-IDARTNR TO TEST-IDARTNR                               
000707       WHEN 'A19'                                                         
000708          MOVE IA19-IDARTNR TO TEST-IDARTNR                               
000709     END-EVALUATE                                                         
000710     .                                                                    
000711     EJECT                                                                
000712 CAC-FLYTTA-TILL-IDLEVNR SECTION.                                         
000713                                                                          
000714     EVALUATE INFIL-IDPTYP                                                
000715       WHEN 'A03'                                                         
000716          MOVE IA03-IDDC-REC   TO WS-IDDC                                 
000717          IF NDC-US                                                       
000718             MOVE '9993'         TO W-IDLEVNR                             
000719          ELSE                                                            
000720             IF NDC-CA                                                    
000721                MOVE '9994'         TO W-IDLEVNR                          
000722             END-IF                                                       
000723          END-IF                                                          
000724       WHEN 'A06'                                                         
000725          MOVE IA06-IDDC-REC   TO WS-IDDC                                 
000726          IF NDC-US                                                       
000727             MOVE '9993'         TO W-IDLEVNR                             
000728          ELSE                                                            
000729             IF NDC-CA                                                    
000730                MOVE '9994'         TO W-IDLEVNR                          
000731             END-IF                                                       
000732          END-IF                                                          
000733       WHEN 'A07'                                                         
000734          MOVE IA07-IDDC-REC   TO WS-IDDC                                 
000735          IF NDC-US                                                       
000736             MOVE '9993'         TO W-IDLEVNR                             
000737          ELSE                                                            
000738             IF NDC-CA                                                    
000739                MOVE '9994'         TO W-IDLEVNR                          
000740             END-IF                                                       
000741          END-IF                                                          
000742       WHEN 'A08'                                                         
000743          MOVE IA08-IDDC-REC   TO WS-IDDC                                 
000744          IF NDC-US                                                       
000745             MOVE '9993'         TO W-IDLEVNR                             
000746          ELSE                                                            
000747            IF NDC-CA                                                     
000748                MOVE '9994'         TO W-IDLEVNR                          
000749            END-IF                                                        
000750          END-IF                                                          
000751       WHEN 'A09'                                                         
000752          MOVE IA09-IDDC-REC   TO WS-IDDC                                 
000753          IF NDC-US                                                       
000754             MOVE '9993'         TO W-IDLEVNR                             
000755          ELSE                                                            
000756            IF NDC-CA                                                     
000757                MOVE '9994'         TO W-IDLEVNR                          
000758            END-IF                                                        
000759          END-IF                                                          
000760       WHEN 'A11'                                                         
000761          MOVE IA11-IDDC-REC   TO WS-IDDC                                 
000762          IF NDC-US                                                       
000763             MOVE '9993'         TO W-IDLEVNR                             
000764          ELSE                                                            
000765            IF NDC-CA                                                     
000766                MOVE '9994'         TO W-IDLEVNR                          
000767            END-IF                                                        
000768          END-IF                                                          
000769       WHEN 'A12'                                                         
000770          MOVE IA12-IDDC-REC   TO WS-IDDC                                 
000771          IF NDC-US                                                       
000772             MOVE '9993'         TO W-IDLEVNR                             
000773          ELSE                                                            
000774            IF NDC-CA                                                     
000775                MOVE '9994'         TO W-IDLEVNR                          
000776            END-IF                                                        
000777          END-IF                                                          
000778       WHEN 'A13'                                                         
000779          MOVE IA13-IDDC-REC   TO WS-IDDC                                 
000780          IF NDC-US                                                       
000781             MOVE '9993'         TO W-IDLEVNR                             
000782          ELSE                                                            
000783            IF NDC-CA                                                     
000784                MOVE '9994'         TO W-IDLEVNR                          
000785            END-IF                                                        
000786          END-IF                                                          
000787       WHEN 'A18'                                                         
000788          MOVE IA18-IDDC-REC   TO WS-IDDC                                 
000789          IF NDC-US                                                       
000790             MOVE '9993'         TO W-IDLEVNR                             
000791          ELSE                                                            
000792            IF NDC-CA                                                     
000793                MOVE '9994'         TO W-IDLEVNR                          
000794            END-IF                                                        
000795          END-IF                                                          
000796       WHEN 'A19'                                                         
000797          MOVE IA19-IDDC-REC   TO WS-IDDC                                 
000798          IF NDC-US                                                       
000799             MOVE '9993'         TO W-IDLEVNR                             
000800          ELSE                                                            
000801            IF NDC-CA                                                     
000802                MOVE '9994'         TO W-IDLEVNR                          
000803            END-IF                                                        
000804          END-IF                                                          
000805     END-EVALUATE                                                         
000806     .                                                                    
000807     EJECT                                                                
000808 CAD-SKAPA-FELLISTA SECTION.                                              
000809                                                                          
000810     MOVE W-IDLEVNR       TO LIST-IDLEVNR                                 
000811     MOVE W-IDARTNR       TO LIST-IDARTNR                                 
000812     MOVE W-IDDC          TO LIST-IDDC                                    
000813                                                                          
000814     PERFORM S12-SKRIV-W56031                                             
000815                                                                          
000816     .                                                                    
000817     EJECT                                                                
000818 CAF-BEHANDLA-A11 SECTION.                                                
000819                                                                          
000820*****************************************************************         
000821**** BINNING, LOCAL DELIVERIES,                         *********         
000822**** PRISET TAS FRÅN WDK621 SAKNAS WDK621 DÅ SKAPAS     *********         
000823**** ETT PRIS PÅ 0                                      *********         
000824*****************************************************************         
000825     IF SEGMENT-FINNS                                                     
000826        MOVE ARTC-ART-KDPRODSL    TO IA11-KDPRODSL                        
000827     END-IF                                                               
000828     PERFORM IMS-GET-ARTC-WLARTC11                                        
000829     IF SEGMENT-FINNS                                                     
000830        MOVE ARTC-CLAG-KDPSLLOC   TO IA11-KDPSLLOC                        
000831     ELSE                                                                 
000832        MOVE ZERO                 TO IA11-KDPSLLOC                        
000833     END-IF                                                               
000834                                                                          
000835     PERFORM IMS-GET-WDK711                                               
000836     IF SEGMENT-FINNS                                                     
000837        MOVE SLAG-PRAVCOST        TO IA11-PRAVCOST-OLD                    
000838        MOVE SLAG-IDLEVNR         TO IA11-IDLEVNR                         
000839                                     W-IDLEVNR                            
000840     ELSE                                                                 
000841        MOVE ZERO                 TO IA11-PRAVCOST-OLD                    
000842        MOVE IA11-IDLEVNR         TO WS-IDLEVNR                           
000843        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
000844        MOVE 'MISSING PARTINFORMATION'                                    
000845                                  TO LIST-BEART                           
000846        PERFORM CAD-SKAPA-FELLISTA                                        
000847        MOVE SLAG-IDLEVNR         TO IA11-IDLEVNR                         
000848        MOVE ZERO                 TO W-IDLEVNR                            
000849        PERFORM IMS-GET-WDK711                                            
000850     END-IF                                                               
000851     PERFORM IMS-GET-ARTC-WLARTC21                                        
000852     IF SEGMENT-FINNS                                                     
000853**     HÄMTA LEVERANTÖRENS KURS          **************                   
000854        MOVE ARTC-PRL-KDVALISO    TO CURR-KDVALISO-ROW                    
000855        CALL W510CURR USING CURR-W510CURR 9305-PCB                        
000856        IF CURR-KDSVAR = ' '                                              
000857          MOVE CURR-PRKURS-NEW    TO W-PRKURS-FRAN                        
000858        ELSE                                                              
000859          MOVE +1                 TO W-PRKURS-FRAN                        
000860        END-IF                                                            
000861**     HÄMTA MOTTAGANDE LANDS KURS       **************                   
000862        IF IA11-IDFTG = '53'                                              
000863          MOVE 'USD'              TO CURR-KDVALISO-ROW                    
000864        END-IF                                                            
000865        IF IA11-IDFTG = '54'                                              
000866          MOVE 'CAD'              TO CURR-KDVALISO-ROW                    
000867        END-IF                                                            
000868        CALL W510CURR USING CURR-W510CURR 9305-PCB                        
000869        IF CURR-KDSVAR = ' '                                              
000870          MOVE CURR-PRKURS-NEW    TO W-PRKURS-TILL                        
000871        ELSE                                                              
000872          MOVE +1                 TO W-PRKURS-TILL                        
000873        END-IF                                                            
000874**     IMKÖPSPRIS I MOTTAGANDE LANDS VALUTA    ********                   
000875        COMPUTE IA11-PRARTBEU = ARTC-PRL-PRARTBEL-PR *                    
000876                                W-PRKURS-FRAN / W-PRKURS-TILL             
000877                                                                          
000878        MOVE ARTC-PRL-PRARTBEL-PR TO AVG-PRARTNTO                         
000879                                     AVG-PRARTBEL                         
000880        MOVE ARTC-PRL-KDVALISO    TO AVG-KDVALISO                         
000881        MOVE 040                  TO AVG-KDCALL                           
000882        MOVE ZERO                 TO AVG-PRKURS                           
000883        MOVE IA11-PRAVCOST-OLD    TO AVG-PRAVCOST-OLD                     
000884        PERFORM IMS-GET-WDK711                                            
000885        COMPUTE IA11-KVLS-OLD = IA11-KVLS-OLD + SLAG-KVLS                 
000886        MOVE IA11-KVLS-OLD        TO AVG-KVLS-OLD                         
000887        MOVE IA11-KVANTMOT        TO AVG-KVANTMOT                         
000888        MOVE IA11-KDPSLLOC        TO AVG-KDPSLLOC                         
000889        MOVE IA11-IDDC-REC        TO AVG-IDDC                             
000890        MOVE ZEROS                TO AVG-KDPRODSL                         
000891                                     AVG-IDFKNGRP                         
000892        CALL W510AVG USING AVG-W510AVG 9305-PCB                           
000893                           AVG-WDB6-PCB                                   
000894        IF AVG-KDSVAR = ' '                                               
000895          MOVE AVG-PRAVCOST-NEW   TO SLAG-PRAVCOST                        
000896                                     IA11-PRAVCOST                        
000897        ELSE                                                              
000898          MOVE 0.01               TO IA11-PRAVCOST                        
000899        END-IF                                                            
000900                                                                          
000901        MOVE AVG-REMARKUP         TO IA11-REMARKUP                        
000902        PERFORM S06-ANROP-MSGI                                            
000903        MOVE MSGI-TILOKDAT        TO W-TILLOKDAT                          
000904        MOVE W-TILLOKDAT          TO SLAG-TIAVCOST                        
000905        PERFORM IMS-REPL-WDK7                                             
000906     ELSE                                                                 
000907        MOVE ZERO                 TO AVG-PRARTNTO                         
000908                                     AVG-PRARTBEL                         
000909                                     IA11-PRARTBEU                        
000910        MOVE 'SEK'                TO AVG-KDVALISO                         
000911        MOVE 'MISSING PURCHASE ORDER PRICE'                               
000912                                  TO LIST-BEART                           
000913        PERFORM CAD-SKAPA-FELLISTA                                        
000914        MOVE 1.00                 TO IA11-REMARKUP                        
000915        MOVE IA11-PRAVCOST-OLD    TO IA11-PRAVCOST                        
000916        COMPUTE IA11-KVLS-OLD = IA11-KVLS-OLD                             
000917                                   + SLAG-KVLS                            
000918     END-IF                                                               
000919     .                                                                    
000920     EJECT                                                                
000921 CAG-BEHANDLA-A13 SECTION.                                                
000922*****************************************************************         
000923**** BINNING, VOR DELIVERIES ************************************         
000924*****************************************************************         
000925     IF SEGMENT-FINNS                                                     
000926       MOVE ARTC-ART-KDPRODSL     TO IA13-KDPRODSL                        
000927     END-IF                                                               
000928     PERFORM IMS-GET-ARTC-WLARTC11                                        
000929     IF SEGMENT-FINNS                                                     
000930        MOVE ARTC-CLAG-KDPSLLOC   TO IA13-KDPSLLOC                        
000931     ELSE                                                                 
000932        MOVE ZERO                 TO IA13-KDPSLLOC                        
000933     END-IF                                                               
000934                                                                          
000935     PERFORM IMS-GET-WDK711                                               
000936     IF SEGMENT-FINNS                                                     
000937        MOVE SLAG-PRAVCOST        TO IA13-PRAVCOST-OLD                    
000938        MOVE SLAG-IDLEVNR         TO W-IDLEVNR                            
000939     ELSE                                                                 
000940        MOVE ZERO                 TO IA13-PRAVCOST-OLD                    
000941        MOVE SPACE                TO WS-IDLEVNR                           
000942        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
000943        MOVE 'MISSING PART AND PARTINFORMATION'                           
000944                                  TO LIST-BEART                           
000945        PERFORM CAD-SKAPA-FELLISTA                                        
000946        MOVE SPACE                TO W-IDLEVNR                            
000947        PERFORM IMS-GET-WDK711                                            
000948     END-IF                                                               
000949     PERFORM IMS-GET-ARTC-WLARTC21                                        
000950     IF SEGMENT-FINNS                                                     
000951       MOVE ARTC-PRL-KDVALISO     TO CURR-KDVALISO-ROW                    
000952       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
000953       IF CURR-KDSVAR = ' '                                               
000954         COMPUTE IA13-PRARTNTO = ARTC-PRL-PRARTBEL-PR *                   
000955                                    CURR-PRKURS-NEW                       
000956       ELSE                                                               
000957         MOVE +0.01               TO IA13-PRARTNTO                        
000958       END-IF                                                             
000959       MOVE 060                   TO AVG-KDCALL                           
000960       MOVE IA13-PRAVCOST-OLD     TO AVG-PRAVCOST-OLD                     
000961       PERFORM IMS-GET-WDK711                                             
000962       COMPUTE IA13-KVLS-OLD = IA13-KVLS-OLD + SLAG-KVLS                  
000963       MOVE IA13-KVLS-OLD         TO AVG-KVLS-OLD                         
000964       MOVE IA13-PRARTNTO         TO AVG-PRARTNTO                         
000965       MOVE IA13-PRKURS           TO AVG-PRKURS                           
000966       MOVE IA13-KVLEVART         TO AVG-KVLEVART                         
000967       MOVE IA13-KDPSLLOC         TO AVG-KDPSLLOC                         
000968       MOVE IA13-IDDC-REC         TO AVG-IDDC                             
000969       MOVE ZEROS                 TO AVG-KDPRODSL                         
000970                                     AVG-IDFKNGRP                         
000971       CALL W510AVG USING AVG-W510AVG 9305-PCB                            
000972                          AVG-WDB6-PCB                                    
000973       IF AVG-KDSVAR = ' '                                                
000974         MOVE AVG-PRAVCOST-NEW    TO SLAG-PRAVCOST                        
000975                                     IA13-PRAVCOST                        
000976       ELSE                                                               
000977         MOVE 0.01                TO IA13-PRAVCOST                        
000978       END-IF                                                             
000979       MOVE AVG-REMARKUP          TO IA13-REMARKUP                        
000980       PERFORM S06-ANROP-MSGI                                             
000981       MOVE MSGI-TILOKDAT         TO W-TILLOKDAT                          
000982       MOVE W-TILLOKDAT           TO SLAG-TIAVCOST                        
000983       PERFORM IMS-REPL-WDK7                                              
000984     ELSE                                                                 
000985        MOVE ZERO                 TO IA13-PRARTNTO                        
000986        MOVE IA13-PRAVCOST-OLD    TO IA13-PRAVCOST                        
000987        MOVE 1.00                 TO IA13-REMARKUP                        
000988        COMPUTE IA13-KVLS-OLD = IA13-KVLS-OLD + SLAG-KVLS                 
000989     END-IF                                                               
000990     .                                                                    
000991     EJECT                                                                
000992 CAH-BEHANDLA-A03 SECTION.                                                
000993     IF SEGMENT-FINNS                                                     
000994        MOVE ARTC-ART-KDPRODSL                                            
000995                          TO IA03-KDPRODSL                                
000996     END-IF                                                               
000997     PERFORM IMS-GET-ARTC-WLARTC11                                        
000998     IF SEGMENT-FINNS                                                     
000999        MOVE ARTC-CLAG-KDPSLLOC                                           
001000                          TO IA03-KDPSLLOC                                
001001     ELSE                                                                 
001002        MOVE ZERO         TO IA03-KDPSLLOC                                
001003     END-IF                                                               
001004     IF IA03-KDEKOHT = 'T10'                                              
001005        PERFORM CAI-BEHANDLA-A03-TI10                                     
001006     ELSE                                                                 
001007        IF IA03-KDEKOHT = 'T30'                                           
001008           PERFORM CAJ-BEHANDLA-A03-TI30                                  
001009        ELSE                                                              
001010           IF IA03-KDEKOHT = 'T40'                                        
001011              PERFORM CAK-BEHANDLA-A03-TI40                               
001012           ELSE                                                           
001013              IF IA03-KDEKOHT = 'T50'                                     
001014                 PERFORM CAL-BEHANDLA-A03-TI50                            
001015              END-IF                                                      
001016           END-IF                                                         
001017        END-IF                                                            
001018     END-IF                                                               
001019     IF SEGMENT-FINNS                                                     
001020        MOVE ARTC-ART-KDPRODSL                                            
001021                       TO IA03-KDPRODSL                                   
001022     END-IF                                                               
001023     PERFORM IMS-GET-ARTC-WLARTC11                                        
001024     IF SEGMENT-FINNS                                                     
001025        MOVE ARTC-CLAG-KDPSLLOC                                           
001026                           TO IA03-KDPSLLOC                               
001027     ELSE                                                                 
001028        MOVE ZERO                                                         
001029                           TO IA03-KDPSLLOC                               
001030     END-IF                                                               
001031     MOVE SPACE            TO IA03-FLSLUT                                 
001032     .                                                                    
001033     EJECT                                                                
001034                                                                          
001035 CAI-BEHANDLA-A03-TI10 SECTION.                                           
001036*****************************************************************         
001037**** FÖR HÄNDELSETYP T10 (BINNING, REFILL GOODS)        *********         
001038**** PRISET TAS FRÅN WDK621 SAKNAS WDK621 DÅ SKAPAS     *********         
001039**** ETT PRIS PÅ 0                                      *********         
001040*****************************************************************         
001041     PERFORM IMS-GET-WDK711                                               
001042     IF SEGMENT-FINNS                                                     
001043        MOVE SLAG-PRAVCOST   TO IA03-PRAVCOST-OLD                         
001044        MOVE SLAG-IDLEVNR    TO W-IDLEVNR                                 
001045     ELSE                                                                 
001046        MOVE ZERO                 TO IA03-PRAVCOST-OLD                    
001047        MOVE SPACE                TO WS-IDLEVNR                           
001048        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
001049        MOVE 'MISSING PARTINFORMATION'                                    
001050                                  TO LIST-BEART                           
001051        PERFORM CAD-SKAPA-FELLISTA                                        
001052        MOVE SPACE                TO W-IDLEVNR                            
001053        PERFORM IMS-GET-WDK711                                            
001054     END-IF                                                               
001055     PERFORM IMS-GET-ARTC-WLARTC21                                        
001056     IF SEGMENT-FINNS                                                     
001057       MOVE ARTC-PRL-KDVALISO     TO CURR-KDVALISO-ROW                    
001058       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
001059       IF CURR-KDSVAR = ' '                                               
001060         COMPUTE IA03-PRARTNTO = ARTC-PRL-PRARTBEL-PR *                   
001061                                     CURR-PRKURS-NEW                      
001062       ELSE                                                               
001063         MOVE 0.01                TO IA03-PRARTNTO                        
001064       END-IF                                                             
001065       MOVE 010                   TO AVG-KDCALL                           
001066       MOVE IA03-PRAVCOST-OLD     TO AVG-PRAVCOST-OLD                     
001067       COMPUTE IA03-KVLS-OLD = IA03-KVLS-OLD                              
001068                                 + SLAG-KVLS                              
001069       MOVE IA03-KVLS-OLD         TO AVG-KVLS-OLD                         
001070       MOVE IA03-KDPSLLOC         TO AVG-KDPSLLOC                         
001071       MOVE IA03-PRARTNTO         TO AVG-PRARTNTO                         
001072       MOVE IA03-PRKURS           TO AVG-PRKURS                           
001073       MOVE IA03-KVANTMOT         TO AVG-KVANTMOT                         
001074       MOVE IA03-IDDC-REC         TO AVG-IDDC                             
001075       MOVE ZEROS                 TO AVG-KDPRODSL                         
001076                                     AVG-IDFKNGRP                         
001077       PERFORM IMS-GET-WDK711                                             
001078       CALL W510AVG USING AVG-W510AVG 9305-PCB                            
001079                          AVG-WDB6-PCB                                    
001080       IF AVG-KDSVAR = ' '                                                
001081         MOVE AVG-PRAVCOST-NEW    TO SLAG-PRAVCOST                        
001082                                     IA03-PRAVCOST                        
001083       ELSE                                                               
001084         MOVE 0.01                TO IA03-PRAVCOST                        
001085       END-IF                                                             
001086       MOVE AVG-REMARKUP          TO IA03-REMARKUP                        
001087       PERFORM S06-ANROP-MSGI                                             
001088       MOVE MSGI-TILOKDAT         TO W-TILLOKDAT                          
001089       MOVE W-TILLOKDAT           TO SLAG-TIAVCOST                        
001090       PERFORM IMS-REPL-WDK7                                              
001091     ELSE                                                                 
001092       MOVE ZERO                  TO IA03-PRARTNTO                        
001093       MOVE IA03-PRAVCOST-OLD     TO IA03-PRAVCOST                        
001094       MOVE 1.00                  TO IA03-REMARKUP                        
001095       COMPUTE IA03-KVLS-OLD = IA03-KVLS-OLD + SLAG-KVLS                  
001096     END-IF                                                               
001097     .                                                                    
001098     EJECT                                                                
001099                                                                          
001100 CAJ-BEHANDLA-A03-TI30 SECTION.                                           
001101*****************************************************************         
001102**** TRANSFER WITHIN THE USA ************************************         
001103*****************************************************************         
001104     MOVE IA03-IDDC-SEND TO W-IDDC                                        
001105     PERFORM IMS-GET-WDK711                                               
001106*****************************************************************         
001107**** LÄSER WDK7 MED SÄNDANDE DC *********************************         
001108*****************************************************************         
001109     IF SEGMENT-FINNS                                                     
001110        MOVE SLAG-IDLEVNR TO W-IDLEVNR                                    
001111        MOVE SLAG-PRAVCOST TO IA03-PRARTNTO                               
001112     ELSE                                                                 
001113*****************************************************************         
001114**** LÄSER WDK7 MED MOTTAGANDE DC *******************************         
001115*****************************************************************         
001116        MOVE IA03-IDDC-REC  TO W-IDDC                                     
001117        PERFORM IMS-GET-WDK711                                            
001118        IF SEGMENT-FINNS                                                  
001119           MOVE SLAG-PRAVCOST                                             
001120                       TO IA03-PRAVCOST-OLD                               
001121           MOVE SLAG-IDLEVNR TO W-IDLEVNR                                 
001122           MOVE SLAG-PRAVCOST TO IA03-PRARTNTO                            
001123        ELSE                                                              
001124           MOVE ZERO TO IA03-PRAVCOST-OLD                                 
001125           MOVE SPACE     TO WS-IDLEVNR                                   
001126           MOVE SPACE     TO W-IDLEVNR                                    
001127           MOVE IA03-IDDC-REC TO W-IDDC                                   
001128           PERFORM NB-NYA-SDC21-SEGMENT-WDK7                              
001129           MOVE 'MISSING PARTINFORMATION'                                 
001130                      TO LIST-BEART                                       
001131           PERFORM CAD-SKAPA-FELLISTA                                     
001132        END-IF                                                            
001133     END-IF                                                               
001134     MOVE IA03-IDDC-REC           TO W-IDDC                               
001135                                     AVG-IDDC                             
001136     PERFORM IMS-GET-WDK711                                               
001137     IF SEGMENT-SAKNAS                                                    
001138        MOVE ZERO                 TO IA03-PRARTNTO                        
001139                                     IA03-PRAVCOST-OLD                    
001140        MOVE SPACE                TO WS-IDLEVNR                           
001141                                     W-IDLEVNR                            
001142        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
001143        MOVE 'MISSING PARTINFORMATION'                                    
001144                                  TO LIST-BEART                           
001145        PERFORM CAD-SKAPA-FELLISTA                                        
001146        PERFORM IMS-GET-WDK711                                            
001147     END-IF                                                               
001148     MOVE SLAG-PRAVCOST           TO IA03-PRAVCOST-OLD                    
001149     MOVE IA03-PRAVCOST-OLD       TO AVG-PRAVCOST-OLD                     
001150     MOVE 020                     TO AVG-KDCALL                           
001151     COMPUTE IA03-KVLS-OLD = IA03-KVLS-OLD + SLAG-KVLS                    
001152     MOVE IA03-KVLS-OLD           TO AVG-KVLS-OLD                         
001153     MOVE IA03-PRARTNTO           TO AVG-PRARTNTO                         
001154     MOVE IA03-KVANTMOT           TO AVG-KVANTMOT                         
001155     MOVE IA03-KDPSLLOC           TO AVG-KDPSLLOC                         
001156     MOVE ZEROS                   TO AVG-KDPRODSL                         
001157                                     AVG-IDFKNGRP                         
001158     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
001159                        AVG-WDB6-PCB                                      
001160     IF AVG-KDSVAR = ' '                                                  
001161       MOVE AVG-PRAVCOST-NEW      TO SLAG-PRAVCOST                        
001162                                     IA03-PRAVCOST                        
001163     ELSE                                                                 
001164       MOVE 0.01                  TO IA03-PRAVCOST                        
001165     END-IF                                                               
001166     MOVE AVG-REMARKUP            TO IA03-REMARKUP                        
001167     PERFORM S06-ANROP-MSGI                                               
001168     MOVE MSGI-TILOKDAT           TO W-TILLOKDAT                          
001169     MOVE W-TILLOKDAT             TO SLAG-TIAVCOST                        
001170     PERFORM IMS-REPL-WDK7                                                
001171     .                                                                    
001172     EJECT                                                                
001173 CAK-BEHANDLA-A03-TI40 SECTION.                                           
001174*****************************************************************         
001175**** TRANSFER BETWEEN CANADA/USA ********************************         
001176*****************************************************************         
001177     PERFORM IMS-GET-WDK711                                               
001178*****************************************************************         
001179**** LÄSER WDK7 MED MOTTAGANDE DC *******************************         
001180*****************************************************************         
001181     IF SEGMENT-FINNS                                                     
001182        MOVE SLAG-PRAVCOST                                                
001183                                   TO IA03-PRAVCOST-OLD                   
001184        MOVE SLAG-IDLEVNR          TO W-IDLEVNR                           
001185**** LÄSER WDK7 MED SÄNDANDE DC *********************************         
001186        MOVE IA03-IDDC-SEND        TO W-IDDC                              
001187        PERFORM IMS-GET-WDK711                                            
001188        IF SEGMENT-FINNS                                                  
001189           MOVE SLAG-PRAVCOST      TO IA03-PRARTNTO                       
001190        ELSE                                                              
001191           MOVE ZERO               TO IA03-PRARTNTO                       
001192        END-IF                                                            
001193     ELSE                                                                 
001194        MOVE IA03-IDDC-SEND        TO W-IDDC                              
001195        PERFORM IMS-GET-WDK711                                            
001196*****************************************************************         
001197**** LÄSER WDK7 MED SÄNDANDE DC *********************************         
001198*****************************************************************         
001199        IF SEGMENT-FINNS                                                  
001200           MOVE SLAG-IDLEVNR       TO W-IDLEVNR                           
001201        ELSE                                                              
001202           MOVE SPACE              TO W-IDLEVNR                           
001203        END-IF                                                            
001204        MOVE ZERO                  TO IA03-PRARTNTO                       
001205                                      IA03-PRAVCOST-OLD                   
001206        MOVE SPACE                 TO WS-IDLEVNR                          
001207                                      W-IDLEVNR                           
001208        MOVE IA03-IDDC-REC         TO W-IDDC                              
001209        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
001210        MOVE 'MISSING PARTINFORMATION'                                    
001211                                   TO LIST-BEART                          
001212        PERFORM CAD-SKAPA-FELLISTA                                        
001213     END-IF                                                               
001214     MOVE IA03-IDDC-REC            TO W-IDDC                              
001215                                      AVG-IDDC                            
001216     PERFORM IMS-GET-WDK711                                               
001217     MOVE 030                      TO AVG-KDCALL                          
001218     MOVE IA03-PRAVCOST-OLD        TO AVG-PRAVCOST-OLD                    
001219     COMPUTE IA03-KVLS-OLD = IA03-KVLS-OLD + SLAG-KVLS                    
001220     MOVE IA03-KVLS-OLD            TO AVG-KVLS-OLD                        
001221     MOVE IA03-PRARTNTO            TO AVG-PRARTNTO                        
001222     MOVE IA03-PRKURS  TO AVG-PRKURS                                      
001223     MOVE IA03-KVANTMOT TO AVG-KVANTMOT                                   
001224     MOVE IA03-KDPSLLOC TO AVG-KDPSLLOC                                   
001225     MOVE ZEROS                TO AVG-KDPRODSL                            
001226                                  AVG-IDFKNGRP                            
001227     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
001228                        AVG-WDB6-PCB                                      
001229     IF AVG-KDSVAR = ' '                                                  
001230       MOVE AVG-PRAVCOST-NEW       TO SLAG-PRAVCOST                       
001231                                      IA03-PRAVCOST                       
001232     ELSE                                                                 
001233       MOVE 0.01                   TO IA03-PRAVCOST                       
001234     END-IF                                                               
001235     MOVE AVG-REMARKUP             TO IA03-REMARKUP                       
001236     PERFORM S06-ANROP-MSGI                                               
001237     MOVE MSGI-TILOKDAT            TO W-TILLOKDAT                         
001238     MOVE W-TILLOKDAT              TO SLAG-TIAVCOST                       
001239     PERFORM IMS-REPL-WDK7                                                
001240     .                                                                    
001241     EJECT                                                                
001242                                                                          
001243 CAL-BEHANDLA-A03-TI50 SECTION.                                           
001244*****************************************************************         
001245**** TRANSFER BETWEEN USA/CANADA ********************************         
001246*****************************************************************         
001247     PERFORM IMS-GET-WDK711                                               
001248*****************************************************************         
001249**** LÄSER WDK7 MED MOTTAGANDE DC *******************************         
001250*****************************************************************         
001251     IF SEGMENT-FINNS                                                     
001252        MOVE SLAG-PRAVCOST                                                
001253                                   TO IA03-PRAVCOST-OLD                   
001254        MOVE SLAG-IDLEVNR          TO W-IDLEVNR                           
001255                                                                          
001256        MOVE IA03-IDDC-SEND        TO W-IDDC                              
001257        PERFORM IMS-GET-WDK711                                            
001258        IF SEGMENT-FINNS                                                  
001259           MOVE SLAG-PRAVCOST      TO IA03-PRARTNTO                       
001260        ELSE                                                              
001261           MOVE ZERO               TO IA03-PRARTNTO                       
001262        END-IF                                                            
001263     ELSE                                                                 
001264        MOVE IA03-IDDC-SEND        TO W-IDDC                              
001265        PERFORM IMS-GET-WDK711                                            
001266*****************************************************************         
001267**** LÄSER WDK7 MED SÄNDANDE DC *********************************         
001268*****************************************************************         
001269        IF SEGMENT-FINNS                                                  
001270           MOVE SLAG-IDLEVNR       TO W-IDLEVNR                           
001271        ELSE                                                              
001272           MOVE SPACE              TO W-IDLEVNR                           
001273        END-IF                                                            
001274        MOVE ZERO                  TO IA03-PRAVCOST-OLD                   
001275        MOVE SPACE                 TO WS-IDLEVNR                          
001276        MOVE IA03-IDDC-REC         TO W-IDDC                              
001277        PERFORM NB-NYA-SDC21-SEGMENT-WDK7                                 
001278        MOVE 'MISSING PARTINFORMATION'                                    
001279                                   TO LIST-BEART                          
001280        PERFORM CAD-SKAPA-FELLISTA                                        
001281     END-IF                                                               
001282     MOVE IA03-IDDC-REC            TO W-IDDC                              
001283                                      AVG-IDDC                            
001284     PERFORM IMS-GET-WDK711                                               
001285     MOVE 030                      TO AVG-KDCALL                          
001286     MOVE IA03-PRAVCOST-OLD                                               
001287                                   TO AVG-PRAVCOST-OLD                    
001288     COMPUTE IA03-KVLS-OLD = IA03-KVLS-OLD                                
001289                                           + SLAG-KVLS                    
001290     MOVE IA03-KVLS-OLD            TO AVG-KVLS-OLD                        
001291     MOVE IA03-PRARTNTO            TO AVG-PRARTNTO                        
001292     MOVE IA03-PRKURS              TO AVG-PRKURS                          
001293     MOVE IA03-KVANTMOT            TO AVG-KVANTMOT                        
001294     MOVE IA03-KDPSLLOC            TO AVG-KDPSLLOC                        
001295     MOVE ZEROS                    TO AVG-KDPRODSL                        
001296                                      AVG-IDFKNGRP                        
001297     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
001298                        AVG-WDB6-PCB                                      
001299     IF AVG-KDSVAR = ' '                                                  
001300       MOVE AVG-PRAVCOST-NEW       TO SLAG-PRAVCOST                       
001301                                      IA03-PRAVCOST                       
001302     ELSE                                                                 
001303       MOVE 0.01                   TO IA03-PRAVCOST                       
001304     END-IF                                                               
001305     MOVE AVG-REMARKUP             TO IA03-REMARKUP                       
001306     PERFORM S06-ANROP-MSGI                                               
001307     MOVE MSGI-TILOKDAT            TO W-TILLOKDAT                         
001308     MOVE W-TILLOKDAT              TO SLAG-TIAVCOST                       
001309     PERFORM IMS-REPL-WDK7                                                
001310     .                                                                    
001311     EJECT                                                                
001312                                                                          
001313 CB-FLYTTA-SKRIV-W56030 SECTION.                                          
001314                                                                          
001315     EVALUATE INFIL-IDPTYP                                                
001316       WHEN 'A01'                                                         
001317          MOVE IA01-W510A01 TO UA01-W510A01                               
001318          WRITE A01-POST FROM UTFIL-AREA                                  
001319       WHEN 'AX2'                                                         
001320          MOVE IAX2-W510AX2 TO UA02-W510A02                               
001321          MOVE 'A02'        TO UA02-IDPTYP                                
001322          WRITE A02-POST FROM UTFIL-AREA                                  
001323       WHEN 'A03'                                                         
001324          MOVE IA03-W510A03 TO UA03-W510A03                               
001325          WRITE A03-POST FROM UTFIL-AREA                                  
001326       WHEN 'A04'                                                         
001327          MOVE IA04-W510A04 TO UA04-W510A04                               
001328          WRITE A04-POST FROM UTFIL-AREA                                  
001329       WHEN 'A05'                                                         
001330          MOVE IA05-W510A05 TO UA05-W510A05                               
001331          WRITE A05-POST FROM UTFIL-AREA                                  
001332       WHEN 'A06'                                                         
001333          MOVE IA06-W510A06 TO UA06-W510A06                               
001334          WRITE A06-POST FROM UTFIL-AREA                                  
001335       WHEN 'A07'                                                         
001336          MOVE IA07-W510A07 TO UA07-W510A07                               
001337          WRITE A07-POST FROM UTFIL-AREA                                  
001338       WHEN 'A08'                                                         
001339          MOVE IA08-W510A08 TO UA08-W510A08                               
001340          WRITE A08-POST FROM UTFIL-AREA                                  
001341       WHEN 'A09'                                                         
001342          MOVE IA09-W510A09 TO UA09-W510A09                               
001343          WRITE A09-POST FROM UTFIL-AREA                                  
001344       WHEN 'A10'                                                         
001345          MOVE IA10-W510A10 TO UA10-W510A10                               
001346          WRITE A10-POST FROM UTFIL-AREA                                  
001347       WHEN 'A11'                                                         
001348          MOVE IA11-W510A11 TO UA11-W510A11                               
001349          WRITE A11-POST FROM UTFIL-AREA                                  
001350       WHEN 'A12'                                                         
001351          MOVE IA12-W510A12 TO UA12-W510A12                               
001352          WRITE A12-POST FROM UTFIL-AREA                                  
001353       WHEN 'A13'                                                         
001354          MOVE IA13-W510A13 TO UA13-W510A13                               
001355          WRITE A13-POST FROM UTFIL-AREA                                  
001356       WHEN 'A14'                                                         
001357          MOVE IA14-W510A14 TO UA14-W510A14                               
001358          WRITE A14-POST FROM UTFIL-AREA                                  
001359       WHEN 'A15'                                                         
001360          MOVE IA15-W510A15 TO UA15-W510A15                               
001361          WRITE A15-POST FROM UTFIL-AREA                                  
001362       WHEN 'A16'                                                         
001363          MOVE IA16-W510A16 TO UA16-W510A16                               
001364          WRITE A16-POST FROM UTFIL-AREA                                  
001365       WHEN 'A17'                                                         
001366          MOVE IA17-W510A17 TO UA17-W510A17                               
001367          WRITE A17-POST FROM UTFIL-AREA                                  
001368       WHEN 'A18'                                                         
001369          MOVE IA18-W510A18 TO UA18-W510A18                               
001370          WRITE A18-POST FROM UTFIL-AREA                                  
001371       WHEN 'A19'                                                         
001372          MOVE IA19-W510A19 TO UA19-W510A19                               
001373          WRITE A19-POST FROM UTFIL-AREA                                  
001374     END-EVALUATE                                                         
001375                                                                          
001376     IF INFIL-IDPTYP = 'A01' OR 'AX2' OR 'A03' OR 'A04' OR 'A05'          
001377                    OR 'A06' OR 'A07' OR 'A08' OR 'A09' OR 'A10'          
001378                    OR 'A11' OR 'A12' OR 'A13' OR 'A14' OR 'A15'          
001379                    OR 'A16' OR 'A17' OR 'A18' OR 'A19'                   
001380       MOVE INFIL-IDPTYP TO POSTSUM-TRANSTYP                              
001381       MOVE 'W56030 '    TO POSTSUM-FDNAMN                                
001382       MOVE 'W56030D2'   TO POSTSUM-DDNAMN2                               
001383       CALL POSTSUM USING POSTSUM-PARM                                    
001384     END-IF                                                               
001385     .                                                                    
001386     EJECT                                                                
001387 CD-SKAPA-BYTESOBJEKT SECTION.                                            
001388     EVALUATE INFIL-IDPTYP                                                
001389       WHEN 'A03'                                                         
001390             ADD +6000 TO   IA03-IDARTNR                                  
001391       WHEN 'A06'                                                         
001392             ADD +6000 TO   IA06-IDARTNR                                  
001393       WHEN 'A07'                                                         
001394             ADD +6000 TO   IA07-IDARTNR                                  
001395       WHEN 'A08'                                                         
001396             ADD +6000 TO   IA08-IDARTNR                                  
001397       WHEN 'A09'                                                         
001398             ADD +6000 TO   IA09-IDARTNR                                  
001399       WHEN 'A11'                                                         
001400             ADD +6000 TO   IA11-IDARTNR                                  
001401       WHEN 'A12'                                                         
001402             ADD +6000 TO   IA12-IDARTNR                                  
001403       WHEN 'A13'                                                         
001404             ADD +6000 TO   IA13-IDARTNR                                  
001405       WHEN 'A18'                                                         
001406             ADD +6000 TO   IA18-IDARTNR                                  
001407       WHEN 'A19'                                                         
001408             ADD +6000 TO   IA19-IDARTNR                                  
001409     END-EVALUATE                                                         
001410                                                                          
001411     .                                                                    
001412     EJECT                                                                
001413 CE-SKAPA-RADIOOBJEKT SECTION.                                            
001414     EVALUATE INFIL-IDPTYP                                                
001415       WHEN 'A03'                                                         
001416             ADD +1000 TO   IA03-IDARTNR                                  
001417       WHEN 'A06'                                                         
001418             ADD +1000 TO   IA06-IDARTNR                                  
001419       WHEN 'A07'                                                         
001420             ADD +1000 TO   IA07-IDARTNR                                  
001421       WHEN 'A08'                                                         
001422             ADD +1000 TO   IA08-IDARTNR                                  
001423       WHEN 'A09'                                                         
001424             ADD +1000 TO   IA09-IDARTNR                                  
001425       WHEN 'A11'                                                         
001426             ADD +1000 TO   IA11-IDARTNR                                  
001427       WHEN 'A12'                                                         
001428             ADD +1000 TO   IA12-IDARTNR                                  
001429       WHEN 'A13'                                                         
001430             ADD +1000 TO   IA13-IDARTNR                                  
001431       WHEN 'A18'                                                         
001432             ADD +1000 TO   IA18-IDARTNR                                  
001433       WHEN 'A19'                                                         
001434             ADD +1000 TO   IA19-IDARTNR                                  
001435     END-EVALUATE                                                         
001436                                                                          
001437     .                                                                    
001438     EJECT                                                                
001439 NB-NYA-SDC21-SEGMENT-WDK7 SECTION.                                       
001440* LÄGG UPP NYA SEGMENT                                                    
001441                                                                          
001442     MOVE ALL '+'      TO WDK7-W005WDK7                                   
001443     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
001444     MOVE WS-IDARTNR   TO WDK7-IDARTNR-KFB                                
001445     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
001446                          WDK7-IDDC                                       
001447     MOVE 'N'          TO WDK7-FLREFILL                                   
001448                                                                          
001449     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
001450     .                                                                    
001451     EJECT                                                                
001452 Z-FINIT SECTION.                                                         
001453                                                                          
001454     CLOSE INFIL W56030                                                   
001455                 W56031                                                   
001456                                                                          
001457     MOVE 'S' TO POSTSUM-OPKOD                                            
001458     CALL POSTSUM USING POSTSUM-PARM                                      
001459     .                                                                    
001460     EJECT                                                                
001461 S01-LAES-INFIL   SECTION.                                                
001462     SKIP2                                                                
001463     READ INFIL INTO INFIL-AREA                                           
001464     AT END                                                               
001465        SET END-OF-INFIL TO TRUE                                          
001466                                                                          
001467     NOT AT END                                                           
001468        MOVE 'INFIL '     TO POSTSUM-FDNAMN                               
001469        MOVE 'W56030D1'   TO POSTSUM-DDNAMN2                              
001470        MOVE INFIL-IDPTYP TO POSTSUM-TRANSTYP                             
001471        CALL POSTSUM USING POSTSUM-PARM                                   
001472                                                                          
001473        ADD 1 TO W-INFIL-KVPOST-IN                                        
001474     END-READ                                                             
001475     .                                                                    
001476     EJECT                                                                
001477 S06-ANROP-MSGI SECTION.                                                  
001478                                                                          
001479     MOVE ALL '+'           TO MSGI-WMSGINIT                              
001480     MOVE '013'             TO MSGI-KDCALL                                
001481     MOVE W-IDDC            TO W-IDDC-MSGI                                
001482     MOVE W-IDDCTEXT-MSGI   TO MSGI-IDUSER                                
001483     MOVE 'W560'            TO MSGI-IDTRANS                               
001484     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
001485                                                                          
001486     .                                                                    
001487     EJECT                                                                
001488                                                                          
001489 S12-SKRIV-W56031 SECTION.                                                
001490                                                                          
001491     WRITE LIST-POST FROM LIST-AREA                                       
001492                                                                          
001493     MOVE UTFIL-IDPTYP TO POSTSUM-TRANSTYP                                
001494     MOVE 'W56031 '    TO POSTSUM-FDNAMN                                  
001495     MOVE 'W56030D3'   TO POSTSUM-DDNAMN2                                 
001496     CALL POSTSUM USING POSTSUM-PARM                                      
001497     .                                                                    
001498     EJECT                                                                
001499 X-TAG-CHECKPOINT   SECTION.                                              
001500                                                                          
001501* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
001502* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
001503     PERFORM IMS-CHECKPOINT                                               
001504     MOVE ZERO TO CHKP-ANT                                                
001505* --- LÄS OM DATABAS OM DET BEHÖVS                                        
001506     .                                                                    
001507     EJECT                                                                
001508* --- IMS SEKTIONER ---                                                   
001509     SKIP3                                                                
001510     EJECT                                                                
001511 IMS-GET-WDK711 SECTION.                                                  
001512     SKIP2                                                                
001513     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-ART-X ')'                     
001514           DELIMITED BY SIZE INTO SSA1                                    
001515     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001516           DELIMITED BY SIZE INTO SSA2                                    
001517     MOVE '  GE' TO GODK-STATUSKODER                                      
001518     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
001519     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001520     PERFORM IMS-STATUSKONTROLL                                           
001521     .                                                                    
001522     SKIP3                                                                
001523 IMS-REPL-WDK7 SECTION.                                                   
001524                                                                          
001525     MOVE '  ' TO GODK-STATUSKODER                                        
001526     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
001527     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001528     PERFORM IMS-STATUSKONTROLL                                           
001529     .                                                                    
001530     SKIP3                                                                
001531 IMS-GET-ARTC-WLARTC01 SECTION.                                           
001532     SKIP2                                                                
001533     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001534           DELIMITED BY SIZE INTO SSA1                                    
001535     MOVE '  ' TO GODK-STATUSKODER                                        
001536     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
001537     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001538     PERFORM IMS-STATUSKONTROLL                                           
001539     .                                                                    
001540     EJECT                                                                
001541 IMS-GET-ARTC-WLARTC11 SECTION.                                           
001542     SKIP2                                                                
001543     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001544           DELIMITED BY SIZE INTO SSA1                                    
001545     MOVE 'WLARTC11 ' TO SSA2                                             
001546     MOVE '  ' TO GODK-STATUSKODER                                        
001547     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
001548     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001549     PERFORM IMS-STATUSKONTROLL                                           
001550     .                                                                    
001551     EJECT                                                                
001552 IMS-GET-ARTC-WLARTC21 SECTION.                                           
001553                                                                          
001554     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001555          DELIMITED BY SIZE INTO SSA1                                     
001556     STRING 'WLARTC11(KDSEGKEY =1)'                                       
001557                     DELIMITED BY SIZE INTO SSA2                          
001558     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-X ')'                         
001559                     DELIMITED BY SIZE INTO SSA3                          
001560     MOVE '  GE' TO GODK-STATUSKODER                                      
001561     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC21 SSA1                  
001562                                                     SSA2                 
001563                                                     SSA3                 
001564     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001565     PERFORM IMS-STATUSKONTROLL                                           
001566     .                                                                    
001567     EJECT                                                                
001568 IMS-RESTART SECTION.                                                     
001569     SKIP2                                                                
001570     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
001571     MOVE '  ' TO GODK-STATUSKODER                                        
001572     CALL CBLTDLI USING XRST MSG-PCB                                      
001573                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
001580                        CHKP-AREA-LENGTH CHKP-AREA                        
001590     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001591     PERFORM IMS-STATUSKONTROLL                                           
001592     .                                                                    
001593     EJECT                                                                
001594 IMS-CHECKPOINT SECTION.                                                  
001595     SKIP2                                                                
001596     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
001597     MOVE '  XD' TO GODK-STATUSKODER                                      
001598     CALL CBLTDLI USING CHKP MSG-PCB                                      
001599                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
001600                        CHKP-AREA-LENGTH CHKP-AREA                        
001601     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001602     PERFORM IMS-STATUSKONTROLL                                           
001603                                                                          
001604     IF IMS-EJ-OK                                                         
001605       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
001606       DISPLAY FELTEXT                                                    
001607       CALL FELLOG                                                        
001608     END-IF                                                               
001609     .                                                                    
001610     EJECT                                                                
001611 IMS-STATUSKONTROLL SECTION.                                              
001612     SKIP2                                                                
001613     SET STATUS-IX TO 1                                                   
001614     SEARCH GODK-STATUS                                                   
001615       AT END                                                             
001616         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001617           DELIMITED BY SIZE INTO FELTEXT                                 
001618         DISPLAY FELTEXT                                                  
001619         CALL FELLOG                                                      
001620       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001621         CONTINUE                                                         
001622     END-SEARCH                                                           
001623     .                                                                    
