000100 ID  DIVISION.                                                            
000205 PROGRAM-ID.    W4764000.                                                 
000305 AUTHOR.        CAMELIA OLGRENER.                                         
000405 DATE-WRITTEN.  MARS 2003.                                                
000505 DATE-COMPILED.                                                           
000605                                                                          
000705*    FUNKTION:                                                            
000805*        PROGRAMMET STARTAS FRÅN MPP 4632 (SHIP-IT)                       
000905*        VIA SOP-RUTIN W476ST                                             
001005*        IDSHIPM ÄR PARAMETER IN                                          
001105*                                                                         
001205*        PGM:ET LÄSER WDE1, WDB2 OCH WDQ2                                 
001305*                                                                         
001405*        SKAPAR FIL MED INFO TILL DIV. TRANSPORTÖRER                      
001505*                                                                         
001605*    E-TR: 10200987 26/06-13 NYTT URVAL TILL TRANSP (GALLIKER).           
001705*    E-TR: 10244338 04/11-14 NYTT URVAL TILL TRANSP (LAGERMAX).           
001805*    JIRA: 2614       /12-18 NYTT URVAL TILL TRANSP (DANX).               
001905*    JIRA: 2615       /02-19 NYTT URVAL TILL TRANSP (SCHENKER).           
002005*    JIRA: 2976       /02-19 NYTT URVAL TILL TRANSP (DHL).                
002105*    PBI : 1514931  06/09-19 REINST. NIGHT-PLUS-SCHENKER.                 
002205*    PBI : 1540086  16/10-19 LDC 1B TILL DK I URVAL TILL TRP.DANX.        
002305*    PBI : 1540102  10/12-19 EDI TILL TRP. BCUBE.                         
002405*    PBI : 1540109  25/02-00 EDI TILL TRP. NEOVIA.                        
002505*    STORY:1639474    /06-20 SEND CORRECT RECEIVER ADDRESS                
002605*    STORY:1735484  28/09-20 NYTT URVAL DANX.                             
002705*                            BORTTAG AV DHL.                              
002805*    STORY:1834228  10/11-20 NYTT URVAL DANX.                             
002905*    STORY:1839234  13/11-20 NYTT URVAL DANX.                             
003005*    STORY:1918243  22/12-20 NYTT URVAL DANX.                             
003105*    STORY:2171444  21/07-21 NY TRANSP.GEODIS.                            
003205*    STORY:2806690  24/05-22 NY TRANSP.DHL.                               
003305*    STORY:2984996  28/09-22 NY TRANSP.DHL FÖR NORGE                      
003405*    STORY:4285328  17/02-25 NEW SELECTION LAGERMAX                       
003505*    STORY:4299083  25/02-25 NEW SELECTION DANX                           
003605*    STORY:4590609  30/10-25 GEODIS  WILL NO LONGER HANDLE GOODS          
003705*                            FROM CDC.REMOVED CODE RELATED TO             
003805*                            DIST87-GEODIS,DIST87-GEODIS-1,               
003905*                            DIST87-GEODIS-17.                            
003906                                                                          
004200     EJECT                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     SKIP2                                                                
004500 INPUT-OUTPUT SECTION.                                                    
004600                                                                          
004700 FILE-CONTROL.                                                            
004800     SKIP2                                                                
004900*- - - - - - - - - - - - INFIL:                                           
005000     SELECT W476IN                       ASSIGN TO W47640D1.              
005100*- - - - - - - - - - - - UTFIL:                                           
005200*          --- FIL TRANSP                                                 
005300     SELECT W47640                       ASSIGN TO W47640D2.              
005400     SKIP2                                                                
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800 FD  W476IN                                                               
005900     RECORDING      F                                                     
006000     BLOCK CONTAINS 0.                                                    
006100 01  PARM            PIC X(80).                                           
006200 FD  W47640                                                               
006300     RECORDING       V                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600 01  UT-TRP-HUV.                                                          
006700*03   -COPY W476E101   -L.                                                
006800     SKIP2                                                                
006900 01  UT-TRP-KND.                                                          
007000*03   -COPY W476E111   -L.                                                
007100     SKIP2                                                                
007200 01  UT-TRP-KLI.                                                          
007300*03   -COPY W476E121   -L.                                                
007400     SKIP2                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600                                                                          
007700*    -- CHECKED BY WY2000                                                 
007800 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4764000'.               
007900                                                                          
008000 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
008100 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
008200 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
008300 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) VALUE +0 COMP-3.          
008400 77  WS-VKORDBTO                 PIC S9(6)V9(1) VALUE +0 COMP-3.          
008500 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0 COMP-3.          
008600 77  WS-KVKOLLI                  PIC S9(5)      VALUE +0 COMP-3.          
008700 77  FOREG-IDORDER               PIC S9(7)      VALUE +0 COMP-3.          
008800 77  WS-BEGMT-RAD1               PIC X(35)      VALUE SPACE.              
008900 77  WS-BEGMT-RAD2               PIC X(35)      VALUE SPACE.              
009000 77  WS-ADGMT-GATA               PIC X(35)      VALUE SPACE.              
009100 77  WS-ADGMT-PADR               PIC X(35)      VALUE SPACE.              
009200 77  WS-ADGMT-LAND               PIC X(35)      VALUE SPACE.              
009300                                                                          
009400 77  FL-NIGHT-PLUS-SCH           PIC X(1)    VALUE 'N'.                   
009500 77  FL-LAGERMAX                 PIC X(1)    VALUE 'N'.                   
009600 77  FL-TRUCKWHEEL               PIC X(1)    VALUE 'N'.                   
009700 77  FL-CAT                      PIC X(1)    VALUE 'N'.                   
009800 77  FL-GALLIKER                 PIC X(1)    VALUE 'N'.                   
009900 77  FL-DANX                     PIC X(1)    VALUE 'N'.                   
010000 77  FL-SCHENKER                 PIC X(1)    VALUE 'N'.                   
010100 77  FL-BCUBE                    PIC X(1)    VALUE 'N'.                   
010200 77  FL-NEOVIA                   PIC X(1)    VALUE 'N'.                   
010300 77  FL-DHL                      PIC X(1)    VALUE 'N'.                   
010400                                                                          
010500 77  SKRIV-POST-SW               PIC X       VALUE 'N'.                   
010600     88 SKRIV-POST                           VALUE 'J'.                   
010700                                                                          
010800 77  INFIL-EOF-SW                PIC X    VALUE 'N'.                      
010900     88  END-OF-W476IN                    VALUE 'J'.                      
011000                                                                          
011100 01  FELTEXT.                                                             
011200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011400     EJECT                                                                
011500*- - - - - - - - - - - - - -                                              
011600*      --- VALID IDDC CODES                                               
011700*                                                                         
011800*01    -COPY WWDC99                                                       
011900 01  TEST-IDDISTR     PIC S9(5)        VALUE ZERO  COMP-3.                
012000*01  FILLER     -COPY WWDIST87   -RED  TEST-IDDISTR.                      
012100     EJECT                                                                
012200*01  FILLER     -COPY WWDIST79   -RED  TEST-IDDISTR.                      
012300     EJECT                                                                
012400******************************************************************        
012500*       CONSTANTS                                                *        
012600******************************************************************        
012700     SKIP2                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
012900 01  KONSTANTER.                                                          
013000     03  JA                      PIC X(1)    VALUE 'J'.                   
013100     03  NEJ                     PIC X(1)    VALUE 'N'.                   
013200     SKIP2                                                                
013300                                                                          
013400 01  PARM-AREA                   PIC X(80)   VALUE SPACE.                 
013500 01  FILLER                      REDEFINES PARM-AREA.                     
013600     03  WS-IDSHIPM              PIC 9(7).                                
013700                                                                          
013800 01  SPAR-IDDISTR                PIC S9(5)  VALUE ZERO COMP-3.            
013900                                                                          
014000     SKIP3                                                                
014100******************************************************************        
014200*       VARIABLES                                                *        
014300******************************************************************        
014400     SKIP2                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
014600 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
014700     SKIP3                                                                
014800     EJECT                                                                
014900******************************************************************        
015000*       WORK AREA                                                *        
015100******************************************************************        
015200     SKIP2                                                                
015300     EJECT                                                                
015400******************************************************************        
015500*       OUTPUT AREA                                                       
015600******************************************************************        
015700     SKIP2                                                                
015800 01  FILLER                 PIC X(16) VALUE 'OUTPUT TRANSP '.             
015900                                                                          
016000 01  ARB-UTAREA             PIC X(250).                                   
016100                                                                          
016200 01  TRP-HUVUD     REDEFINES  ARB-UTAREA.                                 
016300*    03  -COPY W476E101   -PRE UT-.                                       
016400     EJECT                                                                
016500 01  TRP-KUND      REDEFINES  ARB-UTAREA.                                 
016600*    03  -COPY W476E111   -PRE UT-.                                       
016700     EJECT                                                                
016800 01  TRP-KOLLI     REDEFINES  ARB-UTAREA.                                 
016900*    03  -COPY W476E121   -PRE UT-.                                       
017000*                                                                         
017100     EJECT                                                                
017200 01  FILLER                 PIC X(16) VALUE 'SPARAREA      '.             
017300                                                                          
017400 01  SPAR-UTAREA            PIC X(160).                                   
017500                                                                          
017600 01  SPAR-TRP-HUV  REDEFINES  SPAR-UTAREA.                                
017700*    03  -COPY W476E101   -PRE SPAR-.                                     
017800     EJECT                                                                
017900 01  DYNAMISKA-SUBPROGRAM.                                                
018000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
018100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
018200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
018300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
018400     SKIP2                                                                
018500 01  RETURKODER.                                                          
018600     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
018700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
018800     SKIP2                                                                
018900*01   -COPY W0005       -PRE POSTSUM-.                                    
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200     SKIP3                                                                
019300 01  NYCKLAR-TILL-DLI.                                                    
019400     03  W-IDSHIPM-X.                                                     
019500         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
019600     03  W-IDDISTR-X.                                                     
019700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019900     03  W-IDGMT-X.                                                       
020000         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO  COMP-3.          
020100         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO  COMP-3.          
020200    03  W-IDORDER-X.                                                      
020300      05  W-IDORDER              PIC S9(7)   VALUE ZERO  COMP-3.          
020400                                                                          
020500     SKIP2                                                                
020600*    --- STATUS-KOD FRÅN IMS                                              
020700 01  STATUS-WS                   PIC XX.                                  
020800     88  SEGMENT-FINNS                       VALUE '  '.                  
020900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021200     SKIP2                                                                
021300 01  GODK-STATUSKODER.                                                    
021400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500     SKIP3                                                                
021600 01  SSA1                        PIC X(64).                               
021700 01  SSA2                        PIC X(64).                               
021800     EJECT                                                                
021900*    --- IMS FUNKTIONSKODER                                               
022000*01  -COPY W0003                                                          
022100     EJECT                                                                
022200*    ---  DLI INPUT-OUTPUT AREA                                           
022300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE101'.         
022400 01  DLI-IO-WDE101.                                                       
022500*    03  -COPY WDE101                                                     
022600     EJECT                                                                
022700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE111'.         
022800 01  DLI-IO-WDE111.                                                       
022900*    03  -COPY WDE111                                                     
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE121'.         
023200 01  DLI-IO-WDE121.                                                       
023300*    03  -COPY WDE121                                                     
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
023600 01  DLI-IO-WDB201.                                                       
023700*    03  -COPY WDB201                                                     
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
024000 01  DLI-IO-WDQ201.                                                       
024100*    03  -COPY WDQ201                                                     
024200     EJECT                                                                
024300 LINKAGE SECTION.                                                         
024400*01  -COPY W0008  -PRE WDE1-                                              
024500     05  FILLER                  PIC X.                                   
024600*01  -COPY W0008  -PRE WDB2-                                              
024700     05  FILLER                  PIC X.                                   
024800*01  -COPY W0008  -PRE WDQ2-                                              
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100 PROCEDURE DIVISION  USING WDE1-PCB WDB2-PCB                              
025200                           WDQ2-PCB.                                      
025300 MAIN SECTION.                                                            
025400     ENTRY 'DLITCBL' USING WDE1-PCB WDB2-PCB                              
025500                           WDQ2-PCB.                                      
025600     PERFORM A-INIT                                                       
025700                                                                          
025800     PERFORM B-LAES-PARAMETER                                             
025900                                                                          
026000     MOVE WS-IDSHIPM           TO W-IDSHIPM                               
026100     PERFORM IMS-GU-WDE101                                                
026200     PERFORM C-SKAPA-SKEPPN-POST                                          
026300                                                                          
026400     PERFORM IMS-GNP-WDE111                                               
026500                                                                          
026600     PERFORM UNTIL SEGMENT-SAKNAS                                         
026700       PERFORM D-KOLLA-TRANSP                                             
026800                                                                          
026900       MOVE SGMT-IDDISTR       TO TEST-IDDISTR                            
027000                                                                          
027001       IF (FL-NIGHT-PLUS-SCH = JA AND DIST87-NIGHT-PLUS)    OR            
027002          (FL-LAGERMAX       = JA AND DIST87-LAGERMAX)      OR            
027003          (FL-LAGERMAX       = JA AND DIST87-LAGERMAX-LYNK) OR            
027100          (FL-TRUCKWHEEL     = JA AND DIST87-TRUCKWHEEL)    OR            
027200          (FL-GALLIKER       = JA AND DIST87-GALLIKER)      OR            
027300          (FL-DANX           = JA AND DIST87-DANX)          OR            
027400          (FL-DANX           = JA AND DIST87-DANX-DK)       OR            
027500          (FL-DANX           = JA AND DIST87-DANX-SE)       OR            
027700          (FL-DANX           = JA AND DIST87-DANX-LYNK)     OR            
027800          (FL-DANX           = JA AND DIST87-DANX-POLESTAR) OR            
027900          (FL-DANX           = JA AND DIST87-DANX-INT)      OR            
028000          (FL-DANX           = JA AND DIST87-DANX-POLEN)    OR            
028100          (FL-SCHENKER       = JA AND DIST87-SCHENKER)      OR            
028200          (FL-BCUBE          = JA AND DIST87-BCUBE)         OR            
028300          (FL-BCUBE          = JA AND DIST87-BCUBE-PLUS)    OR            
028400          (FL-NEOVIA         = JA AND DIST87-NEOVIA-ES)     OR            
028500          (FL-NEOVIA         = JA AND DIST87-NEOVIA-AFRIKA) OR            
028900          (FL-DHL            = JA AND DIST87-DHL)           OR            
031200          (FL-DHL            = JA AND DIST87-DHL-NO)                      
031300                                                                          
031400                                                                          
031500         PERFORM E-SKAPA-KUNDPOST                                         
031600                                                                          
031700         PERFORM IMS-GNP-WDE121                                           
031800         PERFORM UNTIL SEGMENT-SAKNAS                                     
031900                                                                          
032000           PERFORM F-SKAPA-KOLLIPOST                                      
032100                                                                          
032200           PERFORM IMS-GNP-WDE121                                         
032300         END-PERFORM                                                      
032400       END-IF                                                             
032500                                                                          
032600       PERFORM IMS-GNP-WDE111                                             
032700                                                                          
032800     END-PERFORM                                                          
032900                                                                          
033000     IF SKRIV-POST                                                        
033100       PERFORM G-SKRIV-HUV-POST                                           
033200     END-IF                                                               
033300                                                                          
033400     PERFORM Z-FINIT                                                      
033500                                                                          
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 A-INIT SECTION.                                                          
034100                                                                          
034200     OPEN OUTPUT W47640                                                   
034300     OPEN INPUT  W476IN                                                   
034400                                                                          
034500     MOVE NEJ         TO INFIL-EOF                                        
034600                         SKRIV-POST-SW                                    
034700                                                                          
034800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
034900     .                                                                    
035000     EJECT                                                                
035100 B-LAES-PARAMETER SECTION.                                                
035200                                                                          
035300     PERFORM S12-LAES-W476IN                                              
035400     IF WS-IDSHIPM NUMERIC AND WS-IDSHIPM NOT = ZERO                      
035500       CONTINUE                                                           
035600     ELSE                                                                 
035700       MOVE 'SKEPPNINGS-NR SAKNAS' TO FELTEXT-STR                         
035800       DISPLAY FELTEXT                                                    
035900       CALL FELLOG                                                        
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 C-SKAPA-SKEPPN-POST SECTION.                                             
036400                                                                          
036500     MOVE 'E101'                 TO SPAR-HUV-IDPTYP                       
036600     MOVE SHIP-IDSHIPM           TO SPAR-HUV-IDSHIPM                      
036700     MOVE SHIP-IDTRPTNR          TO SPAR-HUV-IDTRPTNR                     
036800     MOVE SHIP-IDLBBET           TO SPAR-HUV-IDLBBET                      
036900     MOVE SHIP-IDDC              TO SPAR-HUV-IDDC                         
037000                                                                          
037100     MOVE ZERO                   TO SPAR-HUV-IDDISTR                      
037200                                    SPAR-HUV-IDKUNDNR                     
037300                                    SPAR-HUV-SUORDV                       
037400                                    SPAR-HUV-VKORDBTO                     
037500                                    SPAR-HUV-VLORDBTO                     
037600     MOVE SPACE                  TO SPAR-HUV-KDVALISO                     
037700     .                                                                    
037800     EJECT                                                                
037900 D-KOLLA-TRANSP SECTION.                                                  
038000                                                                          
038100     MOVE SGMT-IDDISTR           TO TEST-IDDISTR                          
038200     MOVE SGMT-IDDC              TO WS-IDDC                               
038300                                                                          
038400     IF DIST87-NIGHT-PLUS                                                 
038500       MOVE JA                   TO FL-NIGHT-PLUS-SCH                     
038600                                    SKRIV-POST-SW                         
038700     END-IF                                                               
038800                                                                          
038900     IF DIST87-LAGERMAX                                                   
039000       MOVE JA                   TO FL-LAGERMAX                           
039100                                    SKRIV-POST-SW                         
039200     END-IF                                                               
039300                                                                          
039400     IF DIST87-LAGERMAX-LYNK                                              
039500       MOVE JA                   TO FL-LAGERMAX                           
039600                                    SKRIV-POST-SW                         
039700     END-IF                                                               
039800                                                                          
039900     IF DIST87-TRUCKWHEEL                                                 
040000       MOVE JA                   TO FL-TRUCKWHEEL                         
040100                                    SKRIV-POST-SW                         
040200     END-IF                                                               
040300                                                                          
040400     IF DIST87-GALLIKER                                                   
040500       MOVE JA                   TO FL-GALLIKER                           
040600                                    SKRIV-POST-SW                         
040700     END-IF                                                               
040800                                                                          
040900     IF DIST87-DANX                                                       
041000       MOVE JA                   TO FL-DANX                               
041100                                    SKRIV-POST-SW                         
041200     END-IF                                                               
041300                                                                          
041400     IF DIST87-DANX-DK                                                    
041500       MOVE JA                   TO FL-DANX                               
041600                                    SKRIV-POST-SW                         
041700     END-IF                                                               
041800                                                                          
041900     IF DIST87-DANX-SE                                                    
042000       MOVE JA                   TO FL-DANX                               
042100                                    SKRIV-POST-SW                         
042200     END-IF                                                               
042300                                                                          
044200     IF DIST87-DANX-LYNK                                                  
044300       MOVE JA                   TO FL-DANX                               
044400                                    SKRIV-POST-SW                         
044500     END-IF                                                               
044600                                                                          
044700     IF DIST87-DANX-POLESTAR                                              
044800       MOVE JA                   TO FL-DANX                               
044900                                    SKRIV-POST-SW                         
045000     END-IF                                                               
045100                                                                          
045200     IF DIST87-DANX-INT                                                   
045300       MOVE JA                   TO FL-DANX                               
045400                                    SKRIV-POST-SW                         
045500     END-IF                                                               
045600                                                                          
045700     IF DIST87-DANX-POLEN                                                 
045800       MOVE JA                   TO FL-DANX                               
045900                                    SKRIV-POST-SW                         
046000     END-IF                                                               
046100                                                                          
046200     IF DIST87-SCHENKER                                                   
046300       MOVE JA                   TO FL-SCHENKER                           
046400                                    SKRIV-POST-SW                         
046500     END-IF                                                               
046600                                                                          
046700     IF DIST87-BCUBE                                                      
046800       MOVE JA                   TO FL-BCUBE                              
046900                                    SKRIV-POST-SW                         
047000     END-IF                                                               
047100                                                                          
047200     IF DIST87-BCUBE-PLUS                                                 
047300       MOVE JA                   TO FL-BCUBE                              
047400                                    SKRIV-POST-SW                         
047500     END-IF                                                               
047600                                                                          
047700     IF DIST87-NEOVIA-ES OR DIST87-NEOVIA-AFRIKA                          
047800       MOVE JA                   TO FL-NEOVIA                             
047900                                    SKRIV-POST-SW                         
048000     END-IF                                                               
048100                                                                          
048200     IF DIST87-DHL OR DIST87-DHL-NO                                       
048300       MOVE JA                   TO FL-DHL                                
048400                                    SKRIV-POST-SW                         
048500     END-IF                                                               
048600                                                                          
048700     IF SPAR-HUV-IDDISTR = ZERO                                           
048800       IF SKRIV-POST                                                      
048900         MOVE SGMT-IDDISTR       TO SPAR-HUV-IDDISTR                      
049000       END-IF                                                             
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 E-SKAPA-KUNDPOST SECTION.                                                
049500                                                                          
049600     MOVE 'E111'                 TO UT-KND-IDPTYP                         
049700     MOVE SHIP-IDSHIPM           TO UT-KND-IDSHIPM                        
049800     MOVE SGMT-IDDISTR           TO UT-KND-IDDISTR                        
049900                                    W-IDDISTR-B2                          
050000                                    W-IDDISTR                             
050100     MOVE SGMT-IDKUNDNR          TO UT-KND-IDKUNDNR                       
050200                                    W-IDKUNDNR-B2                         
050300                                    W-IDKUNDNR                            
050400     MOVE SGMT-IDDC              TO UT-KND-IDDC                           
050500                                                                          
050600     PERFORM  IMS-GU-WDB201                                               
050700     IF SEGMENT-FINNS                                                     
050800       MOVE GMT-BEGMT-RAD1      TO UT-KND-BEGMT-RAD1                      
050900       MOVE GMT-BEGMT-RAD2      TO UT-KND-BEGMT-RAD2                      
051000       MOVE GMT-ADGMT-GATA      TO UT-KND-ADGMT-GATA                      
051100       MOVE GMT-ADGMT-PADR      TO UT-KND-ADGMT-PADR                      
051200       MOVE GMT-ADGMT-LAND      TO UT-KND-ADGMT-LAND                      
051300     ELSE                                                                 
051400       MOVE SPACES              TO UT-KND-BEGMT-RAD1                      
051500                                   UT-KND-BEGMT-RAD2                      
051600                                   UT-KND-ADGMT-GATA                      
051700                                   UT-KND-ADGMT-PADR                      
051800                                   UT-KND-ADGMT-LAND                      
051900     END-IF                                                               
052000                                                                          
052100     MOVE SGMT-KDORDKL-MAX      TO SPAR-HUV-KDORDKL                       
052200     MOVE SGMT-KDVALISO         TO SPAR-HUV-KDVALISO                      
052300                                                                          
052400     WRITE UT-TRP-KND  FROM ARB-UTAREA                                    
052500     MOVE 'W47640'              TO POSTSUM-FDNAMN                         
052600     MOVE 'W47640D2'            TO POSTSUM-DDNAMN2                        
052700     MOVE UT-KND-IDPTYP         TO POSTSUM-TRANSTYP                       
052800     SKIP2                                                                
052900     CALL POSTSUM               USING  POSTSUM-PARM                       
053000     .                                                                    
053100     EJECT                                                                
053200 F-SKAPA-KOLLIPOST SECTION.                                               
053300                                                                          
053400     MOVE 'E121'                 TO UT-KLI-IDPTYP                         
053500     MOVE SHIP-IDSHIPM           TO UT-KLI-IDSHIPM                        
053600     MOVE SGMT-IDDISTR           TO UT-KLI-IDDISTR                        
053700                                    TEST-IDDISTR                          
053800     MOVE SGMT-IDKUNDNR          TO UT-KLI-IDKUNDNR                       
053900     MOVE SGMT-IDDC              TO UT-KLI-IDDC                           
054000                                                                          
054100     MOVE SKOLLI-IDKOLLI         TO UT-KLI-IDKOLLI                        
054200     MOVE SKOLLI-IDKUNDRF(3:5)   TO UT-KLI-IDORDNR5                       
054300     MOVE SKOLLI-KDFARLIG-KOLLI  TO UT-KLI-KDFARLIG-KOLLI                 
054400     MOVE SKOLLI-KDFRAKT         TO UT-KLI-KDFRAKT                        
054500                                                                          
054600     IF DIST79-DEALER-PRICE                                               
054700       COMPUTE UT-KLI-SUORDV-KOLLI = SKOLLI-SUORDV-LOC +                  
054800                                     SKOLLI-SUORDV-LOCPREL                
054900       MOVE SKOLLI-KDVALISO      TO UT-KLI-KDVALISO                       
055000                                                                          
055100       COMPUTE WS-SUORDV = WS-SUORDV         +                            
055200                           SKOLLI-SUORDV-LOC +                            
055300                           SKOLLI-SUORDV-LOCPREL                          
055400     ELSE                                                                 
055500       IF DIST79-ECOM-PRICE                                               
055600         MOVE SKOLLI-SUORDV-LOC  TO UT-KLI-SUORDV-KOLLI                   
055700         MOVE SKOLLI-KDVALISO    TO UT-KLI-KDVALISO                       
055800                                                                          
055900         COMPUTE WS-SUORDV = WS-SUORDV       +                            
056000                             SKOLLI-SUORDV-LOC                            
056100       ELSE                                                               
056200         MOVE SKOLLI-SUORDV      TO UT-KLI-SUORDV-KOLLI                   
056300         MOVE 'SEK'              TO UT-KLI-KDVALISO                       
056400                                                                          
056500         IF SKOLLI-SUORDV > ZERO                                          
056600           COMPUTE WS-SUORDV-KOLLI                                        
056700                             = SKOLLI-SUORDV / SGMT-PRKURS                
056800         ELSE                                                             
056900           MOVE ZERO             TO  WS-SUORDV-KOLLI                      
057000         END-IF                                                           
057100                                                                          
057200         COMPUTE WS-SUORDV = WS-SUORDV + WS-SUORDV-KOLLI                  
057300                                                                          
057400         MOVE ZERO               TO WS-SUORDV-KOLLI                       
057500       END-IF                                                             
057600     END-IF                                                               
057700                                                                          
057800     MOVE SKOLLI-VKORDBTO-KOLLI  TO UT-KLI-VKORDBTO-KOLLI                 
057900     MOVE SKOLLI-VLORDBTO-KOLLI  TO UT-KLI-VLORDBTO-KOLLI                 
058000     MOVE SKOLLI-DIKOLLIL        TO UT-KLI-DIKOLLIL                       
058100     MOVE SKOLLI-DIKOLLIB        TO UT-KLI-DIKOLLIB                       
058200     MOVE SKOLLI-DIKOLLIH        TO UT-KLI-DIKOLLIH                       
058300     MOVE SKOLLI-KDORDKL         TO UT-KLI-KDORDKL                        
058400                                                                          
058500     IF FOREG-IDORDER NOT = SKOLLI-IDORDER                                
058600       MOVE SKOLLI-IDORDER       TO FOREG-IDORDER                         
058700                                    W-IDORDER                             
058800       PERFORM IMS-GU-WDQ201                                              
058900                                                                          
059000       IF SEGMENT-FINNS                                                   
059100         MOVE OHUV-BEGMT-RAD1   TO WS-BEGMT-RAD1                          
059200         MOVE OHUV-BEGMT-RAD2   TO WS-BEGMT-RAD2                          
059300         MOVE OHUV-ADGMT-GATA   TO WS-ADGMT-GATA                          
059400         MOVE OHUV-ADGMT-PADR   TO WS-ADGMT-PADR                          
059500         MOVE OHUV-ADGMT-LAND   TO WS-ADGMT-LAND                          
059600       ELSE                                                               
059700         MOVE SPACE             TO WS-ADGMT-GATA                          
059800                                   WS-ADGMT-PADR                          
059900                                   WS-ADGMT-LAND                          
060000                                   WS-BEGMT-RAD1                          
060100                                   WS-BEGMT-RAD2                          
060200       END-IF                                                             
060300     END-IF                                                               
060400                                                                          
060500     MOVE WS-BEGMT-RAD1       TO UT-KLI-BEGMT-RAD1                        
060600     MOVE WS-BEGMT-RAD2       TO UT-KLI-BEGMT-RAD2                        
060700     MOVE WS-ADGMT-GATA       TO UT-KLI-ADGMT-GATA                        
060800     MOVE WS-ADGMT-PADR       TO UT-KLI-ADGMT-PADR                        
060900     MOVE WS-ADGMT-LAND       TO UT-KLI-ADGMT-LAND                        
061000                                                                          
061100     COMPUTE WS-VKORDBTO = WS-VKORDBTO +                                  
061200                           SKOLLI-VKORDBTO-KOLLI                          
061300                                                                          
061400     COMPUTE WS-VLORDBTO = WS-VLORDBTO +                                  
061500                           SKOLLI-VLORDBTO-KOLLI                          
061600                                                                          
061700     COMPUTE WS-KVKOLLI  = WS-KVKOLLI  + 1                                
061800                                                                          
061900     WRITE UT-TRP-KLI  FROM ARB-UTAREA                                    
062000     MOVE 'W47640'               TO POSTSUM-FDNAMN                        
062100     MOVE 'W47640D2'             TO POSTSUM-DDNAMN2                       
062200     MOVE UT-KLI-IDPTYP          TO POSTSUM-TRANSTYP                      
062300     SKIP2                                                                
062400     CALL POSTSUM             USING  POSTSUM-PARM                         
062500     .                                                                    
062600     EJECT                                                                
062700 G-SKRIV-HUV-POST SECTION.                                                
062800                                                                          
062900     MOVE WS-SUORDV           TO SPAR-HUV-SUORDV                          
063000     MOVE WS-VKORDBTO         TO SPAR-HUV-VKORDBTO                        
063100     MOVE WS-VLORDBTO         TO SPAR-HUV-VLORDBTO                        
063200     MOVE WS-KVKOLLI          TO SPAR-HUV-KVKOLLI                         
063300     MOVE SPACE               TO SPAR-HUV-IDLEVNR                         
063400                                                                          
063500     WRITE UT-TRP-HUV  FROM SPAR-UTAREA                                   
063600     MOVE 'W47640'            TO POSTSUM-FDNAMN                           
063700     MOVE 'W47640D2'          TO POSTSUM-DDNAMN2                          
063800     MOVE SPAR-HUV-IDPTYP     TO POSTSUM-TRANSTYP                         
063900     SKIP2                                                                
064000     CALL POSTSUM             USING  POSTSUM-PARM                         
064100     .                                                                    
064200     EJECT                                                                
064300 S12-LAES-W476IN  SECTION.                                                
064400                                                                          
064500     READ W476IN INTO PARM-AREA                                           
064600     AT END                                                               
064700        SET END-OF-W476IN TO TRUE                                         
064800                                                                          
064900     NOT AT END                                                           
065000        MOVE 'INFIL'      TO POSTSUM-FDNAMN                               
065100        MOVE 'W47640D1'   TO POSTSUM-DDNAMN2                              
065200        MOVE SPACE        TO POSTSUM-TRANSTYP                             
065300        CALL POSTSUM USING POSTSUM-PARM                                   
065400     END-READ                                                             
065500     .                                                                    
065600     EJECT                                                                
065700 Z-FINIT SECTION.                                                         
065800                                                                          
065900     MOVE 'S' TO POSTSUM-OPKOD                                            
066000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
066100                                                                          
066200     CLOSE W476IN                                                         
066300           W47640                                                         
066400     .                                                                    
066500     EJECT                                                                
066600*****IMS-LÄSNINGAR******                                                  
066700                                                                          
066800 IMS-GU-WDE101 SECTION.                                                   
066900                                                                          
067000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
067100          DELIMITED BY SIZE INTO SSA1                                     
067200     MOVE '    ' TO GODK-STATUSKODER                                      
067300     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
067400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700     SKIP2                                                                
067800 IMS-GNP-WDE111 SECTION.                                                  
067900                                                                          
068000     MOVE 'WDE111 '          TO SSA1                                      
068100     MOVE '  GE' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
068300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-GNP-WDE121  SECTION.                                                 
068800                                                                          
068900     STRING 'WDE111  (WDE111KY =' W-IDDISTR-X ')'                         
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     MOVE 'WDE121 '          TO SSA2                                      
069200     MOVE '  GE' TO GODK-STATUSKODER                                      
069300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
069400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
069500     PERFORM IMS-STATUSKONTROLL                                           
069600     .                                                                    
069700     SKIP2                                                                
069800 IMS-GU-WDB201 SECTION.                                                   
069900                                                                          
070000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
070100          DELIMITED BY SIZE INTO SSA1                                     
070200     MOVE '    ' TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
070400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700     EJECT                                                                
070800 IMS-GU-WDQ201            SECTION.                                        
070900                                                                          
071000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
071100          DELIMITED BY SIZE INTO SSA1                                     
071200     MOVE '  GE'               TO GODK-STATUSKODER                        
071300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
071400     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700     SKIP3                                                                
071800     EJECT                                                                
071900 IMS-STATUSKONTROLL SECTION.                                              
072000     SET STATUS-IX TO 1                                                   
072100     SEARCH GODK-STATUS                                                   
072200       AT END                                                             
072300         CALL FELLOG                                                      
072400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072500         CONTINUE                                                         
072600     END-SEARCH                                                           
072700     .                                                                    
072805     EJECT                                                                
