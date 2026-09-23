000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2170200.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               DEC 1978.                                    
000500                                                                          
000600                                                                          
000700                                                                          
000800                                                                          
000900     REMARKS.                                                             
001000*    FUNKTION.                                                            
001100*        PROGRAMMET BEHANDLAR TRANSAKTIONERNA R08 OCH 043 (R05)           
001200*        UPPDATERING AV AKTUELLA DATAELEMENT PÅ WDK611 SAMT               
001300*        HÄNDELSEBASER SKER GENOM UTFILER.                                
001400*                                                                         
001500* CHANGE LOG:                                                             
001600* 2015-04-22   E'TRACKER 10130993                                         
001700*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
001800*                                                                         
001900* 2015-12-28   E'TRACKER 10243132                                         
002000*              CHINA EXPORT PROJECT 2015                                  
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*---------------------------------------------------------------          
002800*                                        TRANSAKTIONER FRÅN W092          
002900*                                        OCH W011                         
003000*                                        INPUT                            
003100     SELECT  W21701      ASSIGN W21702D1.                                 
003200*                                                                         
003300*--------------------------------------------------------------           
003400*                                        FELPOSTER TILL TRATTEN           
003500*                                        OUTPUT                           
003600     SELECT  W21703      ASSIGN W21702D2.                                 
003700*---------------------------------------------------------------          
003800*                                       AVTALSTRANSAKTIONER TILL          
003900*                                       INKÖP (A310)                      
004000     SELECT W21707       ASSIGN W21702D4.                                 
004100*---------------------------------------------------------------          
004200*                                       DB-UPPDATERING WDK6               
004300*                                       I PGM W21704                      
004400     SELECT W21705       ASSIGN W21702D5.                                 
004500*---------------------------------------------------------------          
004600*                                       POSTER 2222                       
004700     SELECT W21731       ASSIGN W21702D6.                                 
004800*---------------------------------------------------------------          
004900*                                       U32-POSTER                        
005000     SELECT W21732       ASSIGN W21702D7.                                 
005100*---------------------------------------------------------------          
005200*                                       U33-POSTER                        
005300     SELECT W21733       ASSIGN W21702D8.                                 
005400                                                                          
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700 FILE SECTION.                                                            
005800                                                                          
005900 FD  W21701                                                               
006000     RECORDING V                                                          
006100     BLOCK 0                                                              
006200     LABEL RECORD STANDARD.                                               
006300*01  -COPY W217R08     -L                                                 
006400                                                                          
006500*01  -COPY W217043     -L                                                 
006600                                                                          
006700 FD  W21703                                                               
006800     RECORDING V                                                          
006900     BLOCK 0                                                              
007000     LABEL RECORD STANDARD.                                               
007100 01  U02-R05-POST.                                                        
007200*    03  -COPY W092W001    -L                                             
007300     03  U02-R05-IDELMT          PIC X(16).                               
007400                                                                          
007500 01  U02-R08-POST.                                                        
007600*    03  -COPY W092W001    -L                                             
007700     EJECT                                                                
007800 FD  W21707                                                               
007900     RECORDING F                                                          
008000     BLOCK 0                                                              
008100     LABEL RECORD STANDARD.                                               
008200 01  UT-A310-PV-POST             PIC X(80).                               
008300     SKIP3                                                                
008400 FD  W21705                                                               
008500     RECORDING F                                                          
008600     BLOCK 0                                                              
008700     LABEL RECORD STANDARD.                                               
008800 01  U05-POST.                                                            
008900*05   -COPY W21705      -L                                                
009000     SKIP3                                                                
009100 FD  W21731                                                               
009200     RECORDING F                                                          
009300     BLOCK 0                                                              
009400     LABEL RECORD STANDARD.                                               
009500 01  U31-POST.                                                            
009600*05   -COPY W213R01     -L                                                
009700     SKIP3                                                                
009800 FD  W21732                                                               
009900     RECORDING F                                                          
010000     BLOCK 0                                                              
010100     LABEL RECORD STANDARD.                                               
010200 01  U32-POST.                                                            
010300*05   -COPY W2132213    -L                                                
010400     SKIP3                                                                
010500 FD  W21733                                                               
010600     RECORDING F                                                          
010700     BLOCK 0                                                              
010800     LABEL RECORD STANDARD.                                               
010900 01  U33-POST.                                                            
011000*05   -COPY W2212204    -L                                                
011100     EJECT                                                                
011200 WORKING-STORAGE SECTION.                                                 
011300                                                                          
011400                                                                          
011500*    -- CHECKED BY WY2000                                                 
011600 01  W.                                                                   
011700     05  W-PROGNAMN              PIC X(8)    VALUE 'W2170200'.            
011800     05  W-KOD               PIC X(4).                                    
011900     05  W-DISP-LAGER        PIC S9(9)               COMP-3.              
012000     05  AVTAL-FINNS         PIC X(1)    VALUE 'N'.                       
012100     05  SPAR-IDAVTAL        PIC S9(13)  VALUE ZERO  COMP-3.              
012200     05  SPAR-IDAVTAL-PV     PIC S9(13)  VALUE ZERO  COMP-3.              
012300     05  PV-AVTAL            PIC X       VALUE SPACE.                     
012400     05  NEDCAR-AVTAL        PIC X       VALUE SPACE.                     
012500                                                                          
012600 01  W-IDAVTAL-RED           PIC 9(13).                                   
012700 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
012800     03  FILLER              PIC X(1).                                    
012900     03  W-PREFIX            PIC X(3).                                    
013000     03  W-AVTALNR           PIC X(6).                                    
013100     03  W-SUFFIX            PIC X(3).                                    
013200                                                                          
013300 01  W-PREFIX-NUM            PIC 9(3)    VALUE ZERO.                      
013400 01  W-IDARTNR-8             PIC 9(8)    VALUE ZERO.                      
013500 01  W-KDAVT-GAMLA           PIC S9(1)  COMP-3.                           
013600 01  WS-IDLEVNR-NUM          PIC 9(5)    VALUE ZERO.                      
013700 01  WS-IDANSK-GAMLA         PIC 9(3)    VALUE ZERO.                      
013800 01  TEST-IDINK              PIC 9(3)    VALUE ZERO.                      
013900                                                                          
014000 01  KONSTANTER.                                                          
014100     05  JA                      PIC X(1)    VALUE 'J'.                   
014200     05  NEJ                     PIC X(1)    VALUE 'N'.                   
014300                                                                          
014400 01  SWITCHAR.                                                            
014500     05  SW-W21701-EOF           PIC X       VALUE 'N'.                   
014600                                                                          
014700 01  DYNAMISKA-SUBPROGRAM.                                                
014800     05  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014900     05  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015000     05  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
015100     05  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
015200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015300                                                                          
015400 01  FELTEXT.                                                             
015500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015700     EJECT                                                                
015800 01  DAGENS-DATUM.                                                        
015900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016200                                                                          
016300 01  DAGENS-VECKA.                                                        
016400     03  DAGENS-VECKA-AAR        PIC 9(2).                                
016500     03  DAGENS-VECKA-VECKA      PIC 9(2).                                
016600                                                                          
016700*---------------------------PARAMETRAR TILL DATKORT                       
016800 01  DATUMKORT-ID            PIC X(6)  VALUE 'WDATUM'.                    
016900 01  -COPY WDATKORT                                                       
017000                                                                          
017100     EJECT                                                                
017200*    --- PARAMETRAR TILL W009VADD                                         
017300*                                                                         
017400     EJECT                                                                
017500 77  MAX-KVHELTAL                PIC S9(4)  COMP.                         
017600 77  MAX-KVDECIMAL               PIC S9(4)  COMP.                         
017700                                                                          
017800*01  -COPY WDECAREA                                                       
017900     SKIP2                                                                
018000*01  -COPY WWDCKONS                                                       
018100     EJECT                                                                
018200*--------------------------------------- LÄNKAREA TILL SUBPROGRAM         
018300*01  -COPY W217L020    -PRE IN-                                           
018400                                                                          
018500     EJECT                                                                
018600*--------------------------------------- AREA FÖR W21701-POST             
018700*                                        POSTTYP = R08                    
018800*    03  AREA2  -COPY W217R08    -PRE IN-R08- -RED IN-POSTAREA            
018900     EJECT                                                                
019000*    03  AREA3  -COPY W217043    -PRE IN-043- -RED IN-POSTAREA            
019100     EJECT                                                                
019200*------------------------------------------------------------             
019300*                                        AREA FÖR W21703-POST             
019400*                                        FELPOST                          
019500 01  U02-AREA.                                                            
019600*    03    -COPY W092W001    -PRE U02-                                    
019700                                                                          
019800     03  U02-IDELMT              PIC X(16).                               
019900     EJECT                                                                
020000*------------------------------------------------------------             
020100*                                        AREA FÖR W21707-POST             
020200*01  AREA -COPY A310TB65   -PRE A310-                                     
020300     EJECT                                                                
020400*--------------------------------------- AREA FÖR W21705-POST             
020500*                                                                         
020600*01  AREA  -COPY W21705      -PRE U05-                                    
020700     EJECT                                                                
020800*--------------------------------------- AREA FÖR W21731-POST             
020900*                                                                         
021000 01  U31-AREA                 PIC X(15).                                  
021100     SKIP2                                                                
021200*01  AREA  -COPY W213R01     -PRE 2222-   -RED U31-AREA                   
021300     EJECT                                                                
021400*--------------------------------------- AREA FÖR W21732-POST             
021500*                                                                         
021600 01  U32-AREA                 PIC X(14).                                  
021700     SKIP2                                                                
021800*01  AREA  -COPY W2132213    -PRE 2213-   -RED U32-AREA                   
021900     SKIP2                                                                
022000*01  AREA  -COPY W2132204    -PRE 2204-   -RED U32-AREA                   
022100     SKIP2                                                                
022200*01  AREA  -COPY W2131142    -PRE 1142-   -RED U32-AREA                   
022300     EJECT                                                                
022400*--------------------------------------- AREA FÖR W21733-POST             
022500*                                        POSTTYP = R05 (043)              
022600     SKIP2                                                                
022700*01  AREA  -COPY W2212204    -PRE  U33-                                   
022800     EJECT                                                                
022900*--------------------------------------- PARAMETRAR TILL POSTSUM          
023000*01  -COPY W0005     -PRE POSTSUM-                                        
023100     EJECT                                                                
023200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023300*                                                                         
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023600                                                                          
023700 01  NYCKLAR-TILL-DLI.                                                    
023800     03  W-IDARTNR-X.                                                     
023900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024000     03  W-KDSEGKEY-X.                                                    
024100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
024200     03  W-KDNOTTYP-X.                                                    
024300         05  W-KDNOTTYP          PIC S9      VALUE ZERO COMP-3.           
024400     03  W-IDLEVNR-X.                                                     
024500         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
024600     EJECT                                                                
024700                                                                          
024800*    --- STATUS-KOD FRÅN IMS                                              
024900 01  STATUS-WS                   PIC XX.                                  
025000     88  SEGMENT-FINNS                       VALUE '  '.                  
025100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025300                                                                          
025400 01  GODK-STATUSKODER.                                                    
025500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600                                                                          
025700 01  SSA1                        PIC X(64).                               
025800 01  SSA2                        PIC X(64).                               
025900 01  SSA3                        PIC X(64).                               
026000     EJECT                                                                
026100*    --- IMS FUNKTIONSKODER                                               
026200*01  -COPY W0003                                                          
026300     EJECT                                                                
026400*    ---  DLI INPUT-OUTPUT AREA                                           
026500                                                                          
026600 01  FILLER                 PIC X(16) VALUE 'DLI-IO-AREA-01'.             
026700 01  DLI-IO-AREA-01.                                                      
026800*    03  -COPY WDK601                                                     
026900     EJECT                                                                
027000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-11'.           
027100 01  DLI-IO-AREA-11.                                                      
027200*    03  -COPY WDK611                                                     
027300     EJECT                                                                
027400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-11B'.          
027500 01  DLI-IO-AREA-11B.                                                     
027600*    03  -COPY WDK611 -PRE  11B-                                          
027700     EJECT                                                                
027800                                                                          
027900                                                                          
028000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
028100 01  DLI-IO-AREA-2.                                                       
028200     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
028300                                                                          
028400     03  WLARTC23 REDEFINES IO-AREA-2.                                    
028500         05  -COPY WDK623                                                 
028600     EJECT                                                                
028700     03  WLARTC25 REDEFINES IO-AREA-2.                                    
028800         05  -COPY WDK625                                                 
028900     EJECT                                                                
029000     03  WLARTA01 REDEFINES IO-AREA-2.                                    
029100         05  -COPY WDF101                                                 
029200     EJECT                                                                
029300                                                                          
029400 LINKAGE SECTION.                                                         
029500                                                                          
029600*01  -COPY  W0008   -PRE ARTC-                                            
029700        05  FILLER                       PIC X(1).                        
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE LEVA-                                              
030000        05  FILLER                       PIC X(1).                        
030100     EJECT                                                                
030200 PROCEDURE DIVISION  USING ARTC-PCB LEVA-PCB.                             
030300     ENTRY 'DLITCBL' USING ARTC-PCB LEVA-PCB.                             
030400                                                                          
030500 STYR SECTION.                                                            
030600     PERFORM A-INITIERING                                                 
030700     PERFORM B-LAS-TRANS                                                  
030800                                                                          
030900     PERFORM UNTIL SW-W21701-EOF = JA                                     
031000         MOVE IN-R08-IDARTNR TO W-IDARTNR                                 
031100         PERFORM IMS-GET-WLARTC01                                         
031200         IF SEGMENT-FINNS                                                 
031300            MOVE ART-IDLEVNR   TO IN-IDLEVNR-BAS                          
031400            MOVE ART-KDERS-UTG TO IN-KDERS-UTG                            
031500            PERFORM IMS-GET-WLARTC11                                      
031600            IF SEGMENT-FINNS                                              
031700              MOVE CLAG-IDANSK   TO IN-IDANSK-FEL                         
031800              EVALUATE IN-R08-IDPTYP                                      
031900                 WHEN 'R08'                                               
032000                      PERFORM C-BEHANDLA-R08                              
032100                                                                          
032200                 WHEN '043'                                               
032300                      PERFORM D-BEHANDLA-043                              
032400              END-EVALUATE                                                
032500            ELSE                                                          
032600              MOVE ZERO TO IN-IDANSK-FEL                                  
032700              MOVE 'F01'  TO IN-IDFELKODX                                 
032800            END-IF                                                        
032900                                                                          
033000            IF  IN-IDFELKODX NOT = SPACE                                  
033100                PERFORM E-SKRIV-FELPOST                                   
033200            END-IF                                                        
033300         ELSE                                                             
033400             MOVE ZERO TO IN-IDANSK-FEL                                   
033500             MOVE 'F01'  TO IN-IDFELKODX                                  
033600             PERFORM E-SKRIV-FELPOST                                      
033700         END-IF                                                           
033800                                                                          
033900         PERFORM B-LAS-TRANS                                              
034000     END-PERFORM                                                          
034100                                                                          
034200     PERFORM Z-AVSLUTNING                                                 
034300     MOVE ZERO TO RETURN-CODE                                             
034400     GOBACK                                                               
034500     .                                                                    
034600     EJECT                                                                
034700                                                                          
034800                                                                          
034900 A-INITIERING SECTION.                                                    
035000******************************************************************        
035100*                                                                *        
035200*    ÖPPNA ALLA FILER                                            *        
035300*    INITIERA ARBETSFÄLT                                         *        
035400*    DAGENS-DATUM HÄMTAS FRÅN MASKIN OCH KONVERTERAS TILL AAVV   *        
035500*                                                                *        
035600******************************************************************        
035700                                                                          
035800     OPEN INPUT W21701                                                    
035900         OUTPUT W21703                                                    
036000                W21707                                                    
036100                W21705                                                    
036200                W21731                                                    
036300                W21732                                                    
036400                W21733                                                    
036500                                                                          
036600     MOVE W-PROGNAMN TO POSTSUM-PROGNAMN                                  
036700     MOVE SPACE TO IN-IDFELKODX                                           
036800                   IN-KDBEHAND                                            
036900                                                                          
037000     CALL DATKORT USING W-PROGNAMN DATUMKORT-ID DATUMKORT                 
037100     MOVE DAGENS-DATUM        TO DATUM                                    
037200     MOVE D-AAR               TO DAGENS-DATUM-AAR                         
037300     MOVE D-MAANAD            TO DAGENS-DATUM-MAANAD                      
037400     MOVE D-DAG               TO DAGENS-DATUM-DAG                         
037500     MOVE D-AAR               TO DAGENS-VECKA-AAR                         
037600     MOVE D-VECKA             TO DAGENS-VECKA-VECKA                       
037700     MOVE DAGENS-VECKA        TO IN-TIAAVV                                
037800     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100                                                                          
038200 B-LAS-TRANS SECTION.                                                     
038300******************************************************************        
038400*                                                                *        
038500*    LÄSER W21701 OCH ÖKAR UPP POSTRÄKNAREN                      *        
038600*                                                                *        
038700******************************************************************        
038800                                                                          
038900     READ W21701 INTO IN-POSTAREA                                         
039000       AT END MOVE JA TO SW-W21701-EOF                                    
039100     END-READ                                                             
039200                                                                          
039300     IF  SW-W21701-EOF = NEJ                                              
039400         MOVE 'W21701'        TO POSTSUM-FDNAMN                           
039500         MOVE  IN-R08-IDPTYP  TO POSTSUM-TRANSTYP                         
039600         MOVE 'W21702D1'      TO POSTSUM-DDNAMN2                          
039700         CALL POSTSUM USING POSTSUM-PARM                                  
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100                                                                          
040200                                                                          
040300 C-BEHANDLA-R08 SECTION.                                                  
040400******************************************************************        
040500*                                                                *        
040600*    UPPDATERING AV NOTERINGSFÄLT.                               *        
040700*    VILKEN TYP AV UPPDATERING SOM SKALL GÖRAS BEROR PÅ          *        
040800*    UPPDATERINGSKODEN I TRANSAKTIONEN                           *        
040900*                                                                *        
041000******************************************************************        
041100                                                                          
041200         EVALUATE IN-R08-KDUPPDAT                                         
041300             WHEN 1                                                       
041400                 PERFORM CA-NYUPPLAGGNING                                 
041500                                                                          
041600             WHEN 2                                                       
041700                 PERFORM CB-ANDRING                                       
041800                                                                          
041900             WHEN 3                                                       
042000                 PERFORM CC-BORTTAG                                       
042100         END-EVALUATE                                                     
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042500                                                                          
042600 CA-NYUPPLAGGNING SECTION.                                                
042700******************************************************************        
042800*                                                                *        
042900*    NOTERINGSTYP OCH NOTERING LÄGGS UPP PÅ ARTIKELREGISTRET.    *        
043000*    OM NOTERINGSTYP REDAN FINNS SÄTTS FELKOD TILL 'F02'         *        
043100*                                                                *        
043200*                                                                *        
043300******************************************************************        
043400                                                                          
043500     PERFORM S07-NOLLA-U05-AREA                                           
043600     MOVE ISRT               TO  U05-ATGARD                               
043700     MOVE 'WDK625'           TO  U05-IDSEGM                               
043800     MOVE W-IDARTNR          TO  U05-IDARTNR                              
043900     MOVE    IN-R08-KDNOTTYP TO  W-KDNOTTYP                               
044000                                 NOT-KDNOTTYP                             
044100                                 U05-KDNOTTYP                             
044200     MOVE    IN-R08-TEARTNOT TO  NOT-TEARTNOT                             
044300                                 U05-TEARTNOT                             
044400**** PERFORM IMS-INSERT-WLARTC25                                          
044500     PERFORM IMS-GET-WLARTC25                                             
044600     IF SEGMENT-FINNS                                                     
044700        MOVE 'F02'  TO IN-IDFELKODX                                       
044800     ELSE                                                                 
044900        WRITE U05-POST  FROM U05-AREA                                     
045000                                                                          
045100        MOVE 'W21705'   TO POSTSUM-FDNAMN                                 
045200        MOVE U05-IDSEGM TO POSTSUM-DDNAMN2                                
045300        MOVE U05-ATGARD TO POSTSUM-TRANSTYP                               
045400        CALL POSTSUM USING POSTSUM-PARM                                   
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900                                                                          
046000 CB-ANDRING SECTION.                                                      
046100******************************************************************        
046200*                                                                *        
046300*    ÄNDRING AV NOTERING.                                        *        
046400*    OM NOTERINGSTYP INTE FINNS SÄTTS FELKOD TILL 'F03' .        *        
046500*                                                                *        
046600******************************************************************        
046700                                                                          
046800     PERFORM S07-NOLLA-U05-AREA                                           
046900     MOVE REPL               TO  U05-ATGARD                               
047000     MOVE 'WDK625'           TO  U05-IDSEGM                               
047100     MOVE W-IDARTNR          TO  U05-IDARTNR                              
047200     MOVE    IN-R08-KDNOTTYP TO  W-KDNOTTYP      U05-KDNOTTYP             
047300     PERFORM IMS-GET-WLARTC25                                             
047400     IF SEGMENT-FINNS                                                     
047500        MOVE    IN-R08-TEARTNOT TO  NOT-TEARTNOT U05-TEARTNOT             
047600        WRITE U05-POST  FROM U05-AREA                                     
047700                                                                          
047800        MOVE 'W21705'   TO POSTSUM-FDNAMN                                 
047900        MOVE U05-IDSEGM TO POSTSUM-DDNAMN2                                
048000        MOVE U05-ATGARD TO POSTSUM-TRANSTYP                               
048100        CALL POSTSUM USING POSTSUM-PARM                                   
048200********PERFORM IMS-REPL-WLARTC25                                         
048300     ELSE                                                                 
048400        MOVE 'F03'  TO IN-IDFELKODX                                       
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
048900                                                                          
049000 CC-BORTTAG SECTION.                                                      
049100******************************************************************        
049200*                                                                *        
049300*    BORTTAG AV NOTERING.                                        *        
049400*    OM NOTERINGSTYP INTE FINNS SÄTTS FELKOD TILL 'F04'.         *        
049500*                                                                *        
049600******************************************************************        
049700                                                                          
049800     PERFORM S07-NOLLA-U05-AREA                                           
049900     MOVE DLET               TO  U05-ATGARD                               
050000     MOVE 'WDK625'           TO  U05-IDSEGM                               
050100     MOVE W-IDARTNR          TO  U05-IDARTNR                              
050200     MOVE    IN-R08-KDNOTTYP TO  W-KDNOTTYP  U05-KDNOTTYP                 
050300     PERFORM IMS-GET-WLARTC25                                             
050400     IF SEGMENT-FINNS                                                     
050500        WRITE U05-POST  FROM U05-AREA                                     
050600                                                                          
050700        MOVE 'W21705'   TO POSTSUM-FDNAMN                                 
050800        MOVE U05-IDSEGM TO POSTSUM-DDNAMN2                                
050900        MOVE U05-ATGARD TO POSTSUM-TRANSTYP                               
051000        CALL POSTSUM USING POSTSUM-PARM                                   
051100********PERFORM IMS-DLET-WLARTC25                                         
051200     ELSE                                                                 
051300        MOVE 'F04'  TO IN-IDFELKODX                                       
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800                                                                          
051900 D-BEHANDLA-043 SECTION.                                                  
052000                                                                          
052100     IF IN-KDBEHAND = 'CLSE'                                              
052200        CLOSE W21707                                                      
052300     ELSE                                                                 
052400        MOVE ART-IDLEVNR  TO W-IDLEVNR                                    
052500        MOVE CLAG-KDAVT TO W-KDAVT-GAMLA                                  
052600        MOVE CLAG-IDANSK TO WS-IDANSK-GAMLA                               
052700        PERFORM D1-HAMTA-AVTAL                                            
052800        PERFORM D-BEARBETA-SEGM-11                                        
052900        IF  IN-IDFELKODX = SPACE                                          
053000            PERFORM IMS-GHU-WLARTC11                                      
053100            MOVE DLI-IO-AREA-11 TO DLI-IO-AREA-11B                        
053200            PERFORM D2-SKAPA-U05-FIL                                      
053300            WRITE U05-POST  FROM U05-AREA                                 
053400                                                                          
053500            MOVE 'W21705'   TO POSTSUM-FDNAMN                             
053600            MOVE U05-IDSEGM TO POSTSUM-DDNAMN2                            
053700            MOVE U05-ATGARD TO POSTSUM-TRANSTYP                           
053800            CALL POSTSUM USING POSTSUM-PARM                               
053900************PERFORM IMS-REPL-WLARTC11                                     
054000        END-IF                                                            
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500                                                                          
054600 D1-HAMTA-AVTAL SECTION.                                                  
054700                                                                          
054800     MOVE NEJ      TO AVTAL-FINNS                                         
054900                      PV-AVTAL                                            
055000                      NEDCAR-AVTAL                                        
055100     MOVE ZERO     TO SPAR-IDAVTAL                                        
055200                      SPAR-IDAVTAL-PV                                     
055300     PERFORM IMS-GET-WLARTC23                                             
055400     PERFORM UNTIL SEGMENT-SAKNAS                                         
055500        MOVE AVT-IDAVTAL TO SPAR-IDAVTAL                                  
055600        MOVE SPAR-IDAVTAL TO W-IDAVTAL-RED                                
055700        MOVE W-PREFIX TO W-PREFIX-NUM                                     
055800        MOVE JA TO AVTAL-FINNS                                            
055900                                                                          
056000***         ÄVEN NAP-AVTAL TAS MED, PREFIX = 004                          
056100        IF (W-PREFIX-NUM >  99 AND W-PREFIX-NUM < 987) OR                 
056200           (W-PREFIX-NUM > 987 AND W-PREFIX-NUM < 1000) OR                
056300           (W-PREFIX-NUM = 004)                                           
056400            MOVE JA TO PV-AVTAL                                           
056500            MOVE SPAR-IDAVTAL TO SPAR-IDAVTAL-PV                          
056600        END-IF                                                            
056700                                                                          
056800        IF (W-PREFIX-NUM > 639 AND W-PREFIX-NUM < 660)                    
056900            MOVE JA TO NEDCAR-AVTAL                                       
057000        END-IF                                                            
057100        PERFORM IMS-GET-WLARTC23                                          
057200     END-PERFORM                                                          
057300     IF AVTAL-FINNS = NEJ                                                 
057400        MOVE ZERO TO SPAR-IDAVTAL                                         
057500                     SPAR-IDAVTAL-PV                                      
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 D2-SKAPA-U05-FIL SECTION.                                                
058000                                                                          
058100     PERFORM S07-NOLLA-U05-AREA                                           
058200     MOVE REPL               TO  U05-ATGARD                               
058300     MOVE 'WDK611'           TO  U05-IDSEGM                               
058400     MOVE W-IDARTNR          TO  U05-IDARTNR                              
058500     MOVE CLAG-FLMANAT       TO  U05-FLMANAT                              
058600     MOVE CLAG-FLMANBK       TO  U05-FLMANBK                              
058700     MOVE CLAG-FLMANGK       TO  U05-FLMANGK                              
058800     MOVE CLAG-FLMANKP       TO  U05-FLMANKP                              
058900     MOVE CLAG-FLMANLT       TO  U05-FLMANLT                              
059000     MOVE CLAG-FLMANQ        TO  U05-FLMANQ                               
059100     MOVE CLAG-IDANSK        TO  U05-IDANSK                               
059200     MOVE CLAG-IDINK         TO  U05-IDINK                                
059300     MOVE CLAG-IDPLANGR-AG   TO  U05-IDPLANGR-AG                          
059400     MOVE CLAG-KDAVT         TO  U05-KDAVT                                
059500     MOVE CLAG-KDGK          TO  U05-KDGK                                 
059600     MOVE CLAG-KDHF          TO  U05-KDHF                                 
059700     MOVE CLAG-KDKSP         TO  U05-KDKSP                                
059800     MOVE CLAG-KVBK          TO  U05-KVBK                                 
059900     MOVE CLAG-KVKP          TO  U05-KVKP                                 
060000     MOVE CLAG-KVQ           TO  U05-KVQ                                  
060100     MOVE CLAG-KVQ-JUST      TO  U05-KVQ-JUST                             
060200     MOVE CLAG-KVVECKOR-AT   TO  U05-KVVECKOR-AT                          
060300     MOVE CLAG-KVVECKOR-LT   TO  U05-KVVECKOR-LT                          
060400     MOVE CLAG-REDIRLEV      TO  U05-REDIRLEV                             
060500     MOVE CLAG-KDLEVPLF      TO  U05-KDLEVPLF                             
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900                                                                          
061000 D-BEARBETA-SEGM-11 SECTION.                                              
061100******************************************************************        
061200*                                                                *        
061300*    TESTAR VILKET FÄLT SOM SKALL ÄNDRAS OCH DÄREFTER            *        
061400*    KONTROLLERAS OCH UPPDATERAS FÄLTET. I VISSA FALL HÄMTAS     *        
061500*    INFORMATION FRÅN LEVERANTÖRSREGISTRET                       *        
061600*                                                                *        
061700******************************************************************        
061800                                                                          
061900     EVALUATE IN-043-IDELMT                                               
062000        WHEN 'KVVECKOR-AT'                                                
062100             MOVE 3 TO MAX-KVHELTAL                                       
062200             MOVE 0 TO MAX-KVDECIMAL                                      
062300             PERFORM S10-KONV-KONTR-NYTT                                  
062400             IF IN-IDFELKODX = SPACE                                      
062500               MOVE JA                   TO  CLAG-FLMANAT                 
062600               MOVE DEC-IDEDITDATA       TO  CLAG-KVVECKOR-AT             
062700               PERFORM S01-SKRIV-2214                                     
062800             END-IF                                                       
062900                                                                          
063000        WHEN 'KVVECKOR-LT'                                                
063100             MOVE 3 TO MAX-KVHELTAL                                       
063200             MOVE 0 TO MAX-KVDECIMAL                                      
063300             PERFORM S10-KONV-KONTR-NYTT                                  
063400             IF IN-IDFELKODX = SPACE                                      
063500               MOVE JA                   TO  CLAG-FLMANLT                 
063600               MOVE DEC-IDEDITDATA       TO  CLAG-KVVECKOR-LT             
063700               PERFORM S01-SKRIV-2214                                     
063800             END-IF                                                       
063900                                                                          
064000        WHEN 'KVBK'                                                       
064100             MOVE 7 TO MAX-KVHELTAL                                       
064200             MOVE 0 TO MAX-KVDECIMAL                                      
064300             PERFORM S10-KONV-KONTR-NYTT                                  
064400             IF IN-IDFELKODX = SPACE                                      
064500               MOVE JA                   TO  CLAG-FLMANBK                 
064600               MOVE DEC-IDEDITDATA       TO  CLAG-KVBK                    
064700             END-IF                                                       
064800                                                                          
064900        WHEN 'KVKP'                                                       
065000             MOVE 7 TO MAX-KVHELTAL                                       
065100             MOVE 0 TO MAX-KVDECIMAL                                      
065200             PERFORM S10-KONV-KONTR-NYTT                                  
065300             IF IN-IDFELKODX = SPACE                                      
065400               MOVE JA                   TO  CLAG-FLMANKP                 
065500               MOVE DEC-IDEDITDATA       TO  CLAG-KVKP                    
065600             END-IF                                                       
065700                                                                          
065800        WHEN 'KVQ'                                                        
065900             MOVE 7 TO MAX-KVHELTAL                                       
066000             MOVE 0 TO MAX-KVDECIMAL                                      
066100             PERFORM S10-KONV-KONTR-NYTT                                  
066200             IF IN-IDFELKODX = SPACE                                      
066300               MOVE JA                   TO  CLAG-FLMANQ                  
066400               MOVE DEC-IDEDITDATA       TO  CLAG-KVQ                     
066500             END-IF                                                       
066600                                                                          
066700        WHEN 'KVQ-JUST'                                                   
066800             MOVE 7 TO MAX-KVHELTAL                                       
066900             MOVE 0 TO MAX-KVDECIMAL                                      
067000             PERFORM S10-KONV-KONTR-NYTT                                  
067100             IF IN-IDFELKODX = SPACE                                      
067200               MOVE DEC-IDEDITDATA       TO CLAG-KVQ-JUST                 
067300             END-IF                                                       
067400                                                                          
067500        WHEN 'KDAVT'                                                      
067600             MOVE 1 TO MAX-KVHELTAL                                       
067700             MOVE 0 TO MAX-KVDECIMAL                                      
067800             PERFORM S10-KONV-KONTR-NYTT                                  
067900             IF IN-IDFELKODX = SPACE                                      
068000               PERFORM DA-TRANS-INKOP                                     
068100               MOVE DEC-IDEDITDATA  TO CLAG-KDAVT                         
068200               IF CLAG-KDAVT = 0                                          
068300                  MOVE 0 TO CLAG-KDHF                                     
068400               END-IF                                                     
068500               PERFORM S01-SKRIV-2214                                     
068600             END-IF                                                       
068700                                                                          
068800        WHEN 'IDANSK'                                                     
068900             PERFORM DB-UPPDAT-IDANSK                                     
069000                                                                          
069100        WHEN 'FLMANAT'                                                    
069200             PERFORM DC-UPPDAT-FLMANAT                                    
069300                                                                          
069400        WHEN 'FLMANLT'                                                    
069500             PERFORM DD-UPPDAT-FLMANLT                                    
069600                                                                          
069700        WHEN 'IDPLANGR-AG'                                                
069800             PERFORM DE-UPPDAT-IDPLANGR-AG                                
069900                                                                          
070000        WHEN 'KDHF'                                                       
070100             PERFORM DF-UPPDAT-KDHF                                       
070200                                                                          
070300        WHEN 'IDINK'                                                      
070400             MOVE IN-043-IDFVARDE-NYTT TO CLAG-IDINK                      
070500             PERFORM S01-SKRIV-2214                                       
070600                                                                          
070700        WHEN 'REDIRLEV'                                                   
070800             PERFORM DG-UPPDAT-REDIRLEV                                   
070900                                                                          
071000        WHEN 'KDGK'                                                       
071100             PERFORM DH-UPPDAT-KDGK                                       
071200                                                                          
071300*       WHEN 'KDLTK'                                                      
071400*            PERFORM DI-UPPDAT-KDLTK   UTGÅR                              
071500                                                                          
071600        WHEN 'FLMANGK'                                                    
071700             PERFORM DJ-UPPDAT-FLMANGK                                    
071800                                                                          
071900        WHEN 'KDLEVPLF'                                                   
072000             PERFORM DK-UPPDAT-KDLEVPLF                                   
072100                                                                          
072200     END-EVALUATE                                                         
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600                                                                          
072700 DA-TRANS-INKOP SECTION.                                                  
072800                                                                          
072900     IF PV-AVTAL = JA                                                     
073000       MOVE SPAR-IDAVTAL-PV  TO W-IDAVTAL-RED                             
073100       IF CLAG-KDAVT = 1 AND DEC-IDEDITDATA  = 0                          
073200          IF CLAG-IDINK (1:3) NUMERIC                                     
073300             MOVE CLAG-IDINK (1:3) TO TEST-IDINK                          
073400          ELSE                                                            
073500             MOVE ZERO             TO TEST-IDINK                          
073600          END-IF                                                          
073700          IF SPAR-IDAVTAL NOT = ZERO                                      
073800             PERFORM S04-SKAPA-B65                                        
073900             PERFORM S05-SKRIV-A310-POST-PV                               
074000          END-IF                                                          
074100        END-IF                                                            
074200     END-IF                                                               
074300     IF NEDCAR-AVTAL = JA                                                 
074400        IF SPAR-IDAVTAL NOT = ZERO                                        
074500           PERFORM S06-SKRIV-1142                                         
074600        END-IF                                                            
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100                                                                          
075200 DB-UPPDAT-IDANSK SECTION.                                                
075300******************************************************************        
075400*    KONTROLL OCH UPPDATERING AV IDANSK                          *        
075500******************************************************************        
075600                                                                          
075700         MOVE 3 TO MAX-KVHELTAL                                           
075800         MOVE 0 TO MAX-KVDECIMAL                                          
075900         PERFORM S10-KONV-KONTR-NYTT                                      
076000         IF IN-IDFELKODX = SPACE                                          
076100           IF CLAG-IDPLANGR-AG = 9                                        
076200           OR ART-IDLEVNR = SPACE OR '9998 '                              
076300                                                                          
076400               MOVE DEC-IDEDITDATA   TO  CLAG-IDANSK                      
076500               PERFORM S08-SKRIV-U33                                      
076600           ELSE                                                           
076700               MOVE 'F02' TO IN-IDFELKODX                                 
076800           END-IF                                                         
076900         END-IF                                                           
077000     .                                                                    
077100     EJECT                                                                
077200                                                                          
077300                                                                          
077400 DC-UPPDAT-FLMANAT SECTION.                                               
077500******************************************************************        
077600*    UPPDATERING AV FLMANAT. OM FLMANAT SÄTTS TILL NEJ UPPDATERAS*        
077700*    KVVECKOR-AT FRÅN LEVERANTÖRSREGISTRET OCH                   *        
077800*     2214-POST SKRIVS PÅ HÄNDELSEREGISTRET                      *        
077900******************************************************************        
078000                                                                          
078100     MOVE IN-043-IDFVARDE-NYTT TO  CLAG-FLMANAT                           
078200     IF CLAG-FLMANAT = NEJ                                                
078300       IF CLAG-KDHF > ZERO                                                
078400         MOVE 16                TO CLAG-KVVECKOR-AT                       
078500       ELSE                                                               
078600         MOVE ART-IDLEVNR TO W-IDLEVNR                                    
078700         PERFORM IMS-GET-WLLEVA01                                         
078800         IF SEGMENT-FINNS                                                 
078900            MOVE  LEV-KVVECKOR-AT  TO  CLAG-KVVECKOR-AT                   
079000         END-IF                                                           
079100       END-IF                                                             
079200       PERFORM S01-SKRIV-2214                                             
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700                                                                          
079800 DD-UPPDAT-FLMANLT SECTION.                                               
079900******************************************************************        
080000*    UPPDATERING AV FLMANLT. OM FLMANLT SÄTTS TILL NEJ UPPDATERAS*        
080100*    'VVECKOR-LT FRÅN LEVERANTÖRSREGISTRET OCH                   *        
080200*    2214-POST SKRIVS PÅ HÄNDELSEREGISTRET                       *        
080300******************************************************************        
080400                                                                          
080500     MOVE IN-043-IDFVARDE-NYTT TO CLAG-FLMANLT                            
080600     IF   CLAG-FLMANLT = NEJ                                              
080700       IF CLAG-KDHF > ZERO                                                
080800         IF CLAG-KDHF = 1                                                 
080900            MOVE 6        TO CLAG-KVVECKOR-LT                             
081000         ELSE                                                             
081100            MOVE 8        TO CLAG-KVVECKOR-LT                             
081200         END-IF                                                           
081300         PERFORM S01-SKRIV-2214                                           
081400       ELSE                                                               
081500         MOVE ART-IDLEVNR TO W-IDLEVNR                                    
081600         PERFORM IMS-GET-WLLEVA01                                         
081700         IF SEGMENT-FINNS                                                 
081800            MOVE  LEV-KVVECKOR-LT  TO  CLAG-KVVECKOR-LT                   
081900         END-IF                                                           
082000         PERFORM S01-SKRIV-2214                                           
082100       END-IF                                                             
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500                                                                          
082600 DE-UPPDAT-IDPLANGR-AG SECTION.                                           
082700******************************************************************        
082800*    KONTROLL OCH ÄNDRING AV IDPLANGR-AG                         *        
082900******************************************************************        
083000                                                                          
083100     MOVE 1 TO MAX-KVHELTAL                                               
083200     MOVE 0 TO MAX-KVDECIMAL                                              
083300     PERFORM S10-KONV-KONTR-NYTT                                          
083400     IF IN-IDFELKODX = SPACE                                              
083500       MOVE DEC-IDEDITDATA  TO CLAG-IDPLANGR-AG                           
083600       IF CLAG-IDPLANGR-AG > ZERO                                         
083700          IF CLAG-IDPLANGR-AG <= 8                                        
083800             MOVE ART-IDLEVNR TO W-IDLEVNR                                
083900             PERFORM IMS-GET-WLLEVA01                                     
084000             IF SEGMENT-FINNS                                             
084100                MOVE LEV-IDANSK-PG (CLAG-IDPLANGR-AG)                     
084200                                     TO CLAG-IDANSK                       
084300                PERFORM S08-SKRIV-U33                                     
084400             END-IF                                                       
084500          END-IF                                                          
084600       END-IF                                                             
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000                                                                          
085100                                                                          
085200 DF-UPPDAT-KDHF SECTION.                                                  
085300                                                                          
085400     MOVE 1 TO MAX-KVHELTAL                                               
085500     MOVE 0 TO MAX-KVDECIMAL                                              
085600     PERFORM S10-KONV-KONTR-NYTT                                          
085700     IF IN-IDFELKODX = SPACE                                              
085800       IF CLAG-KDHF = 0 OR DEC-IDEDITDATA  = 0                            
085900           PERFORM DFA-KOLLA-LEVNR                                        
086000           PERFORM S03-SKRIV-2222                                         
086100       END-IF                                                             
086200*                                                                         
086300       IF DEC-IDEDITDATA  > 0                                             
086400           MOVE 4 TO CLAG-KDAVT                                           
086500           IF CLAG-KDKSP > 1                                              
086600               MOVE +0 TO CLAG-KDKSP                                      
086700           END-IF                                                         
086800       ELSE                                                               
086900           MOVE +0 TO CLAG-KDAVT                                          
087000           PERFORM DFA-KOLLA-LEVNR                                        
087100       END-IF                                                             
087200*                                                                         
087300       IF SPAR-IDAVTAL NOT = ZERO                                         
087400          IF CLAG-KDHF = 0 AND DEC-IDEDITDATA > 0                         
087500             AND W-KDAVT-GAMLA = +1                                       
087600             IF CLAG-IDINK (1:3) NUMERIC                                  
087700                MOVE CLAG-IDINK (1:3) TO TEST-IDINK                       
087800             ELSE                                                         
087900                MOVE ZERO             TO TEST-IDINK                       
088000             END-IF                                                       
088100             IF PV-AVTAL = JA                                             
088200                MOVE SPAR-IDAVTAL-PV TO W-IDAVTAL-RED                     
088300                PERFORM S04-SKAPA-B65                                     
088400                PERFORM S05-SKRIV-A310-POST-PV                            
088500             END-IF                                                       
088600             IF NEDCAR-AVTAL = JA                                         
088700                PERFORM S06-SKRIV-1142                                    
088800             END-IF                                                       
088900          END-IF                                                          
089000       END-IF                                                             
089100                                                                          
089200       MOVE DEC-IDEDITDATA  TO CLAG-KDHF                                  
089300       IF CLAG-KDHF > ZERO                                                
089400          IF CLAG-FLMANLT = 'N'                                           
089500             IF CLAG-KDHF = 1                                             
089600                MOVE 6    TO CLAG-KVVECKOR-LT                             
089700             ELSE                                                         
089800                MOVE 8    TO CLAG-KVVECKOR-LT                             
089900             END-IF                                                       
090000          END-IF                                                          
090100          IF CLAG-FLMANAT = 'N'                                           
090200                MOVE 16   TO CLAG-KVVECKOR-AT                             
090300          END-IF                                                          
090400       END-IF                                                             
090500     END-IF                                                               
090600     .                                                                    
090700     EJECT                                                                
090800                                                                          
090900                                                                          
091000 DFA-KOLLA-LEVNR SECTION.                                                 
091100                                                                          
091200     IF ART-IDLEVNR = '100  ' OR '259  ' OR '1006 ' OR '1165 '            
091300        OR '1166 ' OR '1540 ' OR '1555 ' OR '1619 ' OR '1621 '            
091400        OR '2270 ' OR '3416 ' OR '4509 ' OR '4730 ' OR '1613 '            
091500        OR '1622 ' OR '1626 ' OR '1164 '                                  
091600        OR '1614 ' OR '1625 ' OR '1658 '                                  
091700        OR 'BT7SA' OR 'BSNRA' OR 'C7CUL' OR 'BL3WA' OR 'LAT6A'            
091800        OR 'BS8CA' OR 'LJVZA' OR 'BRPVA' OR 'BL4AB' OR 'AVKVA'            
091900        OR 'BSB5A' OR 'C7CUQ' OR 'AEOUC' OR 'AE2FL'                       
092000         MOVE +3 TO CLAG-KDAVT                                            
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400                                                                          
092500                                                                          
092600 DG-UPPDAT-REDIRLEV SECTION.                                              
092700******************************************************************        
092800*    KONTROLLERA OCH UPPDATERA REDIRLEV                          *        
092900******************************************************************        
093000                                                                          
093100     MOVE 3 TO MAX-KVHELTAL                                               
093200     MOVE 2 TO MAX-KVDECIMAL                                              
093300     PERFORM S10-KONV-KONTR-NYTT                                          
093400     IF IN-IDFELKODX = SPACE                                              
093500       IF DEC-KVDECIMAL = 0                                               
093600*      -- INGA DECIMALER ANGIVNA, UNDERFÖRSTÅTT 2                         
093700          DIVIDE 100 INTO DEC-IDEDITDATA                                  
093800       END-IF                                                             
093900       IF DEC-IDEDITDATA > 1.00                                           
094000           MOVE 'F07' TO IN-IDFELKODX                                     
094100       ELSE                                                               
094200           MOVE DEC-IDEDITDATA TO CLAG-REDIRLEV                           
094300       END-IF                                                             
094400     END-IF                                                               
094500     .                                                                    
094600     EJECT                                                                
094700                                                                          
094800                                                                          
094900 DH-UPPDAT-KDGK SECTION.                                                  
095000******************************************************************        
095100*    KONTROLLERA OCH UPPDATERA KDGK OCH EV FLMANGK               *        
095200******************************************************************        
095300                                                                          
095400     MOVE 1 TO MAX-KVHELTAL                                               
095500     MOVE 0 TO MAX-KVDECIMAL                                              
095600     PERFORM S10-KONV-KONTR-NYTT                                          
095700     IF IN-IDFELKODX = SPACE                                              
095800       IF DEC-IDEDITDATA  = 2                                             
095900       AND CLAG-KDERS > 10                                                
096000         MOVE 'F04' TO IN-IDFELKODX                                       
096100       ELSE                                                               
096200         MOVE DEC-IDEDITDATA  TO CLAG-KDGK                                
096300         MOVE JA       TO  CLAG-FLMANGK                                   
096400         PERFORM S01-SKRIV-2214                                           
096500       END-IF                                                             
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900                                                                          
097000                                                                          
097100 DJ-UPPDAT-FLMANGK SECTION.                                               
097200******************************************************************        
097300*    UPPDATERAR FLMANGK OCH EV KDGK.                             *        
097400*    OM KDGK ÄNDRAS SKRIVS EN 2214-POST PÅ HÄNDELSEREGISTRET     *        
097500******************************************************************        
097600                                                                          
097700     MOVE IN-043-IDFVARDE-NYTT TO CLAG-FLMANGK                            
097800     IF  CLAG-FLMANGK = NEJ                                               
097900         MOVE ART-IDLEVNR TO W-IDLEVNR                                    
098000         PERFORM IMS-GET-WLLEVA01                                         
098100         IF SEGMENT-FINNS                                                 
098200            MOVE LEV-KDGK TO CLAG-KDGK                                    
098300         END-IF                                                           
098400         PERFORM S01-SKRIV-2214                                           
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800                                                                          
098900 DK-UPPDAT-KDLEVPLF SECTION.                                              
099000******************************************************************        
099100*    UPPDATERAR KDLEVPLF                                         *        
099200******************************************************************        
099300                                                                          
099400     IF  IN-043-IDFVARDE-NYTT = 'J' OR 'S' OR 'G' OR 'N' OR 'P'           
099500                                                                          
099600         MOVE IN-043-IDFVARDE-NYTT TO CLAG-KDLEVPLF                       
099700     ELSE                                                                 
099800         MOVE '011' TO IN-IDFELKODX                                       
099900     END-IF                                                               
100000     .                                                                    
100100     EJECT                                                                
100200                                                                          
100300                                                                          
100400 E-SKRIV-FELPOST SECTION.                                                 
100500******************************************************************        
100600*                                                                *        
100700*    SKAPAR OCH SKRIVER FELPOSTER. FELKODEN FYLLS I DÅ MAN       *        
100800*    UPPTÄCKER FELET.                                            *        
100900*                                                                *        
101000******************************************************************        
101100                                                                          
101200     MOVE ZERO TO U02-AREA                                                
101300                                                                          
101400     IF IN-043-IDPTYP = '043'                                             
101500        MOVE 'R05'             TO U02-IDPTYP                              
101600        MOVE IN-043-IDELMT     TO U02-IDELMT                              
101700        MOVE IN-043-IDARTNR    TO U02-SORTBGP                             
101800     ELSE                                                                 
101900        MOVE IN-R08-IDPTYP     TO U02-IDPTYP                              
102000        MOVE IN-R08-IDARTNR    TO U02-SORTBGP                             
102100     END-IF                                                               
102200                                                                          
102300     MOVE 0000          TO U02-IDDISTR                                    
102400     MOVE IN-IDFELKODX  TO U02-IDFELKODX                                  
102500     MOVE IN-IDANSK-FEL TO U02-IDKUNDNR                                   
102600                                                                          
102700     IF U02-IDPTYP = 'R05'                                                
102800       WRITE U02-R05-POST FROM U02-AREA                                   
102900     ELSE                                                                 
103000       WRITE U02-R08-POST FROM U02-AREA                                   
103100     END-IF                                                               
103200                                                                          
103300     MOVE 'W21703'   TO POSTSUM-FDNAMN                                    
103400     MOVE 'W21702D2' TO POSTSUM-DDNAMN2                                   
103500     MOVE U02-IDPTYP TO POSTSUM-TRANSTYP                                  
103600     CALL POSTSUM USING POSTSUM-PARM                                      
103700     MOVE SPACE TO IN-IDFELKODX                                           
103800     .                                                                    
103900     EJECT                                                                
104000                                                                          
104100                                                                          
104200 Z-AVSLUTNING SECTION.                                                    
104300******************************************************************        
104400*                                                                *        
104500*    STÄNG ALLA FILLER                                           *        
104600*    SKRIV POSTSUMS RÄKNEVERK                                    *        
104700*                                                                *        
104800******************************************************************        
104900                                                                          
105000     CLOSE W21701                                                         
105100           W21703                                                         
105200           W21707                                                         
105300           W21705                                                         
105400           W21731                                                         
105500           W21732                                                         
105600           W21733                                                         
105700                                                                          
105800     MOVE 'S' TO POSTSUM-OPKOD                                            
105900     CALL POSTSUM USING POSTSUM-PARM                                      
106000     .                                                                    
106100     EJECT                                                                
106200                                                                          
106300                                                                          
106400 S01-SKRIV-2214 SECTION.                                                  
106500******************************************************************        
106600*    SKRIVER 2214-POST                                           *        
106700******************************************************************        
106800                                                                          
106900     MOVE SPACE              TO U32-AREA                                  
107000     MOVE IN-043-IDARTNR     TO 2213-IDARTNR                              
107100     MOVE '2213'             TO 2213-IDHTYP                               
107200                                                                          
107300     WRITE U32-POST FROM U32-AREA                                         
107400                                                                          
107500     MOVE 'W21732'   TO POSTSUM-FDNAMN                                    
107600     MOVE 'W21702D7' TO POSTSUM-DDNAMN2                                   
107700     MOVE '2213'     TO POSTSUM-TRANSTYP                                  
107800     CALL POSTSUM USING POSTSUM-PARM                                      
107900     .                                                                    
108000     EJECT                                                                
108100                                                                          
108200 S02-SKRIV-2204 SECTION.                                                  
108300******************************************************************        
108400*    SKRIVER 2204-POST                                           *        
108500******************************************************************        
108600                                                                          
108700     MOVE SPACE             TO U32-AREA                                   
108800                                                                          
108900     MOVE IN-043-IDARTNR    TO 2204-IDARTNR                               
109000     MOVE 13                TO 2204-KDLPORS                               
109100     MOVE WC-CDC-SE         TO 2204-IDDC                                  
109200     MOVE '2204'            TO 2204-IDHTYP                                
109300                                                                          
109400     WRITE U32-POST FROM U32-AREA                                         
109500                                                                          
109600     MOVE 'W21732'   TO POSTSUM-FDNAMN                                    
109700     MOVE 'W21702D7' TO POSTSUM-DDNAMN2                                   
109800     MOVE '2204'     TO POSTSUM-TRANSTYP                                  
109900     CALL POSTSUM USING POSTSUM-PARM                                      
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300                                                                          
110400 S03-SKRIV-2222 SECTION.                                                  
110500******************************************************************        
110600*    SKRIVER 2222-POST PÅ                                        *        
110700******************************************************************        
110800                                                                          
110900     MOVE SPACE             TO U31-AREA                                   
111000                                                                          
111100     MOVE IN-043-IDARTNR    TO 2222-IDARTNR                               
111200     MOVE 'W217'            TO 2222-IDSYSTEM                              
111300     MOVE ART-IDLEVNR       TO 2222-IDLEVNR                               
111400     MOVE SPACES            TO 2222-IDLEVNR-SHIP                          
111500     MOVE 'R01'             TO 2222-IDPTYP                                
111600                                                                          
111700     WRITE U31-POST FROM U31-AREA                                         
111800                                                                          
111900     MOVE 'W21731'   TO POSTSUM-FDNAMN                                    
112000     MOVE 'W21702D6' TO POSTSUM-DDNAMN2                                   
112100     MOVE '2222'     TO POSTSUM-TRANSTYP                                  
112200     CALL POSTSUM USING POSTSUM-PARM                                      
112300     .                                                                    
112400     EJECT                                                                
112500                                                                          
112600                                                                          
112700 S04-SKAPA-B65 SECTION.                                                   
112800                                                                          
112900     MOVE SPACE TO A310-LEVNUM-GODSM                                      
113000                   A310-ANT-BESTANN                                       
113100     MOVE 'B65' TO A310-KT                                                
113200     MOVE DAGENS-DATUM TO A310-DATUM-UTSKR                                
113300     IF ART-IDLEVNR (5:1) = SPACE                                         
113400***    LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                    
113500       MOVE ZERO           TO TALLY                                       
113600       INSPECT ART-IDLEVNR TALLYING TALLY                                 
113700                          FOR CHARACTERS BEFORE INITIAL SPACE             
113800       IF TALLY = ZERO                                                    
113900          MOVE ZERO        TO WS-IDLEVNR-NUM                              
114000       ELSE                                                               
114100          MOVE ART-IDLEVNR (1:TALLY)                                      
114200                           TO WS-IDLEVNR-NUM                              
114300       END-IF                                                             
114400       MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                                 
114500     ELSE                                                                 
114600       MOVE ART-IDLEVNR    TO A310-LEVNUM                                 
114700     END-IF                                                               
114800     MOVE IN-043-IDARTNR TO W-IDARTNR-8                                   
114900     MOVE W-IDARTNR-8 TO A310-ARTNR                                       
115000     MOVE SPAR-IDAVTAL-PV TO W-IDAVTAL-RED                                
115100     MOVE W-PREFIX TO A310-BESTPREF                                       
115200     MOVE W-AVTALNR TO A310-BESTLNR                                       
115300     MOVE W-SUFFIX TO A310-BESTSUFF                                       
115400     .                                                                    
115500     EJECT                                                                
115600                                                                          
115700                                                                          
115800 S05-SKRIV-A310-POST-PV SECTION.                                          
115900                                                                          
116000     WRITE UT-A310-PV-POST FROM A310-AREA                                 
116100     MOVE 'W21707' TO POSTSUM-FDNAMN                                      
116200     MOVE 'W21702D4' TO POSTSUM-DDNAMN2                                   
116300     MOVE 'A310' TO POSTSUM-TRANSTYP                                      
116400     CALL POSTSUM USING POSTSUM-PARM                                      
116500     .                                                                    
116600     EJECT                                                                
116700                                                                          
116800 S06-SKRIV-1142 SECTION.                                                  
116900******************************************************************        
117000*    SKRIVER 1142-POST                                           *        
117100******************************************************************        
117200                                                                          
117300     MOVE SPACE             TO U32-AREA                                   
117400                                                                          
117500     MOVE IN-043-IDARTNR    TO 1142-IDARTNR                               
117600     MOVE '2'               TO 1142-KDSEGKEY                              
117700     MOVE 'A'               TO 1142-KDSVAR                                
117800     MOVE '1142'            TO 1142-IDHTYP                                
117900                                                                          
118000     WRITE U32-POST FROM U32-AREA                                         
118100                                                                          
118200     MOVE 'W21732'   TO POSTSUM-FDNAMN                                    
118300     MOVE 'W21702D7' TO POSTSUM-DDNAMN2                                   
118400     MOVE '1142'     TO POSTSUM-TRANSTYP                                  
118500     CALL POSTSUM USING POSTSUM-PARM                                      
118600     .                                                                    
118700     EJECT                                                                
118800                                                                          
118900 S07-NOLLA-U05-AREA  SECTION.                                             
119000******************************************************************        
119100*    NOLLSTÄLLER AREAN FÖR DB-UPPDATERINGAR                      *        
119200******************************************************************        
119300                                                                          
119400     MOVE SPACE TO U05-AREA                                               
119500     MOVE ZERO  TO U05-IDANSK                                             
119600                   U05-IDINK                                              
119700                   U05-IDPLANGR-AG                                        
119800                   U05-KDAVT                                              
119900                   U05-KDGK                                               
120000                   U05-KDHF                                               
120100                   U05-KDKSP                                              
120200                   U05-KVBK                                               
120300                   U05-KVKP                                               
120400                   U05-KVQ                                                
120500                   U05-KVQ-JUST                                           
120600                   U05-KVVECKOR-AT                                        
120700                   U05-KVVECKOR-LT                                        
120800                   U05-REDIRLEV                                           
120900                   U05-KDNOTTYP                                           
121000     MOVE SPACE TO U05-KDLEVPLF                                           
121100     .                                                                    
121200     EJECT                                                                
121300                                                                          
121400 S08-SKRIV-U33 SECTION.                                                   
121500******************************************************************        
121600*    SKRIVER U33-POST                                            *        
121700******************************************************************        
121800                                                                          
121900     MOVE SPACE             TO U33-AREA                                   
122000                                                                          
122100     MOVE IN-043-IDARTNR    TO U33-IDARTNR                                
122200     MOVE 15                TO U33-KDLPORS                                
122300                                                                          
122400     WRITE U33-POST FROM U33-AREA                                         
122500                                                                          
122600     MOVE 'W21733'   TO POSTSUM-FDNAMN                                    
122700     MOVE 'W21702D8' TO POSTSUM-DDNAMN2                                   
122800     MOVE 'U33'      TO POSTSUM-TRANSTYP                                  
122900     CALL POSTSUM USING POSTSUM-PARM                                      
123000     .                                                                    
123100     EJECT                                                                
123200                                                                          
123300 S10-KONV-KONTR-NYTT  SECTION.                                            
123400                                                                          
123500     MOVE MAX-KVHELTAL         TO DEC-KVHELTAL                            
123600     MOVE MAX-KVDECIMAL        TO DEC-KVDECIMAL                           
123700     MOVE IN-043-IDFVARDE-NYTT TO DEC-IDFRIDATA                           
123800     CALL WDECEDIT USING DEC-WDECAREA                                     
123900                                                                          
124000     IF DEC-KDSVAR-OK                                                     
124100       IF IN-043-KDTECKEN-NYTT = '-'                                      
124200          COMPUTE DEC-IDEDITDATA =  - DEC-IDEDITDATA                      
124300       END-IF                                                             
124400     ELSE                                                                 
124500       IF DEC-KVHELTAL > MAX-KVHELTAL                                     
124600       OR DEC-KVDECIMAL > MAX-KVDECIMAL                                   
124700         MOVE '01E' TO IN-IDFELKODX                                       
124800       ELSE                                                               
124900         MOVE '011' TO IN-IDFELKODX                                       
125000       END-IF                                                             
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400                                                                          
125500                                                                          
125600 IMS-GET-WLARTC01 SECTION.                                                
125700                                                                          
125800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
125900          DELIMITED BY SIZE INTO SSA1                                     
126000     MOVE '  GE' TO GODK-STATUSKODER                                      
126100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
126200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
126300     PERFORM IMS-STATUSKONTROLL                                           
126400     .                                                                    
126500     EJECT                                                                
126600                                                                          
126700                                                                          
126800 IMS-GET-WLARTC11 SECTION.                                                
126900                                                                          
127000     STRING 'WLARTC11(KDSEGKEY =1)'                                       
127100          DELIMITED BY SIZE INTO SSA1                                     
127200     MOVE '  GE' TO GODK-STATUSKODER                                      
127300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
127400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     .                                                                    
127700     EJECT                                                                
127800                                                                          
127900                                                                          
128000 IMS-GHU-WLARTC11 SECTION.                                                
128100                                                                          
128200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
128300          DELIMITED BY SIZE INTO SSA1                                     
128400     STRING 'WLARTC11(KDSEGKEY =1)'                                       
128500          DELIMITED BY SIZE INTO SSA2                                     
128600     MOVE '  GE' TO GODK-STATUSKODER                                      
128700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-11B SSA1 SSA2            
128800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     EJECT                                                                
129200                                                                          
129300                                                                          
129400 IMS-GET-WLARTC23 SECTION.                                                
129500                                                                          
129600     MOVE 'WLARTC23 ' TO SSA1                                             
129700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
129800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-2 SSA1                   
129900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     EJECT                                                                
130200     .                                                                    
130300                                                                          
130400                                                                          
130500 IMS-GET-WLARTC25 SECTION.                                                
130600                                                                          
130700     STRING 'WLARTC11(KDSEGKEY =1)'                                       
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
131000             DELIMITED BY SIZE INTO SSA2                                  
131100     MOVE '  GE' TO GODK-STATUSKODER                                      
131200     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2             
131300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
131400     PERFORM IMS-STATUSKONTROLL                                           
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800                                                                          
131900 IMS-GET-WLLEVA01 SECTION.                                                
132000                                                                          
132100     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
132200            DELIMITED BY SIZE INTO SSA1                                   
132300     MOVE '  ' TO GODK-STATUSKODER                                        
132400     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-2 SSA1                    
132500     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 IMS-STATUSKONTROLL SECTION.                                              
133100                                                                          
133200     SET STATUS-IX TO 1                                                   
133300     SEARCH GODK-STATUS                                                   
133400       AT END                                                             
133500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
133600         DISPLAY FELTEXT                                                  
133700         CALL FELLOG                                                      
133800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
133900         CONTINUE                                                         
134000     END-SEARCH                                                           
134100     .                                                                    
