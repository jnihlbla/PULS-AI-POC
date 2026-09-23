000101*                                                                         
000201******************************************************************        
000301*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0155      *        
000401******************************************************************        
000501*                                                                         
000601 ID DIVISION.                                                             
000701     SKIP2                                                                
000801 PROGRAM-ID.     W4079500.                                                
000901*AUTHOR.         MÅNS SAMUELSSON.                                         
001001*DATE-WRITTEN.   95/10/03.                                                
001101                                                                          
001201*    REMARKS.                                                             
001301*                                                                         
001401*    FUNKTION:                                                            
001501*        BAKGRUNDS MPP SOM SKRIVER UT INLÄGGNINGSLISTA.                   
001601*        STARTAS AV W40738 OCH W40748.                                    
001701*                                                                         
001801*        PROGRAMMET ÄR KOPIERAT FRÅN W60199                               
001901*                                                                         
002001*        PROGRAMMET          LÄSER      WLKREJ (WDA3E)                    
002101*                                       WLKREE (WDA3)                     
002201*                                       WLARTC (WDK6)                     
002301*                                                                         
002401*    PGM-ÄNDRING:                                                         
002501*        SCR/ETRACKER NR. 829496  DATUM 2004-09                           
002601*                                                                         
002701*    INDATA.                                                              
002801*        TRANSAKTION: W4T795X                                             
002901*        MID:         W4I79501                                            
003001*                                                                         
003101*    UTDATA.                                                              
003201*        INLÄGGNINGSLISTA                                                 
003301                                                                          
003401     SKIP3                                                                
003501 ENVIRONMENT DIVISION.                                                    
003601     EJECT                                                                
003701 DATA DIVISION.                                                           
003801 WORKING-STORAGE SECTION.                                                 
003901                                                                          
004001*    -- CHECKED BY WY2000                                                 
004101 77  IDPGM                       PIC X(08)   VALUE 'W4079500'.            
004201                                                                          
004301*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004401 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004501                                                                          
004601 77  JA                          PIC X       VALUE 'J'.                   
004701 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004901 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
005001 77  MAX-TAB-IX                  PIC S9(3)   VALUE +3 COMP-3.             
005101 77  MAX-KVRADER                 PIC S9(3)   VALUE +40 COMP-3.            
005201 77  W-KVRADER                   PIC S9(7)   VALUE ZERO COMP-3.           
005301 77  W-ADLAGOMR                  PIC X(4)    VALUE SPACE.                 
005401 77  W-IDSIDNR                   PIC S9(3)   VALUE ZERO COMP-3.           
005501                                                                          
005601     EJECT                                                                
005701*      --- VALID IDDC CODES                                               
005801*                                                                         
005901*01    -COPY WWDC99                                                       
006001       EJECT                                                              
006101*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006201 01  GENERELLA-SUBPROGRAM.                                                
006301     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006401     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006501     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
006601     SKIP3                                                                
006701* VARIABLER TILL SUBPROGRAM W006PRR1                                      
006801*01  -COPY W006PRAR                                                       
006901     SKIP2                                                                
007001     EJECT                                                                
007101 01  WS-RAPP-AREA.                                                        
007201     03  WS-RAPP-LISTID.                                                  
007301         05  FILLER              PIC X(5)    VALUE 'ILIST'.               
007401         05  WS-RAPP-IDILIST     PIC 9(5)    VALUE ZERO.                  
007501     03  WS-RAPP-LISTRAD.                                                 
007601         05  FILLER              PIC X(2)    VALUE SPACE.                 
007701         05  WS-RAPP-RAD         PIC X(130).                              
007801     03  WS-DUMMY                PIC X(1).                                
007901     03  WS-RAPP-PRINTER         PIC X(8).                                
008001     EJECT                                                                
008101*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008201*                                                                         
008301 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008401     SKIP3                                                                
008501*01  MID -COPY W4I79501                                                   
008601     EJECT                                                                
008701 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008801     SKIP3                                                                
008901*01  -COPY WMSGAREA                                                       
009001     EJECT                                                                
009101*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009201*                                                                         
009301     EJECT                                                                
009401 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009501     SKIP3                                                                
009601 01  NYCKLAR-TILL-DLI.                                                    
009701     03  W-WDA2E1KY-MIN-X.                                                
009801        05  W-A2E1KY-IDDC-MIN        PIC  X(2) VALUE SPACE.               
009901        05  W-A2E1KY-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
010001        05  W-A2E1KY-ADLAGOMR-MIN    PIC S9(3) COMP-3 VALUE ZERO.         
010101        05  W-A2E1KY-ADGANG-MIN      PIC S9(3) COMP-3 VALUE ZERO.         
010201        05  W-A2E1KY-ADPLATS-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
010301        05  W-A2E1KY-IDDISTR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
010401        05  W-A2E1KY-IDKUNDNR-MIN    PIC S9(7) COMP-3 VALUE ZERO.         
010501        05  W-A2E1KY-IDRAPPNR-MIN    PIC  X(7) VALUE SPACE.               
010601        05  W-A2E1KY-IDARTNR-MIN     PIC S9(9) COMP-3 VALUE ZERO.         
010701        05  W-A2E1KY-IDRADNR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
010801                                                                          
010901     03  W-WDA2E1KY-MAX-X.                                                
011001        05  W-A2E1KY-IDDC-MAX        PIC  X(2) VALUE SPACE.               
011101        05  W-A2E1KY-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
011201        05  W-A2E1KY-ADLAGOMR-MAX    PIC S9(3) COMP-3 VALUE ZERO.         
011301        05  W-A2E1KY-ADGANG-MAX      PIC S9(3) COMP-3 VALUE ZERO.         
011401        05  W-A2E1KY-ADPLATS-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
011501        05  W-A2E1KY-IDDISTR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
011601        05  W-A2E1KY-IDKUNDNR-MAX    PIC S9(7) COMP-3 VALUE ZERO.         
011701        05  W-A2E1KY-IDRAPPNR-MAX    PIC  X(7) VALUE SPACE.               
011801        05  W-A2E1KY-IDARTNR-MAX     PIC S9(9) COMP-3 VALUE ZERO.         
011901        05  W-A2E1KY-IDRADNR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
012001                                                                          
012101     03  W-IDLEVANM-X.                                                    
012201         05  W-IDDISTR           PIC S9(5)    COMP-3 VALUE ZERO.          
012301         05  W-IDKUNDNR          PIC S9(7)    COMP-3 VALUE ZERO.          
012401         05  W-IDRAPPNR          PIC X(7)     VALUE SPACE.                
012501                                                                          
012601     03  W-WDA211KY-X.                                                    
012701       04   W-IDARTNR-X.                                                  
012801        05  W-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.         
012901       04   W-IDRADNR                PIC S9(5) COMP-3 VALUE ZERO.         
013001                                                                          
013101     03  W-KDSEGKEY-X.                                                    
013201        05  W-KDSEGKEY               PIC 9(1)        VALUE 1.             
013301                                                                          
013401     03  W-IDDC-X.                                                        
013501        05  W-IDDC                   PIC  X(2)        VALUE SPACE.        
013601                                                                          
013701     03  W-IDSKYLT-X.                                                     
013801        05  W-IDSKYLT                PIC  X(3)        VALUE 'S  '.        
013901                                                                          
014001     SKIP2                                                                
014101*    --- STATUS-KOD FRÅN IMS                                              
014201 01  STATUS-WS                   PIC XX.                                  
014301     88  SEGMENT-FINNS                       VALUE '  '.                  
014401     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014501     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014601     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014701     SKIP2                                                                
014801 01  GODK-STATUSKODER.                                                    
014901     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015001     SKIP3                                                                
015101 01  SSA1                        PIC X(112).                              
015201 01  SSA2                        PIC X(64).                               
015301 01  SSA3                        PIC X(64).                               
015401     EJECT                                                                
015501*    --- IMS FUNKTIONSKODER                                               
015601*01  -COPY W0003                                                          
015701     EJECT                                                                
015801*    ---  DLI INPUT-OUTPUT AREA                                           
015901 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016001     SKIP3                                                                
016101 01  DLI-IO-AREA1.                                                        
016201     03  IO-AREA1                PIC X(1200)  VALUE SPACE.                
016301     SKIP3                                                                
016401     03  WLKREE01 REDEFINES IO-AREA1.                                     
016501*        05  -COPY WDA201                                                 
016601     EJECT                                                                
016701     03  WLKREE11 REDEFINES IO-AREA1.                                     
016801*        05  -COPY WDA211                                                 
016901     EJECT                                                                
017001     03  WLKREE21 REDEFINES IO-AREA1.                                     
017101*        05  -COPY WDA221                                                 
017201     EJECT                                                                
017301     03  WLKREJ01 REDEFINES IO-AREA1.                                     
017401*        05  -COPY WDA2E1                                                 
017501     EJECT                                                                
017601     03  WLARTC11 REDEFINES IO-AREA1.                                     
017701*        05  -COPY WDK611   -PRE ARTC-                                    
017801     EJECT                                                                
017901     03  WLARTS11 REDEFINES IO-AREA1.                                     
018001*        05  -COPY WDK711   -PRE ARTS-                                    
018101     EJECT                                                                
018201     03  WLBENA11 REDEFINES IO-AREA1.                                     
018301*        05  -COPY WDD311                                                 
018401     EJECT                                                                
018501*  PRINTRADER FÖR INLÄGGNINGSLISTA RAPPORT                                
018601                                                                          
018701 01  LIST-HRAD1.                                                          
018801     03   FILLER                  PIC X(1)  VALUE SPACE.                  
018901     03   FILLER                  PIC X(22) VALUE                         
019001                                        'VOLVO CAR AFTER SALES '.         
019101     03   FILLER                  PIC X(19) VALUE SPACE.                  
019201     03   FILLER                  PIC X(6)  VALUE 'W40795'.               
019301     03   FILLER                  PIC X(11) VALUE SPACE.                  
019401     03   FILLER                  PIC X(19) VALUE                         
019501                                        'INLÄGGNINGSLISTA NR'.            
019601     03   FILLER                  PIC X(3)  VALUE SPACE.                  
019701     03   HRAD1-IDILIST           PIC Z(4)9 VALUE ZERO.                   
019801     03   FILLER                  PIC X(12) VALUE SPACE.                  
019901     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
020001     03   FILLER                  PIC X(8)  VALUE SPACE.                  
020101     03   FILLER                  PIC X(4)  VALUE 'SID '.                 
020201     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
020301                                                                          
020401 01  LIST-HRAD1-ENG.                                                      
020501     03   FILLER                  PIC X(1)  VALUE SPACE.                  
020601     03   FILLER                  PIC X(22) VALUE                         
020701                                        'VOLVO CAR AFTER SALES '.         
020801     03   FILLER                  PIC X(19) VALUE SPACE.                  
020901     03   FILLER                  PIC X(6)  VALUE 'W40795'.               
021001     03   FILLER                  PIC X(11) VALUE SPACE.                  
021101     03   FILLER                  PIC X(19) VALUE                         
021201                                        'BINNING LIST    NO.'.            
021301     03   FILLER                  PIC X(3)  VALUE SPACE.                  
021401     03   HRAD1-IDILIST-ENG       PIC Z(4)9 VALUE ZERO.                   
021501     03   FILLER                  PIC X(12) VALUE SPACE.                  
021601     03   HRAD1-DATUM-ENG         PIC X(6)  VALUE SPACE.                  
021701     03   FILLER                  PIC X(7)  VALUE SPACE.                  
021801     03   FILLER                  PIC X(5)  VALUE 'PAGE'.                 
021901     03   HRAD1-IDSIDNR-ENG       PIC Z(2)9 VALUE ZERO.                   
022001                                                                          
022101 01  LIST-HRUB3.                                                          
022201     03   FILLER                  PIC X(5)  VALUE SPACE.                  
022301     03   FILLER                  PIC X(10) VALUE 'LAGERPLATS'.           
022401     03   FILLER                  PIC X(5)  VALUE SPACE.                  
022501     03   FILLER                  PIC X(6)  VALUE 'ARTNR '.               
022601     03   FILLER                  PIC X(10) VALUE 'BENÄMNING '.           
022701     03   FILLER                  PIC X(08) VALUE SPACE.                  
022801     03   FILLER                  PIC X(5)  VALUE 'ANTAL'.                
022901     03   FILLER                  PIC X(2)  VALUE SPACE.                  
023001     03   FILLER                  PIC X(9)  VALUE '  PB-SEP '.            
023101     03   FILLER                  PIC X(3)  VALUE 'FT '.                  
023201     03   FILLER                  PIC X(6)  VALUE 'DISTR '.               
023301     03   FILLER                  PIC X(8)  VALUE '   KUND '.             
023401     03   FILLER                  PIC X(10) VALUE '   RAPPNR '.           
023501     03   FILLER                  PIC X(10) VALUE 'NOTERINGAR'.           
023601                                                                          
023701 01  LIST-HRUB3-ENG.                                                      
023801     03   FILLER                  PIC X(5)  VALUE SPACE.                  
023901     03   FILLER                  PIC X(10) VALUE 'STORAGE   '.           
024001     03   FILLER                  PIC X(5)  VALUE SPACE.                  
024101     03   FILLER                  PIC X(6)  VALUE 'PARTNO'.               
024201     03   FILLER                  PIC X(11) VALUE 'DESCRIPTION'.          
024301     03   FILLER                  PIC X(07) VALUE SPACE.                  
024401     03   FILLER                  PIC X(5)  VALUE 'QTY  '.                
024501     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024601     03   FILLER                  PIC X(9)  VALUE '  PR-SEP '.            
024701     03   FILLER                  PIC X(3)  VALUE 'PC '.                  
024801     03   FILLER                  PIC X(6)  VALUE 'DISTR '.               
024901     03   FILLER                  PIC X(8)  VALUE '   CUST '.             
025001     03   FILLER                  PIC X(10) VALUE ' REPORTNO.'.           
025101     03   FILLER                  PIC X(10) VALUE 'NOTES     '.           
025201                                                                          
025301 01  LIST-LRAD.                                                           
025401     03   FILLER                  PIC X(4)  VALUE SPACE.                  
025501     03   FILLER                  PIC X(1)  VALUE SPACE.                  
025601     03   LRAD-ADLAGOMR           PIC Z(1)9.                              
025701     03   FILLER REDEFINES LRAD-ADLAGOMR.                                 
025801          05 LRAD-NA-ADLAGOMR     PIC 9(2).                               
025901     03   FILLER                  PIC X(1)  VALUE SPACE.                  
026001     03   LRAD-ADGANG             PIC Z(1)9.                              
026101     03   FILLER REDEFINES LRAD-ADGANG.                                   
026201          05 LRAD-NA-ADGANG       PIC 9(2).                               
026301     03   FILLER                  PIC X(1)  VALUE SPACE.                  
026401     03   LRAD-ADPLATS            PIC Z(4)9.                              
026501     03   FILLER REDEFINES LRAD-ADPLATS.                                  
026601          05 LRAD-NA-ADPLATS      PIC 9(5).                               
026701     03   FILLER                  PIC X(3)  VALUE SPACE.                  
026801     03   LRAD-IDARTNR            PIC Z(7)9.                              
026901     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027001     03   LRAD-BEART              PIC X(13).                              
027101     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027201     03   LRAD-KVLEVANM-KVAR      PIC Z(5)9.                              
027301     03   FILLER                  PIC X(2)  VALUE SPACE.                  
027401     03   LRAD-KVPB-SEP           PIC Z(5)9.9.                            
027501     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027601     03   LRAD-BEFT               PIC Z(2).                               
027701     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027801     03   LRAD-IDDISTR            PIC Z(5).                               
027901     03   FILLER                  PIC X(3)  VALUE SPACE.                  
028001     03   LRAD-IDKUNDNR           PIC Z(7).                               
028101     03   FILLER                  PIC X(3)  VALUE SPACE.                  
028201     03   LRAD-IDRAPPNR           PIC Z(9).                               
028301     03   FILLER                  PIC X(31) VALUE SPACE.                  
028401                                                                          
028501 LINKAGE SECTION.                                                         
028601                                                                          
028701*01  -COPY W0009   -PRE MSG-                                              
028801     EJECT                                                                
028901*01  -COPY W0009   -PRE ALT-                                              
029001     EJECT                                                                
029101*01  -COPY W0008  -PRE LISB-                                              
029201     05  FILLER                  PIC X.                                   
029301     EJECT                                                                
029401*01  -COPY W0008  -PRE KREJ-                                              
029501     05  FILLER                  PIC X.                                   
029601     EJECT                                                                
029701*01  -COPY W0008  -PRE KREE-                                              
029801     05  FILLER                  PIC X.                                   
029901     EJECT                                                                
030001*01  -COPY W0008  -PRE ARTC-                                              
030101     05  FILLER                  PIC X.                                   
030201     EJECT                                                                
030301*01  -COPY W0008  -PRE ARTS-                                              
030401     05  FILLER                  PIC X.                                   
030501     EJECT                                                                
030601*01  -COPY W0008  -PRE BENA-                                              
030701     05  FILLER                  PIC X.                                   
030801     EJECT                                                                
030901 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB LISB-PCB KREJ-PCB              
031001                                   KREE-PCB                               
031101                                   ARTC-PCB ARTS-PCB BENA-PCB.            
031201     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB KREJ-PCB              
031301                                   KREE-PCB                               
031401                                   ARTC-PCB ARTS-PCB BENA-PCB.            
031501                                                                          
031601     PERFORM IMS-GET-MSG                                                  
031701     IF SEGMENT-FINNS                                                     
031801       PERFORM A-INIT                                                     
031901       PERFORM B-BEHANDLA-UTSKRIFT                                        
032001     END-IF                                                               
032101                                                                          
032201     PERFORM Z-FINIT                                                      
032301     MOVE ZERO TO RETURN-CODE                                             
032401     GOBACK                                                               
032501     .                                                                    
032601     EJECT                                                                
032701 A-INIT SECTION.                                                          
032801                                                                          
032901     IF MSG-DUBBLA-TRANSKODER                                             
033001       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I79501                 
033101       IF MSG-KDMFSFOR-2 = 2                                              
033201         MOVE 'GB '        TO W-IDSKYLT                                   
033301       END-IF                                                             
033401     ELSE                                                                 
033501       IF MSG-KDMFSFOR-1 = 2                                              
033601         MOVE 'GB '        TO W-IDSKYLT                                   
033701       END-IF                                                             
033801       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I79501                 
033901     END-IF                                                               
034001                                                                          
034101     PERFORM AA-OPEN-PRINTER                                              
034201     .                                                                    
034301     EJECT                                                                
034401 AA-OPEN-PRINTER         SECTION.                                         
034501                                                                          
034601     MOVE MID-IDPRTLST        TO WS-RAPP-PRINTER                          
034701     IF INDX = ZERO                                                       
034801       MOVE +1 TO INDX                                                    
034901     END-IF                                                               
035000     MOVE MID-IDILIST (INDX)  TO WS-RAPP-IDILIST                          
035100     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
035200                         PRT-OPEN                                         
035300                         WS-RAPP-PRINTER                                  
035400                         ALT-PCB                                          
035500                         LISB-PCB                                         
035600                         WS-RAPP-LISTID                                   
035700                         WS-DUMMY                                         
035800                         WS-DUMMY                                         
035900     .                                                                    
036000     EJECT                                                                
036100 B-BEHANDLA-UTSKRIFT    SECTION.                                          
036200                                                                          
036300     MOVE +1                   TO INDX                                    
036400     PERFORM UNTIL INDX        >  MID-KVPOST                              
036500         MOVE ZERO             TO W-IDSIDNR                               
036600         PERFORM BA-SKAPA-ILISTA                                          
036700         ADD +1                TO INDX                                    
036800     END-PERFORM                                                          
036900     .                                                                    
037000     EJECT                                                                
037100 BA-SKAPA-ILISTA    SECTION.                                              
037200                                                                          
037300     MOVE ZERO                 TO W-IDSIDNR                               
037400                                                                          
037500     MOVE 99                   TO W-KVRADER                               
037600                                                                          
037700     MOVE LOW-VALUE            TO W-WDA2E1KY-MIN-X                        
037800     MOVE HIGH-VALUE           TO W-WDA2E1KY-MAX-X                        
037900     MOVE MID-IDDC             TO W-A2E1KY-IDDC-MIN                       
038000                                  W-A2E1KY-IDDC-MAX                       
038100     MOVE MID-IDILIST (INDX)   TO W-A2E1KY-IDILIST-MIN                    
038200                                  W-A2E1KY-IDILIST-MAX                    
038300     MOVE MID-IDDC             TO WS-IDDC                                 
038310                                  W-IDDC                                  
038400     PERFORM IMS-GU-KREJ-KREJ01                                           
038500                                                                          
038600     IF SEGMENT-FINNS                                                     
038700        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
038800            MOVE SEQE-IDDISTR            TO W-IDDISTR                     
038900            MOVE SEQE-IDKUNDNR           TO W-IDKUNDNR                    
039000            MOVE SEQE-IDRAPPNR           TO W-IDRAPPNR                    
039100            MOVE SEQE-IDARTNR            TO W-IDARTNR                     
039200            MOVE SEQE-IDRADNR            TO W-IDRADNR                     
039300                                                                          
039400            PERFORM IMS-GU-KREE-KREE11                                    
039500            PERFORM BAA-SKAPA-RAD                                         
039600            PERFORM IMS-GN-KREJ-KREJ01                                    
039700        END-PERFORM                                                       
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 BAA-SKAPA-RAD       SECTION.                                             
040200                                                                          
041500     MOVE LEV-IDARTNR          TO LRAD-IDARTNR                            
041600     MOVE W-IDDISTR            TO LRAD-IDDISTR                            
041700     MOVE W-IDKUNDNR           TO LRAD-IDKUNDNR                           
041800     MOVE W-IDRAPPNR           TO LRAD-IDRAPPNR                           
041900*                                                                         
042000     MOVE LEV-KVANTAL-ILI      TO LRAD-KVLEVANM-KVAR                      
042100                                                                          
042200     PERFORM BAB-HAMTA-BENAMNING                                          
042300                                                                          
042401     PERFORM BAD-HAMTA-PB-O-ADRESS                                        
042500                                                                          
042600     IF W-KVRADER              >  MAX-KVRADER                             
042700         PERFORM S10-SKAPA-HUVUD                                          
042800         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
042900         ADD +1                TO W-KVRADER                               
043000      ELSE                                                                
043100         MOVE PRT-AFTER-2      TO PRT-RADSKIP                             
043200         ADD +2                TO W-KVRADER                               
043300     END-IF                                                               
043400     MOVE LIST-LRAD            TO WS-RAPP-RAD                             
043500     PERFORM S01-SKRIV-RAD                                                
043600                                                                          
043700     .                                                                    
043800     EJECT                                                                
043900 BAB-HAMTA-BENAMNING       SECTION.                                       
044000     PERFORM IMS-GU-WLBENA11                                              
044100     IF SEGMENT-FINNS                                                     
044200       MOVE TEXT-BEART          TO LRAD-BEART                             
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044601 BAD-HAMTA-PB-O-ADRESS     SECTION.                                       
044700                                                                          
044801     IF CDC-SE                                                            
044901       PERFORM IMS-GU-WLARTC11                                            
045001       IF SEGMENT-FINNS                                                   
045100        MOVE ARTC-CLAG-KVPB-SEP   TO LRAD-KVPB-SEP                        
045200        MOVE ARTC-CLAG-BEFT       TO LRAD-BEFT                            
045301        MOVE ARTC-CLAG-ADLAGOMR   TO LRAD-ADLAGOMR                        
045401        MOVE ARTC-CLAG-ADGANG     TO LRAD-ADGANG                          
045501        MOVE ARTC-CLAG-ADPLATS    TO LRAD-ADPLATS                         
045601       ELSE                                                               
045700        MOVE ZERO                 TO LRAD-KVPB-SEP                        
045800                                     LRAD-BEFT                            
045901                                     LRAD-ADLAGOMR                        
046001                                     LRAD-ADGANG                          
046101                                     LRAD-ADPLATS                         
046201       END-IF                                                             
046301     ELSE                                                                 
046401       MOVE ZERO                  TO LRAD-KVPB-SEP                        
046501                                     LRAD-BEFT                            
046601       PERFORM IMS-GU-WLARTS11                                            
046701       IF SEGMENT-FINNS                                                   
046801         IF NDC-US                                                        
046901           MOVE ARTS-SLAG-ADLAGOMR TO LRAD-NA-ADLAGOMR                    
047001           MOVE ARTS-SLAG-ADGANG   TO LRAD-NA-ADGANG                      
047101           MOVE ARTS-SLAG-ADPLATS  TO LRAD-NA-ADPLATS                     
047201         ELSE                                                             
047301           MOVE ARTS-SLAG-ADLAGOMR TO LRAD-ADLAGOMR                       
047401           MOVE ARTS-SLAG-ADGANG   TO LRAD-ADGANG                         
047501           MOVE ARTS-SLAG-ADPLATS  TO LRAD-ADPLATS                        
047601         END-IF                                                           
047701       ELSE                                                               
047801         IF NDC-US                                                        
047901           MOVE ZERO            TO LRAD-NA-ADLAGOMR                       
048001                                   LRAD-NA-ADGANG                         
048101                                   LRAD-NA-ADPLATS                        
048201         ELSE                                                             
048301           MOVE ZERO            TO LRAD-ADLAGOMR                          
048401                                   LRAD-ADGANG                            
048501                                   LRAD-ADPLATS                           
048601         END-IF                                                           
048701       END-IF                                                             
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 Z-FINIT                   SECTION.                                       
049200                                                                          
049300     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
049400                         PRT-CLOSE                                        
049500                         WS-RAPP-PRINTER                                  
049600                         ALT-PCB                                          
049700                         LISB-PCB                                         
049800                         WS-RAPP-LISTID                                   
049900                         WS-DUMMY                                         
050000                         WS-DUMMY                                         
050100     .                                                                    
050200     EJECT                                                                
050300 S01-SKRIV-RAD SECTION.                                                   
050400                                                                          
050500     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
050600                         PRT-WRITE                                        
050700                         WS-RAPP-PRINTER                                  
050800                         ALT-PCB                                          
050900                         LISB-PCB                                         
051000                         WS-RAPP-LISTID                                   
051100                         PRT-RADSKIP                                      
051200                         WS-RAPP-LISTRAD                                  
051300                                                                          
051400     MOVE SPACE                TO WS-RAPP-LISTRAD                         
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 S10-SKAPA-HUVUD     SECTION.                                             
051900                                                                          
052000     MOVE MID-IDILIST (INDX)      TO HRAD1-IDILIST                        
052100                                     HRAD1-IDILIST-ENG                    
052200     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
052300     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
052400                                     HRAD1-IDSIDNR-ENG                    
052500     ACCEPT HRAD1-DATUM           FROM DATE                               
052600     ACCEPT HRAD1-DATUM-ENG       FROM DATE                               
052700                                                                          
052800     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
052900     IF W-IDSKYLT = 'GB '                                                 
053000       MOVE LIST-HRAD1-ENG          TO WS-RAPP-RAD                        
053100     ELSE                                                                 
053200       MOVE LIST-HRAD1              TO WS-RAPP-RAD                        
053300     END-IF                                                               
053400     PERFORM S01-SKRIV-RAD                                                
053500                                                                          
053600     MOVE PRT-AFTER-4             TO PRT-RADSKIP                          
053700     IF W-IDSKYLT = 'GB '                                                 
053800       MOVE LIST-HRUB3-ENG          TO WS-RAPP-RAD                        
053900     ELSE                                                                 
054000       MOVE LIST-HRUB3              TO WS-RAPP-RAD                        
054100     END-IF                                                               
054200     PERFORM S01-SKRIV-RAD                                                
054300                                                                          
054400     MOVE +9                      TO W-KVRADER                            
054500     .                                                                    
054600     EJECT                                                                
054700* --- IMS SEKTIONER ---                                                   
054800     SKIP3                                                                
054900 IMS-GET-MSG SECTION.                                                     
055000                                                                          
055100     MOVE '  QC' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
055300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-GU-KREJ-KREJ01 SECTION.                                              
055800     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
055900                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
056000          DELIMITED BY SIZE INTO SSA1                                     
056100     MOVE '  GE' TO GODK-STATUSKODER                                      
056200     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA1 SSA1                     
056300     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     SKIP2                                                                
056700 IMS-GN-KREJ-KREJ01 SECTION.                                              
056800     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
056900                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
057000          DELIMITED BY SIZE INTO SSA1                                     
057100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
057200     CALL CBLTDLI USING GN KREJ-PCB DLI-IO-AREA1 SSA1                     
057300     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
057400     PERFORM IMS-STATUSKONTROLL                                           
057500     .                                                                    
057600     EJECT                                                                
057700 IMS-GU-KREE-KREE11 SECTION.                                              
057800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
057900          DELIMITED BY SIZE INTO SSA1                                     
058000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
058100          DELIMITED BY SIZE INTO SSA2                                     
058200     MOVE '  ' TO GODK-STATUSKODER                                        
058300     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA1 SSA1 SSA2                
058400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
058500     PERFORM IMS-STATUSKONTROLL                                           
058600     .                                                                    
058700     EJECT                                                                
058800 IMS-GU-WLARTC11    SECTION.                                              
058900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
059000          DELIMITED BY SIZE INTO SSA1                                     
059100     MOVE 'WLARTC11 ' TO SSA2                                             
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
059400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     SKIP3                                                                
059801 IMS-GU-WLARTS11    SECTION.                                              
059901     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
060001          DELIMITED BY SIZE INTO SSA1                                     
060104     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
060105          DELIMITED BY SIZE INTO SSA2                                     
060201     MOVE '  GE' TO GODK-STATUSKODER                                      
060301     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA1 SSA1 SSA2                
060401     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
060501     PERFORM IMS-STATUSKONTROLL                                           
060601     .                                                                    
061701     SKIP3                                                                
061801 IMS-GU-WLBENA11    SECTION.                                              
061901     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
062001          DELIMITED BY SIZE INTO SSA1                                     
062101     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
062201          DELIMITED BY SIZE INTO SSA2                                     
062301     MOVE '  GE' TO GODK-STATUSKODER                                      
062401     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA1 SSA1 SSA2                
062501     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
062601     PERFORM IMS-STATUSKONTROLL                                           
062701     .                                                                    
062801     SKIP3                                                                
062901 IMS-STATUSKONTROLL SECTION.                                              
063001                                                                          
063101     SET STATUS-IX TO 1                                                   
063201     SEARCH GODK-STATUS                                                   
063301       AT END                                                             
063401         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063501         DELIMITED BY SIZE INTO FELTEXT                                   
063601         CALL FELLOG                                                      
063701       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063801         CONTINUE                                                         
063901     END-SEARCH                                                           
064001     .                                                                    
