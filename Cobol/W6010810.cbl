000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010810.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/02/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM VISAR ANGIVEN ARTIKELS                  
001200*        FÖRBEHANDLINGS- OCH/ELLER FÖRPACKNINGSGRUPP.                     
001300*        OCH OLIKA ANSVARIGA FÖR ARTIKELN                                 
001400*                                                                         
001500*        THE PROGRAM READS     W6HANA (W6G1)                              
001600*        THE PROGRAM READS     W6PLAA (W6G1)                              
001700*        THE PROGRAM READS     WLARTC (WDK6)                              
001800*        THE PROGRAM READS     WLARTC (WDK7)                              
001900*        THE PROGRAM READS     WDP3                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        REQU:        W60108I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESP:        W60108O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6010800'.            
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003700 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003800                                                                          
003900 01  ALL-SPACE.                                                           
004000     03 FILLER                   PIC X(50)   VALUE SPACE.                 
004100                                                                          
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
004500                                                                          
004600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004700                                                                          
004800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004900 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005000 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
005100 77  SPAR-IDFKNGRP               PIC S9(5)   VALUE ZERO COMP-3.           
005200 77  WS-IDPERSON                 PIC S9(3)   VALUE +0 COMP-3.             
005300 01  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
005400                                                                          
005500 01  SPAR-IDBERED                PIC S9(3)   VALUE +0 COMP-3.             
005600 01  SPAR-IDANSK                 PIC S9(3)   VALUE +0 COMP-3.             
005700 01  SPAR-IDINK                  PIC S9(3)   VALUE +0 COMP-3.             
005800 01  SPAR-IDPERSON-ANSV          PIC S9(3)   VALUE +0 COMP-3.             
005900 01  SPAR-ADLAGOMR               PIC 9(2)    VALUE ZERO.                  
006000*      --- VALID IDDC CODES                                               
006100*                                                                         
006200*01    -COPY WWDCKONS                                                     
006210*01    -COPY WWLNDKON                                                     
006300*01    -COPY WWDC99                                                       
006400*01    -COPY WWDC99 -PRE STYR-                                            
006500       EJECT                                                              
006600                                                                          
006700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006800     88  KEYS-OK                             VALUE 'Y'.                   
006900     88  KEYS-WRONG                          VALUE 'N'.                   
007000                                                                          
007100 77  ARTNR-SW                    PIC X       VALUE 'N'.                   
007200     88  ARTNR-NY                            VALUE 'Y'.                   
007300                                                                          
007400 77  LEVNR-SW                    PIC X       VALUE 'N'.                   
007500     88  LEVNR-NY                            VALUE 'Y'.                   
007600                                                                          
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERAL-SUBPROGRAM.                                                  
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
008200     EJECT                                                                
008300                                                                          
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008600     03  ERR-ART-MISSING         PIC X(3)    VALUE '025'.                 
008700     03  ERR-OMR-MISSING         PIC X(3)    VALUE '025'.                 
008800     EJECT                                                                
008900*01  -COPY W611STYR                                                       
009000     EJECT                                                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009300     SKIP3                                                                
009400*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
009700*                                                                         
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  KEYS-TO-DLI.                                                         
010200*    --- D601                                                             
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500*    --- D661                                                             
010600     03  W-KDCLAGER-X.                                                    
010700         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
010800     03  W-KDARBTYP-X.                                                    
010900         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
011000     03  W-W6GXKEY-X.                                                     
011100         05  FILLER              PIC X(4)    VALUE '6005'.                
011200         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
011300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
011400     03  W-W6GXKEY-DC11-X.                                                
011500         05  FILLER              PIC X(4)    VALUE '6005'.                
011600         05  FILLER              PIC X(2)    VALUE '11'.                  
011700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
011800     03  W-ADINLOMR-X.                                                    
011900         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
012000     03  W-IDPERSON-X.                                                    
012100         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
012200     03  W-IDDC-X.                                                        
012300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012310     03  W-IDLAND-X.                                                      
012320         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
012400     03  W-WDP3A1-MIN.                                                    
012500       05  W-IDLAND-A-MIN        PIC X(2)    VALUE SPACE.                 
012510       05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
012600       05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
012700       05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.                 
012800     03  W-WDP3A1-MAX.                                                    
012810       05  W-IDLAND-A-MAX        PIC X(2)    VALUE SPACE.                 
012900       05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
013000       05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
013100       05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.                 
013200     03  W-WDP3B1-X.                                                      
013210         05  W-IDLAND-B          PIC X(2)    VALUE SPACE.                 
013300         05  W-IDLEVNR-B         PIC X(5)    VALUE SPACE.                 
013400         05  W-KDARBTYP-B        PIC X(8)    VALUE SPACE.                 
013500     03  W-WDP3C1-MIN.                                                    
013510       05  W-IDLAND-C-MIN        PIC X(2)    VALUE SPACE.                 
013600       05  W-IDFKNGRPF-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
013700       05  W-IDFKNGRPT-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
013800       05  W-KDARBTYP-C-MIN      PIC X(8)    VALUE SPACE.                 
013900     03  W-WDP3C1-MAX.                                                    
013910       05  W-IDLAND-C-MAX        PIC X(2)    VALUE SPACE.                 
014000       05  W-IDFKNGRPF-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
014100       05  W-IDFKNGRPT-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
014200       05  W-KDARBTYP-C-MAX      PIC X(8)    VALUE SPACE.                 
014300     SKIP2                                                                
014400*                                                                         
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FOUND                       VALUE '  '.                  
014800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014900     88  SEGMENT-MISSING                     VALUE 'GE'                   
015000                                                   'GB'.                  
015100     SKIP2                                                                
015200 01  GOOD-STATUSCODES.                                                    
015300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500*                                                                         
015600 01  SSA1                        PIC X(128).                              
015700 01  SSA2                        PIC X(64).                               
015800 01  SSA3                        PIC X(64).                               
015900     EJECT                                                                
016000*                                                                         
016100*    --- IMS FUNCTION CODES                                               
016200*01  -COPY W0003                                                          
016300     EJECT                                                                
016400*                                                                         
016500*    ---  DLI INPUT-OUTPUT AREA                                           
016600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016700     SKIP3                                                                
016800*                                                                         
016900 01  DLI-IO-AREA-PLAA.                                                    
017000     03  IO-AREA-PLAA            PIC X(150)  VALUE SPACE.                 
017100     SKIP3                                                                
017200     03  W6PLAA11 REDEFINES IO-AREA-PLAA.                                 
017300*        05  -COPY W6GX6006  -PRE PLAA-                                   
017400     SKIP3                                                                
017500 01  DLI-IO-AREA.                                                         
017600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
017700     SKIP3                                                                
017800     03  WLARTC01 REDEFINES IO-AREA.                                      
017900*        05  -COPY WDK601  -PRE K601-                                     
018000     SKIP3                                                                
018100     03  WLARTC11 REDEFINES IO-AREA.                                      
018200*        05  -COPY WDK611  -PRE K611-                                     
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
018600 01  DLI-IO-AREA-WDK711.                                                  
018700*    03  -COPY WDK711 -PRE WDK7-                                          
018800     EJECT                                                                
018801 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
018802 01  DLI-IO-AREA-WDK712.                                                  
018803*    03  -COPY WDK712                                                     
018804     EJECT                                                                
018810 01  DLI-IO-AREA-WDK722.                                                  
018820*    03  -COPY WDK722 -PRE K722-                                          
018830     EJECT                                                                
018900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-P311'.          
019000*                                                                         
019100 01  DLI-IO-AREA-P311.                                                    
019200*    03  -COPY WDP311                                                     
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3A'.         
019500 01  DLI-IO-AREA-WDP3A.                                                   
019600*    03  -COPY WDP3A1                                                     
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3B'.         
019900 01  DLI-IO-AREA-WDP3B.                                                   
020000*    03  -COPY WDP3B1                                                     
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP3C'.         
020300 01  DLI-IO-AREA-WDP3C.                                                   
020400*    03  -COPY WDP3C1                                                     
020500     EJECT                                                                
020600 LINKAGE SECTION.                                                         
020700                                                                          
020800 01  REQU-AREA.                                                           
020900*    03 -COPY WZ01REQU                                                    
021000*    03 -COPY W60108I1                                                    
021100     EJECT                                                                
021200 01  RESP-AREA.                                                           
021300*    03 -COPY WZ01RESP                                                    
021400*    03 -COPY W60108O1                                                    
021500     EJECT                                                                
021600                                                                          
021700*01  -COPY W0008  -PRE HANA-                                              
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008  -PRE PLAA-                                              
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008  -PRE WDP3-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008  -PRE ARTC-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE WDK7-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDP3A-                                             
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDP3B-                                             
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE WDP3C-                                             
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
024200                           HANA-PCB  PLAA-PCB  WDP3-PCB  ARTC-PCB         
024300                           WDK7-PCB  WDP3A-PCB WDP3B-PCB WDP3C-PCB        
024400                           .                                              
024500     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA                            
024600                           HANA-PCB  PLAA-PCB  WDP3-PCB  ARTC-PCB         
024700                           WDK7-PCB  WDP3A-PCB WDP3B-PCB WDP3C-PCB        
024800                           .                                              
024900                                                                          
025000     PERFORM A-INIT                                                       
025100     PERFORM B-CHECK-KEYS                                                 
025200     IF KEYS-OK                                                           
025300       IF W-IDARTNR > +0                                                  
025400         PERFORM FA-READ-BASICDATA-IDARTNR                                
025500       ELSE                                                               
025600         PERFORM FB-READ-BASICDATA-IDLEVNR                                
025700       END-IF                                                             
025800       IF KEYS-OK                                                         
025900         PERFORM FC-READ-NAME                                             
026000       END-IF                                                             
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO                   TO RETURN-CODE                           
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     MOVE ALL '+'                TO RESP-W60108O1                         
027000     MOVE 001                    TO RESP-IDMSGVER                         
027100     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
027200                                    RESP-IDMSG-INFO                       
027300                                    RESP-IDELMT-ERROR                     
027400                                                                          
027500     MOVE LOW-VALUE              TO W-WDP3A1-MIN                          
027600                                    W-WDP3C1-MIN                          
027700     MOVE HIGH-VALUE             TO W-WDP3A1-MAX                          
027800                                    W-WDP3C1-MAX                          
027900     .                                                                    
028000     EJECT                                                                
028100*----------------------------------------------------------------*        
028200 B-CHECK-KEYS SECTION.                                                    
028300                                                                          
028400     MOVE YES                    TO KEYS-SW                               
028500                                                                          
028600     MOVE REQU-IDARTNR-KEY       TO WS-IDARTNR                            
028700                                                                          
028800     IF WS-IDARTNR = ALL '+'                                              
028900       CONTINUE                                                           
029000     ELSE                                                                 
029100       MOVE YES                  TO ARTNR-SW                              
029200       IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                        
029300         MOVE WS-IDARTNR         TO W-IDARTNR                             
029400       END-IF                                                             
029500     END-IF                                                               
029600                                                                          
029700     MOVE REQU-IDLEVNR-KEY       TO WS-IDLEVNR                            
029800                                                                          
029900     IF WS-IDLEVNR = ALL '+'                                              
030000       CONTINUE                                                           
030100     ELSE                                                                 
030200       MOVE YES                  TO LEVNR-SW                              
030300     END-IF                                                               
030400                                                                          
030500     IF LEVNR-NY                                                          
030600       MOVE WS-IDLEVNR           TO W-IDLEVNR                             
030700       IF ARTNR-NY                                                        
030800         IF WS-IDARTNR NUMERIC AND WS-IDARTNR > 0                         
030900           MOVE SPACE            TO W-IDLEVNR                             
031000                                    WS-IDLEVNR                            
031100         ELSE                                                             
031200           MOVE ZERO             TO W-IDARTNR                             
031300                                    WS-IDARTNR                            
031400         END-IF                                                           
031500       ELSE                                                               
031600         MOVE ZERO               TO W-IDARTNR                             
031700                                    WS-IDARTNR                            
031800       END-IF                                                             
031900     ELSE                                                                 
032000       IF ARTNR-NY                                                        
032100         MOVE SPACE              TO W-IDLEVNR                             
032200                                    WS-IDLEVNR                            
032300       ELSE                                                               
032400         MOVE ZERO               TO W-IDARTNR                             
032500                                    WS-IDARTNR                            
032600       END-IF                                                             
032700     END-IF                                                               
032800                                                                          
032900     IF W-IDARTNR = +0 AND W-IDLEVNR = SPACE                              
033000       MOVE NEJ                  TO KEYS-SW                               
033100     END-IF                                                               
033200                                                                          
033300     MOVE REQU-IDDC-KEY          TO WS-IDDC                               
033400     IF GOOD-DC                                                           
033500       IF SDC                                                             
033600         MOVE WC-CDC-SE          TO WS-IDDC                               
033700       END-IF                                                             
033800       MOVE WS-IDDC              TO W-6005-IDDC                           
033900                                    W-IDDC                                
034000     ELSE                                                                 
034100       MOVE NEJ                  TO KEYS-SW                               
034200     END-IF                                                               
034300                                                                          
034301     IF NDC-CN                                                            
034302        MOVE WC-LAND-CN   TO W-IDLAND                                     
034303     ELSE                                                                 
034304        IF NDC-US                                                         
034305           MOVE WC-LAND-US   TO W-IDLAND                                  
034306        ELSE                                                              
034307           MOVE WC-LAND-SE   TO W-IDLAND                                  
034308        END-IF                                                            
034309     END-IF                                                               
034310                                                                          
034311     MOVE W-IDLAND  TO W-IDLAND-A-MIN                                     
034312                       W-IDLAND-A-MAX                                     
034313                       W-IDLAND-B                                         
034314                       W-IDLAND-C-MIN                                     
034315                       W-IDLAND-C-MAX                                     
034340                                                                          
034400     IF CDC OR NDC                                                        
034500       CONTINUE                                                           
034600     ELSE                                                                 
034700       MOVE NEJ                  TO KEYS-SW                               
034800     END-IF                                                               
034900                                                                          
035000     MOVE WS-IDARTNR             TO RESP-IDARTNR                          
035100     INSPECT RESP-IDARTNR REPLACING LEADING ZERO BY SPACE                 
035200     MOVE WS-IDLEVNR             TO RESP-IDLEVNR                          
035300     MOVE WS-IDDC                TO RESP-IDDC                             
035400     INSPECT RESP-IDDC    REPLACING LEADING ZERO BY SPACE                 
035500                                                                          
035600     IF KEYS-WRONG                                                        
035700       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
035800       PERFORM MFS-ERASE-FIELD-OUT                                        
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200*----------------------------------------------------------------*        
036300 FA-READ-BASICDATA-IDARTNR SECTION.                                       
036400                                                                          
036500     MOVE +1                     TO W-KDCLAGER                            
036600     MOVE ZERO                   TO STYR-IDARTNR                          
036700                                    STYR-IDFKNGRP                         
036800                                    STYR-BEFT                             
036900     MOVE W-6005-IDDC            TO STYR-IDDC                             
037000                                                                          
037100     PERFORM IMS-GU-ARTC-K601                                             
037200     IF NDC                                                               
037300       PERFORM IMS-GU-WDK711                                              
037400     END-IF                                                               
037500                                                                          
037600     IF SEGMENT-FOUND                                                     
037700       MOVE K601-ART-IDARTNR     TO STYR-IDARTNR                          
037800       MOVE K601-ART-IDFKNGRP    TO STYR-IDFKNGRP                         
037900                                    SPAR-IDFKNGRP                         
038000       MOVE K601-ART-IDLEVNR     TO STYR-IDLEVNR                          
038100                                    SPAR-IDLEVNR                          
038200                                                                          
038300       PERFORM IMS-GNP-ARTC-K611                                          
038400                                                                          
038500       IF SEGMENT-FOUND                                                   
038600         MOVE K611-CLAG-IDBERED  TO SPAR-IDBERED                          
041200         IF W-6005-IDDC = ZERO                                            
041210           MOVE K611-CLAG-BEFT   TO STYR-BEFT                             
041300           MOVE WS-IDDC          TO W-6005-IDDC                           
041400                                    RESP-IDDC                             
041500         END-IF                                                           
041600                                                                          
041601         IF NDC-CN OR NDC-US                                              
041610           PERFORM IMS-GNP-WDK722                                         
041620           IF SEGMENT-FOUND                                               
041630             MOVE K722-XLAG-IDANSK   TO SPAR-IDANSK                       
041640**                                                                        
041650             IF K722-XLAG-IDINK (1:3) NUMERIC                             
041660               MOVE K722-XLAG-IDINK (1:3)                                 
041670                                     TO SPAR-IDINK                        
041680             ELSE                                                         
041690               IF K722-XLAG-IDINK (2:3) NUMERIC                           
041691                 MOVE K722-XLAG-IDINK (2:3)                               
041692                                     TO SPAR-IDINK                        
041693               ELSE                                                       
041694                 IF K722-XLAG-IDINK (1:2) NUMERIC                         
041695                   MOVE K722-XLAG-IDINK (1:2)                             
041696                                     TO SPAR-IDINK                        
041697                 ELSE                                                     
041698                   IF K722-XLAG-IDINK (1:1) NUMERIC                       
041699                     MOVE K722-XLAG-IDINK (1:1)                           
041700                                     TO SPAR-IDINK                        
041701                   ELSE                                                   
041702                     MOVE ZERO       TO SPAR-IDINK                        
041703                   END-IF                                                 
041704                 END-IF                                                   
041705               END-IF                                                     
041706             END-IF                                                       
041707           END-IF                                                         
041708*                                                                         
041709           PERFORM IMS-GU-WDK712                                          
041710           IF SEGMENT-FOUND                                               
041711             IF LART-BEFT > 0                                             
041712               MOVE LART-BEFT    TO STYR-BEFT                             
041713             END-IF                                                       
041714           END-IF                                                         
041715         END-IF                                                           
041716                                                                          
041717         IF SPAR-IDANSK NOT > 0                                           
041718           MOVE K611-CLAG-IDANSK   TO SPAR-IDANSK                         
041720         END-IF                                                           
042020**                                                                        
042021         IF SPAR-IDINK NOT > 0                                            
042030           IF K611-CLAG-IDINK (1:3) NUMERIC                               
042040             MOVE K611-CLAG-IDINK (1:3)                                   
042050                                   TO SPAR-IDINK                          
042060           ELSE                                                           
042070             IF K611-CLAG-IDINK (2:3) NUMERIC                             
042080               MOVE K611-CLAG-IDINK (2:3)                                 
042090                                   TO SPAR-IDINK                          
042091             ELSE                                                         
042092               IF K611-CLAG-IDINK (1:2) NUMERIC                           
042093                 MOVE K611-CLAG-IDINK (1:2)                               
042094                                   TO SPAR-IDINK                          
042095               ELSE                                                       
042096                 IF K611-CLAG-IDINK (1:1) NUMERIC                         
042097                   MOVE K611-CLAG-IDINK (1:1)                             
042098                                   TO SPAR-IDINK                          
042099                 ELSE                                                     
042100                   MOVE ZERO       TO SPAR-IDINK                          
042101                 END-IF                                                   
042102               END-IF                                                     
042103             END-IF                                                       
042104           END-IF                                                         
042105         END-IF                                                           
042106**                                                                        
042107         IF CDC                                                           
042108           MOVE K611-CLAG-ADLAGOMR                                        
042109                                 TO RESP-ADLAGOMR                         
042110                                    SPAR-ADLAGOMR                         
042120         ELSE                                                             
042200           MOVE WDK7-SLAG-ADLAGOMR                                        
042300                                 TO RESP-ADLAGOMR                         
042400                                    SPAR-ADLAGOMR                         
042520         END-IF                                                           
042530                                                                          
042600       END-IF                                                             
042610                                                                          
042700       IF W-IDLEVNR NOT = SPACE                                           
042800         MOVE W-IDLEVNR          TO STYR-IDLEVNR                          
042900       END-IF                                                             
043000                                                                          
043100       MOVE W-6005-IDDC          TO STYR-IDDC                             
043200*-------CALL TILL SUBPROGRAM                                              
043300       CALL W611STYR          USING STYR-W611STYR                         
043400                                    HANA-PCB PLAA-PCB                     
043500                                                                          
043600       IF STYR-KDSVAR-FEL                                                 
043700         MOVE ERR-OMR-MISSING    TO RESP-IDMSG-ERROR                      
043800         MOVE 'ADPLATS'          TO RESP-IDELMT-ERROR                     
043900         PERFORM MFS-ERASE-FIELD-OUT                                      
044000         MOVE NEJ                TO KEYS-SW                               
044100       ELSE                                                               
044200         MOVE STYR-ADINLOMR-FB   TO RESP-ADINLOMR-FB                      
044300         MOVE STYR-ADINLOMR-FP   TO RESP-ADINLOMR-FP                      
044400         MOVE W-6005-IDDC        TO STYR-WS-IDDC                          
044500         IF (STYR-ADINLOMR-FB = 'INS?' OR 'FB? ') AND                     
044600            (STYR-CDC)                                                    
044700           IF W-IDLEVNR NOT = SPACE                                       
044800             MOVE W-IDLEVNR      TO STYR-IDLEVNR                          
044900           END-IF                                                         
045000                                                                          
045100           IF STYR-CDC-SE                                                 
045200             MOVE WC-CDC-TR      TO STYR-IDDC                             
045300           ELSE                                                           
045400             MOVE WC-CDC-SE      TO STYR-IDDC                             
045500           END-IF                                                         
045600*-------CALL TILL SUBPROGRAM                                              
045700           CALL W611STYR      USING STYR-W611STYR                         
045800                                    HANA-PCB PLAA-PCB                     
045900           IF STYR-KDSVAR-FEL                                             
046000             MOVE ERR-OMR-MISSING                                         
046100                                 TO RESP-IDMSG-ERROR                      
046200             MOVE 'ADPLATS'      TO RESP-IDELMT-ERROR                     
046300             PERFORM MFS-ERASE-FIELD-OUT                                  
046400             MOVE NEJ            TO KEYS-SW                               
046500           ELSE                                                           
046600             IF STYR-ADINLOMR-FB = 'INS?' OR 'FB? '                       
046700               MOVE W-6005-IDDC  TO STYR-IDDC                             
046800               MOVE RESP-ADINLOMR-FB                                      
046900                                 TO STYR-ADINLOMR-FB                      
047000             ELSE                                                         
047100               MOVE STYR-ADINLOMR-FB                                      
047200                                 TO RESP-ADINLOMR-FB                      
047300               MOVE STYR-IDDC    TO W-6005-IDDC                           
047400             END-IF                                                       
047500           END-IF                                                         
047600         END-IF                                                           
047700       END-IF                                                             
047800     ELSE                                                                 
047900       IF SEGMENT-MISSING                                                 
048000         MOVE ERR-ART-MISSING    TO RESP-IDMSG-ERROR                      
048100         MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                     
048200         PERFORM MFS-ERASE-FIELD-OUT                                      
048300         MOVE NEJ                TO KEYS-SW                               
048400       END-IF                                                             
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 FB-READ-BASICDATA-IDLEVNR SECTION.                                       
048900                                                                          
049000     MOVE ZERO                   TO STYR-IDARTNR                          
049100                                    STYR-IDFKNGRP                         
049200                                    STYR-BEFT                             
049300     MOVE W-IDLEVNR              TO STYR-IDLEVNR                          
049400     MOVE W-6005-IDDC            TO STYR-IDDC                             
049500                                                                          
049600*----CALL TILL SUBPROGRAM                                                 
049700     CALL W611STYR            USING STYR-W611STYR                         
049800                                    HANA-PCB PLAA-PCB                     
049900                                                                          
050000     IF STYR-KDSVAR-FEL                                                   
050100       MOVE ERR-OMR-MISSING      TO RESP-IDMSG-ERROR                      
050200       MOVE 'ADPLATS'            TO RESP-IDELMT-ERROR                     
050300       PERFORM MFS-ERASE-FIELD-OUT                                        
050400       MOVE NEJ                  TO KEYS-SW                               
050500     ELSE                                                                 
050600       MOVE STYR-ADINLOMR-FB     TO RESP-ADINLOMR-FB                      
050700       MOVE W-6005-IDDC          TO STYR-WS-IDDC                          
050800       IF (STYR-ADINLOMR-FB = 'FB? ' OR 'INS?') AND                       
050900          (STYR-CDC)                                                      
051000         MOVE ZERO               TO STYR-IDARTNR                          
051100                                    STYR-IDFKNGRP                         
051200                                    STYR-BEFT                             
051300         MOVE W-IDLEVNR          TO STYR-IDLEVNR                          
051400         IF STYR-CDC-SE                                                   
051500           MOVE WC-CDC-TR        TO STYR-IDDC                             
051600         ELSE                                                             
051700           MOVE WC-CDC-SE        TO STYR-IDDC                             
051800         END-IF                                                           
051900                                                                          
052000*----CALL TILL SUBPROGRAM                                                 
052100         CALL W611STYR        USING STYR-W611STYR                         
052200                                    HANA-PCB PLAA-PCB                     
052300                                                                          
052400         IF STYR-KDSVAR-FEL                                               
052500           MOVE ERR-OMR-MISSING  TO RESP-IDMSG-ERROR                      
052600           MOVE 'ADPLATS'        TO RESP-IDELMT-ERROR                     
052700           PERFORM MFS-ERASE-FIELD-OUT                                    
052800           MOVE NEJ              TO KEYS-SW                               
052900         ELSE                                                             
053000           IF STYR-ADINLOMR-FB = 'FB? ' OR 'INS?'                         
053100             MOVE W-6005-IDDC    TO STYR-IDDC                             
053200             MOVE RESP-ADINLOMR-FB                                        
053300                                 TO STYR-ADINLOMR-FB                      
053400           ELSE                                                           
053500             MOVE STYR-ADINLOMR-FB                                        
053600                                 TO RESP-ADINLOMR-FB                      
053700             MOVE STYR-IDDC      TO W-6005-IDDC                           
053800           END-IF                                                         
053900         END-IF                                                           
054000       END-IF                                                             
054100     END-IF                                                               
054200                                                                          
054300     MOVE ALL-SPACE              TO RESP-ADINLOMR-FP                      
054400                                    RESP-ADLAGOMR                         
054500     .                                                                    
054600     EJECT                                                                
054700 FC-READ-NAME SECTION.                                                    
054800                                                                          
054900     IF SPAR-IDANSK > +0                                                  
055000       MOVE 'ANSK    '           TO W-KDARBTYP                            
055100       MOVE SPAR-IDANSK          TO W-IDPERSON                            
055200       PERFORM IMS-GU-P311                                                
055300       IF SEGMENT-FOUND                                                   
055400         MOVE PERS-IDNAMN        TO RESP-IDNAMN-ANSK                      
055500         MOVE PERS-IDTFN         TO RESP-IDTFN-ANSK                       
055600       ELSE                                                               
055700         MOVE ALL-SPACE          TO RESP-IDNAMN-ANSK                      
055800                                    RESP-IDTFN-ANSK                       
055900       END-IF                                                             
056000     ELSE                                                                 
056100       MOVE ALL-SPACE            TO RESP-IDNAMN-ANSK                      
056200                                    RESP-IDTFN-ANSK                       
056300     END-IF                                                               
056400                                                                          
056500     IF SPAR-IDINK  > +0                                                  
056600       MOVE 'INK     '           TO W-KDARBTYP                            
056700       MOVE SPAR-IDINK           TO W-IDPERSON                            
056800       PERFORM IMS-GU-P311                                                
056900       IF SEGMENT-FOUND                                                   
057000         MOVE PERS-IDNAMN        TO RESP-IDNAMN-INK                       
057100         MOVE PERS-IDTFN         TO RESP-IDTFN-INK                        
057200       ELSE                                                               
057300         MOVE ALL-SPACE          TO RESP-IDNAMN-INK                       
057400                                    RESP-IDTFN-INK                        
057500       END-IF                                                             
057600     ELSE                                                                 
057700       MOVE ALL-SPACE            TO RESP-IDNAMN-INK                       
057800                                    RESP-IDTFN-INK                        
057900     END-IF                                                               
058000                                                                          
058100     IF SPAR-IDBERED > +0                                                 
058200       MOVE 'BER     '           TO W-KDARBTYP                            
058300       MOVE SPAR-IDBERED         TO W-IDPERSON                            
058400       PERFORM IMS-GU-P311                                                
058500       IF SEGMENT-FOUND                                                   
058600         MOVE PERS-IDNAMN        TO RESP-IDNAMN-BEREDARE                  
058700         MOVE PERS-IDTFN         TO RESP-IDTFN-BEREDARE                   
058800       ELSE                                                               
058900         MOVE ALL-SPACE          TO RESP-IDNAMN-BEREDARE                  
059000                                    RESP-IDTFN-BEREDARE                   
059100       END-IF                                                             
059200     ELSE                                                                 
059300       MOVE ALL-SPACE            TO RESP-IDNAMN-BEREDARE                  
059400                                    RESP-IDTFN-BEREDARE                   
059500     END-IF                                                               
059600                                                                          
059700     MOVE STYR-ADINLOMR-FB       TO W-ADINLOMR                            
059800     PERFORM IMS-GU-PLAA11                                                
059900     IF SEGMENT-FOUND                                                     
060000       MOVE PLAA-6006-IDPERSON-ANSV                                       
060100                                 TO SPAR-IDPERSON-ANSV                    
060200     ELSE                                                                 
060300       MOVE +0                   TO SPAR-IDPERSON-ANSV                    
060400     END-IF                                                               
060500                                                                          
060600     IF SPAR-IDPERSON-ANSV  > +0                                          
060700       MOVE 'CDC     '           TO W-KDARBTYP                            
060800       MOVE SPAR-IDPERSON-ANSV   TO W-IDPERSON                            
060900       PERFORM IMS-GU-P311                                                
061000       IF SEGMENT-FOUND                                                   
061100         MOVE PERS-IDNAMN        TO RESP-IDNAMN-INLEV                     
061200         MOVE PERS-IDTFN         TO RESP-IDTFN-INLEV                      
061300       ELSE                                                               
061400         MOVE ALL-SPACE          TO RESP-IDNAMN-INLEV                     
061500                                    RESP-IDTFN-INLEV                      
061600       END-IF                                                             
061700     ELSE                                                                 
061800       MOVE ALL-SPACE            TO RESP-IDNAMN-INLEV                     
061900                                    RESP-IDTFN-INLEV                      
062000     END-IF                                                               
062100                                                                          
062200     MOVE 'QUAL'                 TO W-KDARBTYP                            
062300                                    W-KDARBTYP-B                          
062400     IF W-IDARTNR > ZERO                                                  
062500       PERFORM FCA-SOEK-IDARTNR                                           
062600     ELSE                                                                 
062700       PERFORM FCB-SOEK-IDLEVNR                                           
062800     END-IF                                                               
062900     IF SW-TRAEFF = 'J'                                                   
063000       MOVE WS-IDPERSON          TO W-IDPERSON                            
063100       PERFORM IMS-GU-P311                                                
063200       IF SEGMENT-FOUND                                                   
063300         MOVE PERS-IDNAMN        TO RESP-IDNAMN-KVAL                      
063400         MOVE PERS-IDTFN         TO RESP-IDTFN-KVAL                       
063500       ELSE                                                               
063600         MOVE ALL-SPACE          TO RESP-IDNAMN-KVAL                      
063700                                    RESP-IDTFN-KVAL                       
063800       END-IF                                                             
063900     ELSE                                                                 
064000       MOVE ALL-SPACE            TO RESP-IDNAMN-KVAL                      
064100                                    RESP-IDTFN-KVAL                       
064200     END-IF                                                               
064300                                                                          
064400     MOVE 'CDC'                  TO W-KDARBTYP                            
064500                                    W-KDARBTYP-B                          
064600     IF W-IDARTNR > ZERO                                                  
064700       PERFORM FCA-SOEK-IDARTNR                                           
064800     ELSE                                                                 
064900       PERFORM FCB-SOEK-IDLEVNR                                           
065000     END-IF                                                               
065100     IF SW-TRAEFF = 'J'                                                   
065200       MOVE WS-IDPERSON          TO W-IDPERSON                            
065300       PERFORM IMS-GU-P311                                                
065400       IF SEGMENT-FOUND                                                   
065500         MOVE PERS-IDNAMN        TO RESP-IDNAMN-FORP                      
065600         MOVE PERS-IDTFN         TO RESP-IDTFN-FORP                       
065700       ELSE                                                               
065800         MOVE ALL-SPACE          TO RESP-IDNAMN-FORP                      
065900                                    RESP-IDTFN-FORP                       
066000       END-IF                                                             
066100     ELSE                                                                 
066200       MOVE ALL-SPACE            TO RESP-IDNAMN-FORP                      
066300                                    RESP-IDTFN-FORP                       
066400     END-IF                                                               
066500                                                                          
066600     IF SPAR-ADLAGOMR > +0                                                
066700       MOVE SPAR-ADLAGOMR        TO W-ADINLOMR                            
066800       PERFORM IMS-GU-PLAA11                                              
066900       IF SEGMENT-FOUND                                                   
067000         MOVE 'CDC     '         TO W-KDARBTYP                            
067100         MOVE PLAA-6006-IDPERSON-ANSV                                     
067200                                 TO W-IDPERSON                            
067300         PERFORM IMS-GU-P311                                              
067400         IF SEGMENT-FOUND                                                 
067500           MOVE PERS-IDNAMN      TO RESP-IDNAMN-LO                        
067600           MOVE PERS-IDTFN       TO RESP-IDTFN-LO                         
067700         ELSE                                                             
067800           MOVE ALL-SPACE        TO RESP-IDNAMN-LO                        
067900                                    RESP-IDTFN-LO                         
068000         END-IF                                                           
068100       ELSE                                                               
068200         MOVE ALL-SPACE          TO RESP-IDNAMN-LO                        
068300                                    RESP-IDTFN-LO                         
068400       END-IF                                                             
068500     ELSE                                                                 
068600       MOVE ALL-SPACE            TO RESP-IDNAMN-LO                        
068700                                    RESP-IDTFN-LO                         
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 FCA-SOEK-IDARTNR SECTION.                                                
069200                                                                          
069300     MOVE NEJ                    TO SW-TRAEFF                             
069400     PERFORM IMS-GU-WDP3A                                                 
069500     PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = 'J'                     
069600       IF SEQA-IDARTNR-TOM < W-IDARTNR                                    
069700         PERFORM IMS-GN-WDP3A                                             
069800       ELSE                                                               
069900         IF SEQA-IDARTNR-FOM <= W-IDARTNR AND                             
070000         SEQA-IDARTNR-TOM >= W-IDARTNR                                    
070100           MOVE 'J'              TO SW-TRAEFF                             
070200         ELSE                                                             
070300           MOVE 'GE'             TO STATUS-WS                             
070400         END-IF                                                           
070500       END-IF                                                             
070600     END-PERFORM                                                          
070700     IF SW-TRAEFF = 'J'                                                   
070800       MOVE SEQA-IDPERSON        TO WS-IDPERSON                           
070900     ELSE                                                                 
071000       MOVE SPAR-IDLEVNR         TO W-IDLEVNR-B                           
071100       PERFORM IMS-GU-WDP3B                                               
071200       IF SEGMENT-FOUND                                                   
071300         MOVE 'J'                TO SW-TRAEFF                             
071400         MOVE SEQB-IDPERSON      TO WS-IDPERSON                           
071500       ELSE                                                               
071600         PERFORM IMS-GU-WDP3C                                             
071700         PERFORM UNTIL SEGMENT-MISSING OR SW-TRAEFF = 'J'                 
071800           IF SEQC-IDFKNGRP-TOM < SPAR-IDFKNGRP                           
071900             PERFORM IMS-GN-WDP3C                                         
072000           ELSE                                                           
072100             IF SEQC-IDFKNGRP-FOM <= SPAR-IDFKNGRP AND                    
072200                SEQC-IDFKNGRP-TOM >= SPAR-IDFKNGRP                        
072300               MOVE 'J'          TO SW-TRAEFF                             
072400             ELSE                                                         
072500               MOVE 'GE'         TO STATUS-WS                             
072600             END-IF                                                       
072700           END-IF                                                         
072800         END-PERFORM                                                      
072900         IF SW-TRAEFF = 'J'                                               
073000           MOVE SEQC-IDPERSON    TO WS-IDPERSON                           
073100         END-IF                                                           
073200       END-IF                                                             
073300     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
073600 FCB-SOEK-IDLEVNR SECTION.                                                
073700                                                                          
073800     MOVE NEJ                    TO SW-TRAEFF                             
073900     MOVE WS-IDLEVNR             TO W-IDLEVNR-B                           
074000     PERFORM IMS-GU-WDP3B                                                 
074100     IF SEGMENT-FOUND                                                     
074200       MOVE 'J'                  TO SW-TRAEFF                             
074300       MOVE SEQB-IDPERSON        TO WS-IDPERSON                           
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700*----------------------------------------------------------------*        
074800 MFS-ERASE-FIELD-OUT SECTION.                                             
074900                                                                          
075000*    --- ALLA UTDATA-FÄLT                                                 
075100     MOVE ALL-SPACE              TO RESP-ADINLOMR-FB                      
075200                                    RESP-ADINLOMR-FP                      
075300                                    RESP-ADLAGOMR                         
075400                                    RESP-IDNAMN-INLEV                     
075500                                    RESP-IDTFN-INLEV                      
075600                                    RESP-IDNAMN-LO                        
075700                                    RESP-IDTFN-LO                         
075800                                    RESP-IDNAMN-ANSK                      
075900                                    RESP-IDTFN-ANSK                       
076000                                    RESP-IDNAMN-BEREDARE                  
076100                                    RESP-IDTFN-BEREDARE                   
076200                                    RESP-IDNAMN-FORP                      
076300                                    RESP-IDTFN-FORP                       
076400                                    RESP-IDNAMN-KVAL                      
076500                                    RESP-IDTFN-KVAL                       
076600                                    RESP-IDNAMN-INK                       
076700                                    RESP-IDTFN-INK                        
076800     .                                                                    
076900     EJECT                                                                
077000*----------------------------------------------------------------*        
077100* --- IMS SECTIONS ---                                                    
077200*----------------------------------------------------------------*        
077300 IMS-GU-ARTC-K601 SECTION.                                                
077400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
077500       DELIMITED BY SIZE INTO SSA1                                        
077600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
077700     CALL CBLTDLI             USING GU  ARTC-PCB                          
077800                                    DLI-IO-AREA                           
077900                                    SSA1                                  
078000     MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
078100     PERFORM IMS-STATUSCHECK                                              
078200     .                                                                    
078300     SKIP2                                                                
078400*                                                                         
078500 IMS-GNP-ARTC-K611 SECTION.                                               
078600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
078700       DELIMITED BY SIZE INTO SSA1                                        
078800     MOVE 'WLARTC11'             TO SSA2                                  
078900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
079000     CALL CBLTDLI             USING GNP ARTC-PCB                          
079100                                    DLI-IO-AREA                           
079200                                    SSA1                                  
079300                                    SSA2                                  
079400     MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
079500     PERFORM IMS-STATUSCHECK                                              
079600     .                                                                    
079700     SKIP2                                                                
079800*                                                                         
079900 IMS-GU-WDK711 SECTION.                                                   
080000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
080100       DELIMITED BY SIZE INTO SSA1                                        
080200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
080300       DELIMITED BY SIZE INTO SSA2                                        
080400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
080500     CALL CBLTDLI             USING GU  WDK7-PCB                          
080600                                    DLI-IO-AREA-WDK711                    
080700                                    SSA1 SSA2                             
080800     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
080900     PERFORM IMS-STATUSCHECK                                              
081000     .                                                                    
081100     SKIP2                                                                
081200*                                                                         
081210 IMS-GNP-WDK722 SECTION.                                                  
081220     MOVE 'WDK722' TO SSA1                                                
081260     MOVE '  GE'                 TO GOOD-STATUSCODES                      
081270     CALL CBLTDLI             USING GNP WDK7-PCB                          
081280                                    DLI-IO-AREA-WDK722                    
081290                                    SSA1                                  
081291     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
081292     PERFORM IMS-STATUSCHECK                                              
081293     .                                                                    
081294     SKIP2                                                                
081295*                                                                         
081296 IMS-GU-WDK712 SECTION.                                                   
081297     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
081298          DELIMITED BY SIZE INTO SSA1                                     
081299     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
081300          DELIMITED BY SIZE INTO SSA2                                     
081301     MOVE '  GE' TO GOOD-STATUSCODES                                      
081302     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
081303     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
081304     PERFORM IMS-STATUSCHECK                                              
081305     .                                                                    
081306     SKIP2                                                                
081310 IMS-GU-P311      SECTION.                                                
081400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
081500       DELIMITED BY SIZE INTO SSA1                                        
081600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
081700       DELIMITED BY SIZE INTO SSA2                                        
081800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
081900     CALL CBLTDLI             USING GU  WDP3-PCB                          
082000                                    DLI-IO-AREA-P311                      
082100                                    SSA1 SSA2                             
082200     MOVE WDP3-STATUS-CODE       TO STATUS-WS                             
082300     PERFORM IMS-STATUSCHECK                                              
082400     .                                                                    
082500     SKIP2                                                                
082700 IMS-GU-PLAA11    SECTION.                                                
082800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-X ')'                         
082900       DELIMITED BY SIZE INTO SSA1                                        
083000     STRING 'W6PLAA11(ADINLOMR =' W-ADINLOMR-X ')'                        
083100       DELIMITED BY SIZE INTO SSA2                                        
083200     MOVE '  GBGE'               TO GOOD-STATUSCODES                      
083300     CALL CBLTDLI             USING GU  PLAA-PCB                          
083400                                    DLI-IO-AREA-PLAA                      
083500                                    SSA1 SSA2                             
083600     MOVE PLAA-STATUS-CODE       TO STATUS-WS                             
083700     PERFORM IMS-STATUSCHECK                                              
083800     .                                                                    
083900     SKIP2                                                                
084000*                                                                         
084100 IMS-GU-WDP3A SECTION.                                                    
084200     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
084300                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
084400                    '&KDARBTYP =' W-KDARBTYP ')'                          
084500       DELIMITED BY SIZE INTO SSA1                                        
084600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
084700     CALL CBLTDLI             USING GU WDP3A-PCB                          
084800                                    DLI-IO-AREA-WDP3A SSA1                
084900     MOVE WDP3A-STATUS-CODE      TO STATUS-WS                             
085000     PERFORM IMS-STATUSCHECK                                              
085100     .                                                                    
085200     SKIP2                                                                
085300*                                                                         
085400 IMS-GN-WDP3A SECTION.                                                    
085500     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
085600                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
085700                    '&KDARBTYP =' W-KDARBTYP ')'                          
085800       DELIMITED BY SIZE INTO SSA1                                        
085900     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
086000     CALL CBLTDLI             USING GN WDP3A-PCB                          
086100                                    DLI-IO-AREA-WDP3A SSA1                
086200     MOVE WDP3A-STATUS-CODE      TO STATUS-WS                             
086300     PERFORM IMS-STATUSCHECK                                              
086400     .                                                                    
086500     SKIP2                                                                
086600*                                                                         
086700 IMS-GU-WDP3B SECTION.                                                    
086800     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
086900                    '&KDARBTYP =' W-KDARBTYP ')'                          
087000       DELIMITED BY SIZE INTO SSA1                                        
087100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
087200     CALL CBLTDLI             USING GU WDP3B-PCB                          
087300                                    DLI-IO-AREA-WDP3B SSA1                
087400     MOVE WDP3B-STATUS-CODE      TO STATUS-WS                             
087500     PERFORM IMS-STATUSCHECK                                              
087600     .                                                                    
087700     SKIP2                                                                
087800*                                                                         
087900 IMS-GU-WDP3C SECTION.                                                    
088000     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
088100                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
088200                    '&KDARBTYP =' W-KDARBTYP ')'                          
088300       DELIMITED BY SIZE INTO SSA1                                        
088400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
088500     CALL CBLTDLI             USING GU WDP3C-PCB                          
088600                                    DLI-IO-AREA-WDP3C SSA1                
088700     MOVE WDP3C-STATUS-CODE      TO STATUS-WS                             
088800     PERFORM IMS-STATUSCHECK                                              
088900     .                                                                    
089000     SKIP2                                                                
089100*                                                                         
089200 IMS-GN-WDP3C SECTION.                                                    
089300     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
089400                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
089500                    '&KDARBTYP =' W-KDARBTYP ')'                          
089600       DELIMITED BY SIZE INTO SSA1                                        
089700     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
089800     CALL CBLTDLI             USING GN WDP3C-PCB                          
089900                                    DLI-IO-AREA-WDP3C SSA1                
090000     MOVE WDP3C-STATUS-CODE      TO STATUS-WS                             
090100     PERFORM IMS-STATUSCHECK                                              
090200     .                                                                    
090300     SKIP2                                                                
090400*                                                                         
090500 IMS-STATUSCHECK SECTION.                                                 
090600     SET STATUS-IX               TO 1                                     
090700     SEARCH GOOD-STATUS                                                   
090800       AT END                                                             
090900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
091000           DELIMITED BY SIZE INTO ERROR-TEXT                              
091100         CALL FELLOG                                                      
091200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
091300         CONTINUE                                                         
091400     END-SEARCH                                                           
091500     .                                                                    
