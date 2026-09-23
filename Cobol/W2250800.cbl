000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300     SKIP2                                                                
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.                 W2250800.                                    
000600*              PROGRAM CONVERTED BY                                       
000700*              COBOL CONVERSION AID PO 5785-ABJ                           
000800*              CONVERSION DATE 05/25/91 19:45:18.                         
000900*AUTHOR.                     IDK, GÖTEBORG.                               
001000*DATE-WRITTEN.               SEP 1978.                                    
001100*    SKIP2                                                                
001200*REMARKS.                                                                 
001300*    FUNKTION.                                                            
001400*        PROGRAMMET MACHAR IHOP FILERNA W22505 (RO-INFO) OCH              
001500*        W22509 (LEVERANSINFO).                                           
001600*        SAMMA ID                                                         
001610*            NOLLSTÄLL W22505-INFO PÅ W22511 FÖR ATT FÅ NOLL I SDC        
001700*            FLYTTA BÅDAS INFO TILL W22511                                
001800*        SKILDA ID                                                        
001900*        A.                                                               
001910*            NOLLSTÄLL W22505-INFO PÅ W22511 FÖR ATT FÅ NOLL I SDC        
002000*            FLYTTA W22505-INFO TILL W22511                               
002100*            NOLLSTÄLL W22509-INFO PÅ W22511                              
002200*        B.                                                               
002300*            FLYTTA W22509-INFO TILL W22511                               
002400*            NOLLSTÄLL W22505-INFO PÅ W22511                              
002410*                                                                         
002420*        SDC:                                                             
002430*        INGA FÖRÄNDRINGAR NU.                                            
002440*        FIL MED RESTORDERINFO W22505 INNEHÅLLER EJ                       
002441*        SDC-INFO.                                                        
002442*        INFIL W22509 OCH UTFILEN INNEHÅLLER T V                          
002443*        SDC-INFO SOM FYLLS MED NOLL.                                     
002444*                                                                         
002450*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900*--------------------------------------- INPUT RESTORDERINFO              
003000     SKIP1                                                                
003100     SELECT  W22505          ASSIGN UT-S-W22508D1.                        
003200     SKIP1                                                                
003300*--------------------------------------- INPUT LEVERANSINFO               
003400     SELECT  W22509          ASSIGN UT-S-W22508D2.                        
003500     SKIP1                                                                
003600*--------------------------------------- OUTPUT RESTORDERINFO OCH         
003700*                                        LEVERANSINFO                     
003800     SKIP1                                                                
003900     SELECT W22511           ASSIGN UT-S-W22508D3.                        
004000*--------------------------------------- RANDOM SORTFIL                   
004100     SELECT  SORTFIL         ASSIGN UT-S-W22508DS.                        
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400 FILE SECTION.                                                            
004500     SKIP2                                                                
004600 FD  W22505                                                               
004700     RECORDING F                                                          
004800     BLOCK 0                                                              
004900                           .                                              
005000*01  -COPY W225P231C0  -L                                                 
005200     SKIP2                                                                
005300 FD  W22509                                                               
005400     RECORDING F                                                          
005500     BLOCK 0                                                              
005600                           .                                              
005700*01  -COPY W225P233C0  -L                                                 
005900     SKIP2                                                                
006000 FD  W22511                                                               
006100     RECORDING F                                                          
006200     BLOCK 0                                                              
006300                           .                                              
006400*01  POST -COPY W225P234C0 -PRE ROLEV- -L                        C        
006600     EJECT                                                                
006700 SD  SORTFIL                                                              
006800                .                                                         
006900 01  SORTPOST.                                                            
007000     05  SORT-RANDOMKEY             PIC X(4).                             
007100     05  SORT-AREA.                                                       
007200*        07 POST   -COPY W225P234C0 -PRE SORT-                            
007400     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007510                                                                          
007600*    -- CHECKED BY WY2000                                                 
007900***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
008000*                                                                         
008100     SKIP3                                                                
008200 01  RO-ID.                                                               
008300     03  RO-ID-IDARTNR       PIC S9(9).                                   
008400 01  LEV-ID.                                                              
008500     03  LEV-ID-IDARTNR      PIC S9(9).                                   
008600     SKIP1                                                                
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
008900     03  W015RAND            PIC X(8)    VALUE 'W015RAND'.                
009000     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
009100     SKIP1                                                                
009200 01  KONSTANTER.                                                          
009300     03  JA                  PIC X       VALUE 'J'.                       
009400     03  NEJ                 PIC X       VALUE 'N'.                       
009500     03  DATABASE            PIC X(4)    VALUE 'WDK6'.                    
009600     03  ABENDKOD            PIC S9(4)   VALUE +0 COMP SYNC.              
009700     SKIP1                                                                
009800 01  IX                      PIC S9(9)                COMP SYNC.          
009900     SKIP1                                                                
010000 01  EOF-SW.                                                              
010100     03  SORT-EOF            PIC X       VALUE 'N'.                       
010200     EJECT                                                                
010300*--------------------------------------- PARAMETRAR TILL POSTSUM          
010400     SKIP1                                                                
010500*01  -COPY W0005      -PRE POSTSUM-                                       
010700     EJECT                                                                
010800*--------------------------------------- AREA FÖR W22505-POST             
010900*                                        RESTORDERINFO                    
011000*01  AREA -COPY W225P231   -PRE RO-                                       
011200     SKIP1                                                                
011300*--------------------------------------- AREA FÖR W22509-POST             
011400*                                        LEVERANSINFO                     
011500*01  AREA -COPY W225P233   -PRE LEV-                                      
011700     SKIP1                                                                
011800*--------------------------------------- AREA FÖR W22511-POST             
011900*                                        RESTORDERINFO OCH                
012000*                                        LEVERANSINFO                     
012100*01  AREA -COPY W225P234C0 -PRE ROLEV-                                    
012300     EJECT                                                                
012400 01  SORTWPOST.                                                           
012500     05  SORTW-RANDOMKEY             PIC X(4).                            
012600     05  SORTW-AREA.                                                      
012700*        07 POST   -COPY W225P234   -PRE SORTW-                           
012900     EJECT                                                                
013000 PROCEDURE DIVISION.                                                      
013100     SKIP2                                                                
013200     PERFORM A-INITIERA                                                   
013300     SORT SORTFIL ASCENDING                                               
013400          SORT-RANDOMKEY SORT-IDARTNR                                     
013500*                                                                         
013600     INPUT PROCEDURE AA-BEARBETA                                          
013700     OUTPUT PROCEDURE E-RETURN-SORT                                       
013800*                                                                         
013900     IF SORT-RETURN > 0                                                   
014000       DISPLAY 'SORT-FEL'                                                 
014100       MOVE +20 TO ABENDKOD                                               
014200       PERFORM S99-ABEND                                                  
014300     ELSE                                                                 
014400       MOVE ZERO TO RETURN-CODE                                           
014500       PERFORM Z-AVSLUTA                                                  
014600       MOVE ZERO TO RETURN-CODE                                           
014700       GOBACK                                                             
014800     END-IF                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INITIERA SECTION.                                                      
015200     SKIP2                                                                
015300     OPEN INPUT W22505 W22509                                             
015400     OUTPUT W22511                                                        
015500     MOVE 'W2250800'         TO POSTSUM-PROGNAMN                          
015600     .                                                                    
015700     EJECT                                                                
015800 AA-BEARBETA SECTION.                                                     
015900     SKIP2                                                                
016000     PERFORM S01-LAS-RO                                                   
016100     PERFORM S02-LAS-LEV                                                  
016200     PERFORM UNTIL RO-ID = HIGH-VALUE OR LEV-ID = HIGH-VALUE              
016500       IF  RO-ID = LEV-ID                                                 
016510         PERFORM S05-NOLLSTALL-ROINFO                                     
016600         PERFORM B-FLYTTA-RO                                              
016700         PERFORM C-FLYTTA-LEV                                             
016800         PERFORM S01-LAS-RO                                               
016900         PERFORM S02-LAS-LEV                                              
017000       ELSE                                                               
017100         IF  RO-ID LESS LEV-ID                                            
017200           PERFORM S04-NOLLSTALL-LEVINFO                                  
017210           PERFORM S05-NOLLSTALL-ROINFO                                   
017300           PERFORM B-FLYTTA-RO                                            
017400           PERFORM S01-LAS-RO                                             
017500         ELSE                                                             
017600           IF  LEV-ID LESS RO-ID                                          
017700             PERFORM S05-NOLLSTALL-ROINFO                                 
017800             PERFORM C-FLYTTA-LEV                                         
017900             PERFORM S02-LAS-LEV                                          
018000           END-IF                                                         
018100         END-IF                                                           
018200       END-IF                                                             
018300       CALL W015RAND USING SORTW-IDARTNR                                  
018400                     SORTW-RANDOMKEY DATABASE                             
018500       RELEASE SORTPOST FROM SORTW-AREA                                   
018600     END-PERFORM                                                          
018700     .                                                                    
018800     EJECT                                                                
018900 B-FLYTTA-RO SECTION.                                                     
019000     SKIP2                                                                
019100     MOVE '234'              TO ROLEV-IDPTYP                              
019200     MOVE RO-IDARTNR         TO ROLEV-IDARTNR                             
019300     MOVE RO-KDCLPOST        TO ROLEV-KDCLPOST                            
019400     MOVE RO-KVROS-CDC-1-2   TO ROLEV-KVROS-CDC-1-2                       
019500     MOVE RO-KVROS-CDC-3-4   TO ROLEV-KVROS-CDC-3-4                       
019600     MOVE RO-KVRORAD-KVAR-V1-CDC TO ROLEV-KVRORAD-KVAR-V1-CDC             
019700     MOVE RO-KVRORAD-KVAR-IV-CDC TO ROLEV-KVRORAD-KVAR-IV-CDC             
019800     MOVE RO-KVRORAD-KVAR-P-CDC  TO ROLEV-KVRORAD-KVAR-P-CDC              
019900     MOVE RO-TIRODAT-ORDER-CDC   TO ROLEV-TIRODAT-ORDER-CDC               
020000     MOVE RO-KVRORAD-TOT-CDC     TO ROLEV-KVRORAD-CDC-TOT                 
020800     MOVE ROLEV-AREA             TO SORTW-AREA                            
020900     .                                                                    
021000     EJECT                                                                
021100 C-FLYTTA-LEV SECTION.                                                    
021200     SKIP2                                                                
021300     MOVE '234'                      TO ROLEV-IDPTYP                      
021400     MOVE LEV-IDARTNR                TO ROLEV-IDARTNR                     
021500     MOVE LEV-KVRORAD-CDC-1-2-VECKA (2)                                   
021510                                  TO ROLEV-KVRORAD-CDC-1-2-VECKA          
021600     MOVE LEV-KVRORAD-CDC-3-4-VECKA (2)                                   
021700     TO ROLEV-KVRORAD-CDC-3-4-VECKA                                       
021800     MOVE LEV-KVEJRO-CDC-1-2 (2)     TO ROLEV-KVEJRO-CDC-1-2              
021900     MOVE LEV-KVEJRO-CDC-3-4 (2)     TO ROLEV-KVEJRO-CDC-3-4              
022000     MOVE LEV-KVRORAD-SDC-1-2-VECKA (2)                                   
022010                                  TO ROLEV-KVRORAD-SDC-1-2-VECKA          
022100     MOVE LEV-KVRORAD-SDC-3-4-VECKA (2)                                   
022200     TO ROLEV-KVRORAD-SDC-3-4-VECKA                                       
022300     MOVE LEV-KVEJRO-SDC-1-2 (2)     TO ROLEV-KVEJRO-SDC-1-2              
022400     MOVE LEV-KVEJRO-SDC-3-4 (2)     TO ROLEV-KVEJRO-SDC-3-4              
022500     SKIP1                                                                
022600     MOVE +1                         TO IX                                
022700     PERFORM UNTIL                                                        
022800      ( IX GREATER 3 )                                                    
022900       MOVE LEV-KVAVBRAD-CDC-1-2 (IX) TO ROLEV-KVAVBRAD-CDC-1-2           
023000       (IX)                                                               
023100       MOVE LEV-KVAVBRAD-CDC-3-4 (IX) TO ROLEV-KVAVBRAD-CDC-3-4           
023200       (IX)                                                               
023300       MOVE LEV-KVFYSAVV-CDC-1-2 (IX) TO ROLEV-KVFYSAVV-CDC-1-2           
023400       (IX)                                                               
023500       MOVE LEV-KVFYSAVV-CDC-3-4 (IX) TO ROLEV-KVFYSAVV-CDC-3-4           
023600       (IX)                                                               
023700       MOVE LEV-KVINORD-CDC-1-2 (IX)  TO ROLEV-KVINORD-CDC-1-2            
023710       (IX)                                                               
023800       MOVE LEV-KVINORD-CDC-3-4 (IX)  TO ROLEV-KVINORD-CDC-3-4            
023900       (IX)                                                               
024000       MOVE LEV-KVRORAD-CDC-1-2 (IX)  TO ROLEV-KVRORAD-CDC-1-2            
024010       (IX)                                                               
024100       MOVE LEV-KVRORAD-CDC-3-4 (IX)  TO ROLEV-KVRORAD-CDC-3-4            
024200       (IX)                                                               
024300       MOVE LEV-KVAVBRAD-SDC-1-2 (IX) TO ROLEV-KVAVBRAD-SDC-1-2           
024310       (IX)                                                               
024400       MOVE LEV-KVAVBRAD-SDC-3-4 (IX) TO ROLEV-KVAVBRAD-SDC-3-4           
024500       (IX)                                                               
024600       MOVE LEV-KVFYSAVV-SDC-1-2 (IX) TO ROLEV-KVFYSAVV-SDC-1-2           
024610       (IX)                                                               
024700       MOVE LEV-KVFYSAVV-SDC-3-4 (IX) TO ROLEV-KVFYSAVV-SDC-3-4           
024800       (IX)                                                               
024900       MOVE LEV-KVINORD-SDC-1-2 (IX) TO ROLEV-KVINORD-SDC-1-2 (IX)        
025000       MOVE LEV-KVINORD-SDC-3-4 (IX) TO ROLEV-KVINORD-SDC-3-4 (IX)        
025100       MOVE LEV-KVRORAD-SDC-1-2 (IX) TO ROLEV-KVRORAD-SDC-1-2 (IX)        
025200       MOVE LEV-KVRORAD-SDC-3-4 (IX) TO ROLEV-KVRORAD-SDC-3-4             
025300       (IX)                                                               
025400       ADD +1                          TO IX                              
025500     END-PERFORM                                                          
025600     MOVE ROLEV-AREA TO SORTW-AREA                                        
025700     .                                                                    
025800     EJECT                                                                
025900 E-RETURN-SORT SECTION.                                                   
026000     SKIP2                                                                
026100     PERFORM UNTIL                                                        
026200      NOT ( SORT-EOF = NEJ )                                              
026300       RETURN SORTFIL INTO SORTW-POST                                     
026400                  AT END MOVE JA TO SORT-EOF                              
026500       END-RETURN                                                         
026600       IF SORT-EOF = NEJ                                                  
026700         WRITE ROLEV-POST  FROM SORTW-AREA                                
026800         MOVE 'W22511'      TO POSTSUM-FDNAMN                             
026900         MOVE 'W22500D3'    TO POSTSUM-DDNAMN2                            
027000         MOVE SORTW-IDPTYP   TO POSTSUM-TRANSTYP                          
027100         CALL POSTSUM       USING POSTSUM-PARM                            
027200       END-IF                                                             
027300     END-PERFORM                                                          
027400     .                                                                    
027500     EJECT                                                                
027600 Z-AVSLUTA SECTION.                                                       
027700     SKIP2                                                                
027800     CLOSE W22505 W22509 W22511                                           
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300 S01-LAS-RO SECTION.                                                      
028400     SKIP2                                                                
028500     READ W22505 INTO RO-AREA                                             
028600     AT END MOVE HIGH-VALUE  TO RO-ID                                     
028700     END-READ                                                             
028800     IF  RO-ID NOT = HIGH-VALUE                                           
028900       MOVE RO-IDARTNR TO RO-ID-IDARTNR                                   
029000       MOVE 'W22505'       TO POSTSUM-FDNAMN                              
029100       MOVE 'W22500D1'     TO POSTSUM-DDNAMN2                             
029200       MOVE RO-IDPTYP      TO POSTSUM-TRANSTYP                            
029300       CALL POSTSUM        USING POSTSUM-PARM                             
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 S02-LAS-LEV SECTION.                                                     
029800     SKIP2                                                                
029900     READ W22509 INTO LEV-AREA                                            
030000     AT END MOVE HIGH-VALUE TO LEV-ID                                     
030100     END-READ                                                             
030200     IF  LEV-ID NOT = HIGH-VALUE                                          
030300       MOVE LEV-IDARTNR TO LEV-ID-IDARTNR                                 
030400       MOVE 'W22509'       TO POSTSUM-FDNAMN                              
030500       MOVE 'W22500D2'     TO POSTSUM-DDNAMN2                             
030600       MOVE LEV-IDPTYP     TO POSTSUM-TRANSTYP                            
030700       CALL POSTSUM        USING POSTSUM-PARM                             
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 S04-NOLLSTALL-LEVINFO SECTION.                                           
031200     SKIP2                                                                
031300     MOVE ZERO           TO ROLEV-IDPTYP                                  
031400     ROLEV-IDARTNR                                                        
031500     ROLEV-KDCLPOST                                                       
031600     ROLEV-KVAVBRAD-CDC-1-2 (1)                                           
031700     ROLEV-KVAVBRAD-CDC-3-4 (1)                                           
031800     ROLEV-KVFYSAVV-CDC-1-2 (1)                                           
031900     ROLEV-KVFYSAVV-CDC-3-4 (1)                                           
032000     ROLEV-KVINORD-CDC-1-2  (1)                                           
032100     ROLEV-KVINORD-CDC-3-4  (1)                                           
032200     ROLEV-KVRORAD-CDC-1-2  (1)                                           
032300     ROLEV-KVRORAD-CDC-3-4  (1)                                           
032400     ROLEV-KVAVBRAD-CDC-1-2 (2)                                           
032500     ROLEV-KVAVBRAD-CDC-3-4 (2)                                           
032600     ROLEV-KVFYSAVV-CDC-1-2 (2)                                           
032700     ROLEV-KVFYSAVV-CDC-3-4 (2)                                           
032800     ROLEV-KVINORD-CDC-1-2  (2)                                           
032900     ROLEV-KVINORD-CDC-3-4  (2)                                           
033000     ROLEV-KVRORAD-CDC-1-2  (2)                                           
033100     ROLEV-KVRORAD-CDC-3-4  (2)                                           
033200     ROLEV-KVAVBRAD-CDC-1-2 (3)                                           
033300     ROLEV-KVAVBRAD-CDC-3-4 (3)                                           
033400     ROLEV-KVFYSAVV-CDC-1-2 (3)                                           
033500     ROLEV-KVFYSAVV-CDC-3-4 (3)                                           
033600     ROLEV-KVINORD-CDC-1-2  (3)                                           
033700     ROLEV-KVINORD-CDC-3-4  (3)                                           
033800     ROLEV-KVRORAD-CDC-1-2  (3)                                           
033900     ROLEV-KVRORAD-CDC-3-4  (3)                                           
034000     ROLEV-KVRORAD-CDC-1-2-VECKA                                          
034100     ROLEV-KVRORAD-CDC-3-4-VECKA                                          
034200     ROLEV-KVEJRO-CDC-1-2                                                 
034300     ROLEV-KVEJRO-CDC-3-4                                                 
034400     ROLEV-KVRORAD-CDC-TOT                                                
034500     ROLEV-KVAVBRAD-SDC-1-2 (1)                                           
034600     ROLEV-KVAVBRAD-SDC-3-4 (1)                                           
034700     ROLEV-KVFYSAVV-SDC-1-2 (1)                                           
034800     ROLEV-KVFYSAVV-SDC-3-4 (1)                                           
034900     ROLEV-KVINORD-SDC-1-2  (1)                                           
035000     ROLEV-KVINORD-SDC-3-4  (1)                                           
035100     ROLEV-KVRORAD-SDC-1-2  (1)                                           
035200     ROLEV-KVRORAD-SDC-3-4  (1)                                           
035300     ROLEV-KVAVBRAD-SDC-1-2 (2)                                           
035400     ROLEV-KVAVBRAD-SDC-3-4 (2)                                           
035500     ROLEV-KVFYSAVV-SDC-1-2 (2)                                           
035600     ROLEV-KVFYSAVV-SDC-3-4 (2)                                           
035700     ROLEV-KVINORD-SDC-1-2  (2)                                           
035800     ROLEV-KVINORD-SDC-3-4  (2)                                           
035900     ROLEV-KVRORAD-SDC-1-2  (2)                                           
036000     ROLEV-KVRORAD-SDC-3-4  (2)                                           
036100     ROLEV-KVAVBRAD-SDC-1-2 (3)                                           
036200     ROLEV-KVAVBRAD-SDC-3-4 (3)                                           
036300     ROLEV-KVFYSAVV-SDC-1-2 (3)                                           
036400     ROLEV-KVFYSAVV-SDC-3-4 (3)                                           
036500     ROLEV-KVINORD-SDC-1-2  (3)                                           
036600     ROLEV-KVINORD-SDC-3-4  (3)                                           
036700     ROLEV-KVRORAD-SDC-1-2  (3)                                           
036800     ROLEV-KVRORAD-SDC-3-4  (3)                                           
036900     ROLEV-KVRORAD-SDC-1-2-VECKA                                          
037000     ROLEV-KVRORAD-SDC-3-4-VECKA                                          
037100     ROLEV-KVEJRO-SDC-1-2                                                 
037200     ROLEV-KVEJRO-SDC-3-4                                                 
037300     .                                                                    
037400     EJECT                                                                
037500 S05-NOLLSTALL-ROINFO SECTION.                                            
037600     MOVE ZERO           TO ROLEV-IDPTYP                                  
037700     ROLEV-IDARTNR                                                        
037800     ROLEV-KDCLPOST                                                       
037900     ROLEV-KVROS-CDC-1-2                                                  
038000     ROLEV-KVROS-CDC-3-4                                                  
038100     ROLEV-TIRODAT-ORDER-CDC                                              
038200     ROLEV-KVRORAD-KVAR-V1-CDC                                            
038300     ROLEV-KVRORAD-KVAR-IV-CDC                                            
038400     ROLEV-KVRORAD-KVAR-P-CDC                                             
038500     ROLEV-KVRORAD-CDC-TOT                                                
038600     ROLEV-KVROS-SDC-1-2                                                  
038700     ROLEV-KVROS-SDC-3-4                                                  
038800     ROLEV-TIRODAT-ORDER-SDC                                              
038900     ROLEV-KVRORAD-KVAR-V1-SDC                                            
039000     ROLEV-KVRORAD-KVAR-IV-SDC                                            
039100     ROLEV-KVRORAD-KVAR-P-SDC                                             
039200     ROLEV-KVRORAD-SDC-TOT                                                
039300     .                                                                    
039400     EJECT                                                                
039500 S99-ABEND SECTION.                                                       
039600     SKIP2                                                                
039700     CALL ABEND USING ABENDKOD                                            
039800     .                                                                    
