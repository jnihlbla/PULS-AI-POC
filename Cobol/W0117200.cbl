000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0117200.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   94/10/27.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER FILER MED UPPGIFTER FRÅN FLERA DATABASER        
001000*        SKAPAR DAGLIGT LAGERBAND                                         
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- SAMTLIGA DATAELEMENT FRÅN WDK601 OCH WDK611                
002600     SELECT W01160                     ASSIGN TO W01172D1.                
002700     SKIP2                                                                
002800*          --- SAMTLIGA DATAELEMENT FRÅN WDK621                           
002900     SELECT W01161                     ASSIGN TO W01172D2.                
003000     SKIP2                                                                
003100*          --- UTVALDA DATAELEMENT FRÅN WDK901                            
003200     SELECT W01168                     ASSIGN TO W01172D3.                
003300     SKIP2                                                                
003700*          --- SUMMERAD KVBR PER ARTIKEL, FRÅN WDD902                     
003800     SELECT W01173                     ASSIGN TO W01172D5.                
003900     SKIP2                                                                
004000*          --- SAMTLIGA BENÄMNINGAR PER ARTIKEL, FRÅN WDD3                
004100     SELECT W01174                     ASSIGN TO W01172D6.                
004200     SKIP2                                                                
004300*          --- DAGLIGT LAGERBAND                                          
004400     SELECT W01172                     ASSIGN TO W01172D7.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W01160                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  -COPY W01160      -L.                                                
005500     SKIP3                                                                
005600 FD  W01161                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W01161      -L.                                                
006100     SKIP3                                                                
006200 FD  W01168                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  -COPY W01168      -L.                                                
006700     SKIP3                                                                
007400 FD  W01173                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700                                                                          
007800*01  -COPY W01173      -L.                                                
007900     SKIP3                                                                
008000 FD  W01174                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  -COPY W01174      -L.                                                
008500     SKIP3                                                                
008600 FD  W01172                                                               
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900                                                                          
009000*01  POST -COPY W011100 -PRE  W01172-   -L.                               
009100     EJECT                                                                
009200 WORKING-STORAGE SECTION.                                                 
009300                                                                          
009301                                                                          
009310*    -- CHECKED BY WY2000                                                 
009400 77  IDPGM                       PIC X(8)    VALUE 'W0117200'.            
009500 77  JA                          PIC X       VALUE 'J'.                   
009600 77  NEJ                         PIC X       VALUE 'N'.                   
009700                                                                          
009800 01  ARBETSAREOR.                                                         
009900     03 WS-KDFORP                 PIC 9(4)  VALUE ZERO.                   
010000                                                                          
010100                                                                          
010200 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
010300     88  END-OF-W01160                       VALUE 'J'.                   
010400                                                                          
010500 77  W01161-EOF-SW               PIC X       VALUE 'N'.                   
010600     88  END-OF-W01161                       VALUE 'J'.                   
010700                                                                          
010800 77  W01168-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W01168                       VALUE 'J'.                   
011000                                                                          
011400 77  W01173-EOF-SW               PIC X       VALUE 'N'.                   
011500     88  END-OF-W01173                       VALUE 'J'.                   
011600                                                                          
011700 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
011800     88  END-OF-W01174                       VALUE 'J'.                   
011900     EJECT                                                                
012000 01  ARBETSAREOR.                                                         
012100     03 WS-SPAR-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
012200     03 IX                       PIC S9(9)   VALUE ZERO COMP-3.           
012300                                                                          
012400                                                                          
012500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012600 01  FILLER REDEFINES DAGENS-DATUM.                                       
012700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013000     EJECT                                                                
013100 01  DYNAMISKA-SUBPROGRAM.                                                
013200*                                                                         
013300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013500     SKIP2                                                                
013600*    --- PARAMETRAR TILL ABEND                                            
013700                                                                          
013800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014000     SKIP2                                                                
014100 01  FELTEXT.                                                             
014200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL POSTSUM                                          
014600*                                                                         
014700*01  -COPY W0005   -PRE  POSTSUM-                                         
014800     EJECT                                                                
014900 01  W01160-AREA-START           PIC X(24)   VALUE                        
015000                                 'W01160-AREA-START  '.                   
015100     SKIP2                                                                
015200                                                                          
015300*01  AREA -COPY W01160     -PRE W01160-                                   
015400     EJECT                                                                
015500 01  W01161-AREA-START           PIC X(24)   VALUE                        
015600                                 'W01161-AREA-START  '.                   
015700     SKIP2                                                                
015800                                                                          
015900*01  AREA -COPY W01161     -PRE W01161-                                   
016000     EJECT                                                                
016100 01  W01168-AREA-START           PIC X(24)   VALUE                        
016200                                 'W01168-AREA-START  '.                   
016300     SKIP2                                                                
016400                                                                          
016500*01  AREA -COPY W01168     -PRE W01168-                                   
016600     EJECT                                                                
017300 01  W01173-AREA-START           PIC X(24)   VALUE                        
017400                                 'W01173-AREA-START  '.                   
017500     SKIP2                                                                
017600                                                                          
017700*01  AREA -COPY W01173     -PRE W01173-                                   
017800     EJECT                                                                
017900 01  W01174-AREA-START           PIC X(24)   VALUE                        
018000                                 'W01174-AREA-START  '.                   
018100     SKIP2                                                                
018200                                                                          
018300*01  AREA -COPY W01174     -PRE W01174-                                   
018400     EJECT                                                                
018500 01  DLB-AREA-START              PIC X(24)   VALUE                        
018600                                 'DLB-AREA-START  '.                      
018700     SKIP2                                                                
018800                                                                          
018900*01  AREA -COPY W011100     -PRE DLB-                                     
019000     EJECT                                                                
019100 PROCEDURE DIVISION.                                                      
019200     SKIP2                                                                
019300                                                                          
019400     PERFORM A-INIT                                                       
019500     PERFORM B-LAES-INFILER                                               
019600     PERFORM UNTIL END-OF-W01160                                          
019700        IF W01160-CLAG-KDERS-UTG = +0                                     
019800           MOVE W01160-CLAG-IDARTNR TO WS-SPAR-IDARTNR                    
019900           PERFORM C-NOLLSTALL-DAGLAGERBAND                               
020000           PERFORM D-BEHANDLA-W01160                                      
020100           PERFORM E-BEHANDLA-W01161                                      
020200           PERFORM F-BEHANDLA-W01168                                      
020400           PERFORM H-BEHANDLA-W01173                                      
020500           PERFORM I-BEHANDLA-W01174                                      
020600           PERFORM S11-SKRIV-DLB                                          
020700        END-IF                                                            
020800        PERFORM S01-LAES-W01160                                           
020900     END-PERFORM                                                          
021000     PERFORM Z-FINIT                                                      
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600                                                                          
021700                                                                          
021800 A-INIT SECTION.                                                          
021900                                                                          
022000     OPEN INPUT  W01160                                                   
022100                 W01161                                                   
022200                 W01168                                                   
022400                 W01173                                                   
022500                 W01174                                                   
022600                                                                          
022700     OPEN OUTPUT W01172                                                   
022800     SKIP2                                                                
022900     ACCEPT DAGENS-DATUM  FROM DATE                                       
023000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
023400                                                                          
023500 B-LAES-INFILER SECTION.                                                  
023600                                                                          
023700     PERFORM S01-LAES-W01160                                              
023800     PERFORM S02-LAES-W01161                                              
023900     PERFORM S03-LAES-W01168                                              
024100     PERFORM S05-LAES-W01173                                              
024200     PERFORM S06-LAES-W01174                                              
024300     .                                                                    
024400     EJECT                                                                
024500                                                                          
024600                                                                          
024700 C-NOLLSTALL-DAGLAGERBAND SECTION.                                        
024800                                                                          
024900     INITIALIZE DLB-AREA                                                  
025000     MOVE ZERO         TO WS-KDFORP                                       
025100     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400                                                                          
025500 D-BEHANDLA-W01160 SECTION.                                               
025600                                                                          
025700     MOVE W01160-CLAG-IDARTNR      TO DLB-IDARTNR                         
025800                                                                          
025900     MOVE W01160-CLAG-ADGANG       TO DLB-ADGANG                          
026000     MOVE W01160-CLAG-ADLAGOMR     TO DLB-ADLAGOMR                        
026100     MOVE W01160-CLAG-ADPLATS      TO DLB-ADPLATS                         
026200                                                                          
026300     MOVE W01160-CLAG-BEFT         TO DLB-BEFT                            
026400                                                                          
026500     PERFORM DA-FLYTTA-FLAGGOR                                            
026600     PERFORM DB-FLYTTA-ID                                                 
026700     PERFORM DC-FLYTTA-KODER                                              
026800     PERFORM DD-FLYTTA-KVANTITETER                                        
026900     PERFORM DE-FLYTTA-PRISER                                             
027000                                                                          
027100     MOVE W01160-CLAG-REKSIFFR     TO DLB-REKSIFFR                        
027200     MOVE W01160-CLAG-RESLJUST     TO DLB-RESLJUST                        
027300                                                                          
027400     PERFORM DF-FLYTTA-TIDER                                              
027500     MOVE W01160-CLAG-VKART        TO DLB-VKART                           
027600     MOVE W01160-CLAG-VLARTNTO     TO DLB-VLARTNTO                        
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000                                                                          
028100 DA-FLYTTA-FLAGGOR SECTION.                                               
028200                                                                          
028300     MOVE W01160-CLAG-FLAVRART     TO DLB-FLAVRART                        
028400     MOVE W01160-CLAG-FLIART       TO DLB-FLIART                          
028500     MOVE W01160-CLAG-FLLSRDEL     TO DLB-FLLSRDEL                        
028600     MOVE W01160-CLAG-FLLTKSP      TO DLB-FLLTKSP                         
028700     MOVE W01160-CLAG-FLMANAT      TO DLB-FLMANAT                         
028800     MOVE W01160-CLAG-FLMANBK      TO DLB-FLMANBK                         
028900     MOVE W01160-CLAG-FLMANKP      TO DLB-FLMANKP                         
029000     MOVE W01160-CLAG-FLMANLT      TO DLB-FLMANLT                         
029100     MOVE W01160-CLAG-FLMANPB      TO DLB-FLMANPB                         
029200     MOVE W01160-CLAG-FLMANQ       TO DLB-FLMANQ                          
029300     MOVE W01160-CLAG-FLTOPP       TO DLB-FLTOPP                          
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700                                                                          
029800 DB-FLYTTA-ID SECTION.                                                    
029900                                                                          
030000     MOVE W01160-CLAG-IDANSK           TO DLB-IDANSK                      
030100     MOVE W01160-CLAG-IDARTNR-EMBQ0    TO DLB-IDARTNR-EMBQ0               
030200     MOVE W01160-CLAG-IDARTNR-EMBQ1    TO DLB-IDARTNR-EMBQ1               
030300     MOVE W01160-CLAG-IDARTNR-EMBQ2    TO DLB-IDARTNR-EMBQ2               
030400     MOVE W01160-CLAG-IDARTNR-EMBQ3    TO DLB-IDARTNR-EMBQ3               
030500     MOVE W01160-CLAG-IDARTNR-EMBQ4    TO DLB-IDARTNR-EMBQ4               
030600     MOVE W01160-CLAG-IDBERED          TO DLB-IDBERED                     
030700     MOVE W01160-CLAG-IDFKNGRP         TO DLB-IDFKNGRP                    
030800     MOVE W01160-CLAG-IDINK            TO DLB-IDINK                       
030900     MOVE W01160-CLAG-IDLEVNR          TO DLB-IDLEVNR                     
031000     MOVE W01160-CLAG-IDLKTO           TO DLB-IDLKTO                      
031100     MOVE W01160-CLAG-IDPLANGR-AG      TO DLB-IDPLANGR-AG                 
031200     MOVE W01160-CLAG-IDPLANGR-LEV     TO DLB-IDPLANGR-LEV                
031300     MOVE W01160-CLAG-IDPROJ           TO DLB-IDPROJ                      
031400     MOVE W01160-CLAG-IDPROJUP        TO DLB-IDPROJUP                     
031500     .                                                                    
031600     EJECT                                                                
031700                                                                          
031800                                                                          
031900 DC-FLYTTA-KODER SECTION.                                                 
032000                                                                          
032100     MOVE W01160-CLAG-KDARTHNT     TO DLB-KDARTHNT                        
032200     MOVE W01160-CLAG-KDARTURS     TO DLB-KDARTURS                        
032300     MOVE W01160-CLAG-KDAVT        TO DLB-KDAVT                           
032400     MOVE W01160-CLAG-KDBPSR       TO DLB-KDBPSR                          
032500     MOVE W01160-CLAG-KDEMBKOD-0   TO DLB-KDEMBKOD-0                      
032600     MOVE W01160-CLAG-KDEMBKOD-1   TO DLB-KDEMBKOD-1                      
032700     MOVE W01160-CLAG-KDEMBKOD-2   TO DLB-KDEMBKOD-2                      
032800     MOVE W01160-CLAG-KDERS        TO DLB-KDERS                           
032900     MOVE W01160-CLAG-KDFARLIG     TO DLB-KDFARLIG                        
033000     MOVE W01160-CLAG-KDGK         TO DLB-KDGK                            
033100     MOVE W01160-CLAG-KDHF         TO DLB-KDHF                            
033200     MOVE W01160-CLAG-KDKG         TO DLB-KDKG                            
033300     MOVE W01160-CLAG-KDKSP        TO DLB-KDKSP                           
033400     MOVE W01160-CLAG-KDLEVSP      TO DLB-KDLEVSP                         
033500     MOVE W01160-CLAG-KDLTK        TO DLB-KDLTK                           
033600     MOVE W01160-CLAG-KDLPSP       TO DLB-KDLPSP                          
033700     MOVE W01160-CLAG-KDPRODSL     TO DLB-KDPRODSL                        
033800     MOVE W01160-CLAG-KDSORT       TO DLB-KDSORT                          
033900     MOVE W01160-CLAG-KDSRA        TO DLB-KDSRA                           
034000     MOVE W01160-CLAG-KDTIPPR      TO DLB-KDTIPPR                         
034100     MOVE W01160-CLAG-KDUART       TO DLB-KDUART                          
034200     MOVE W01160-CLAG-KDVSOP       TO DLB-KDVSOP                          
034300     MOVE W01160-CLAG-KDVTH        TO DLB-KDVTH                           
034400     MOVE W01160-CLAG-KDVVKL       TO DLB-KDVVKL                          
034500     MOVE W01160-CLAG-KDYTBEH      TO DLB-KDYTBEH                         
034600                                                                          
034700     MOVE W01160-CLAG-KDFORP       TO WS-KDFORP                           
034800     MOVE WS-KDFORP                TO DLB-KDFORP                          
034900     .                                                                    
035000     EJECT                                                                
035100                                                                          
035200                                                                          
035300 DD-FLYTTA-KVANTITETER SECTION.                                           
035400                                                                          
035500     MOVE W01160-CLAG-KVAKS-CDC    TO DLB-KVAKS-CDC                       
035600     MOVE W01160-CLAG-KVAKS-PAV    TO DLB-KVAKS-PAV                       
035700     MOVE W01160-CLAG-KVAKS-T      TO DLB-KVAKS-T                         
035800     MOVE W01160-CLAG-KVBK         TO DLB-KVBK                            
035900     MOVE W01160-CLAG-KVEFRS       TO DLB-KVEFRS                          
036000     MOVE W01160-CLAG-KVKP         TO DLB-KVKP                            
036100     MOVE W01160-CLAG-KVLAAN       TO DLB-KVLAAN                          
036200     MOVE W01160-CLAG-KVLS         TO DLB-KVLS                            
036300     MOVE W01160-CLAG-KVMP         TO DLB-KVMP                            
036400     MOVE W01160-CLAG-KVOVERF      TO DLB-KVOVERF                         
036500     MOVE W01160-CLAG-KVPALL       TO DLB-KVPALL                          
036600     MOVE W01160-CLAG-KVPB-SATS    TO DLB-KVPB-SATS                       
036700     MOVE W01160-CLAG-KVPB-SEP     TO DLB-KVPB-SEP                        
036800     MOVE W01160-CLAG-KVQ          TO DLB-KVQ                             
036900     MOVE W01160-CLAG-KVQPACK-0    TO DLB-KVQPACK-0                       
037000     MOVE W01160-CLAG-KVQPACK-1    TO DLB-KVQPACK-1                       
037100     MOVE W01160-CLAG-KVQPACK-2    TO DLB-KVQPACK-2                       
037200     MOVE W01160-CLAG-KVQPACK-3    TO DLB-KVQPACK-3                       
037300     MOVE W01160-CLAG-KVQPACK-4    TO DLB-KVQPACK-4                       
037400     MOVE W01160-CLAG-KVRESS       TO DLB-KVRESS                          
037500     MOVE W01160-CLAG-KVROS        TO DLB-KVROS                           
037600     MOVE W01160-CLAG-KVSLAGER     TO DLB-KVSLAGER                        
037700     MOVE W01160-CLAG-KVSLUTKP     TO DLB-KVSLUTKP                        
037800     MOVE W01160-CLAG-KVSPANT      TO DLB-KVSPANT                         
037900     MOVE W01160-CLAG-KVUTRS       TO DLB-KVUTRS                          
038000     MOVE W01160-CLAG-KVVECKOR-AT  TO DLB-KVVECKOR-AT                     
038100     MOVE W01160-CLAG-KVVECKOR-BT  TO DLB-KVVECKOR-BT                     
038200     MOVE W01160-CLAG-KVVECKOR-FT  TO DLB-KVVECKOR-FT                     
038300     MOVE W01160-CLAG-KVVECKOR-LT  TO DLB-KVVECKOR-LT                     
038400     .                                                                    
038500     EJECT                                                                
038600                                                                          
038700                                                                          
038800 DE-FLYTTA-PRISER SECTION.                                                
038900                                                                          
039000     MOVE W01160-CLAG-PRARTSTD     TO DLB-PRARTBES                        
039200     MOVE W01160-CLAG-PRARTSJK     TO DLB-PRARTSJK                        
039300     MOVE W01160-CLAG-PRARTSTD     TO DLB-PRARTSTD                        
039400     MOVE W01160-CLAG-PRINK        TO DLB-PRINK                           
039410     MOVE 1                        TO DLB-KDPRTILL                        
039420     MOVE 0                        TO DLB-FLSPECPR                        
039430     MOVE W01160-CLAG-PRARTBTO-EXP TO DLB-PRARTBTO-EXP                    
039500     .                                                                    
039600     EJECT                                                                
039700                                                                          
039800                                                                          
039900 DF-FLYTTA-TIDER SECTION.                                                 
040000                                                                          
040100     MOVE W01160-CLAG-TIERSDAT     TO DLB-TIERSDAT                        
040200     MOVE W01160-CLAG-TIFINLV      TO DLB-TIFINLV                         
040300     MOVE W01160-CLAG-TIINVDAT     TO DLB-TIINVDAT                        
040400     MOVE W01160-CLAG-TILPSP       TO DLB-TILPSP                          
040500     MOVE W01160-CLAG-TIREGDAT     TO DLB-TIREGDAT                        
040600     MOVE W01160-CLAG-TIRODAT      TO DLB-TIRODAT                         
040700     MOVE W01160-CLAG-TISLJUST     TO DLB-TISLJUST                        
040800     MOVE W01160-CLAG-TIURPROD     TO DLB-TIURPROD                        
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200                                                                          
041300 E-BEHANDLA-W01161 SECTION.                                               
041400                                                                          
041500     IF W01161-PRL-IDARTNR = WS-SPAR-IDARTNR                              
041600        PERFORM EA-FLYTTA-W01161                                          
041700        PERFORM UNTIL W01161-PRL-IDARTNR > WS-SPAR-IDARTNR                
041800           PERFORM S02-LAES-W01161                                        
041900        END-PERFORM                                                       
042000     ELSE                                                                 
042100        IF W01161-PRL-IDARTNR > WS-SPAR-IDARTNR                           
042200           CONTINUE                                                       
042300        ELSE                                                              
042310           PERFORM UNTIL W01161-PRL-IDARTNR > WS-SPAR-IDARTNR             
042320              IF W01161-PRL-IDARTNR = WS-SPAR-IDARTNR                     
042330                 PERFORM EA-FLYTTA-W01161                                 
042340              END-IF                                                      
042350              PERFORM S02-LAES-W01161                                     
042360            END-PERFORM                                                   
042900        END-IF                                                            
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400                                                                          
043500 EA-FLYTTA-W01161 SECTION.                                                
043600                                                                          
043700     MOVE W01161-PRL-PRARTBEL-PR   TO DLB-PRARTBEL-PR                     
043800     MOVE W01161-PRL-PRARTBES-PR   TO DLB-PRARTBES-PR                     
043810     MOVE W01161-PRL-PRARTBES-PR   TO DLB-PRARTBES                        
043900     .                                                                    
044000     EJECT                                                                
044100                                                                          
044200                                                                          
044300 F-BEHANDLA-W01168 SECTION.                                               
044400                                                                          
044500     IF W01168-ART-IDARTNR = WS-SPAR-IDARTNR                              
044600        PERFORM FA-FLYTTA-W01168                                          
044700        PERFORM S03-LAES-W01168                                           
044800     ELSE                                                                 
044900        IF W01168-ART-IDARTNR > WS-SPAR-IDARTNR                           
045000           CONTINUE                                                       
045100        ELSE                                                              
045110           PERFORM UNTIL W01168-ART-IDARTNR > WS-SPAR-IDARTNR             
045120              IF W01168-ART-IDARTNR = WS-SPAR-IDARTNR                     
045130                 PERFORM FA-FLYTTA-W01168                                 
045140              END-IF                                                      
045150              PERFORM S03-LAES-W01168                                     
045160            END-PERFORM                                                   
045700        END-IF                                                            
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200                                                                          
046300 FA-FLYTTA-W01168 SECTION.                                                
046400                                                                          
046500     MOVE W01168-ART-SUTPO-TOT TO DLB-SUTPO-TOT                           
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900                                                                          
050400 H-BEHANDLA-W01173 SECTION.                                               
050500                                                                          
050600     IF W01173-IDARTNR = WS-SPAR-IDARTNR                                  
050700        PERFORM HA-FLYTTA-W01173                                          
050800        PERFORM S05-LAES-W01173                                           
050900     ELSE                                                                 
051000        IF W01173-IDARTNR > WS-SPAR-IDARTNR                               
051100           CONTINUE                                                       
051200        ELSE                                                              
051210           PERFORM UNTIL W01173-IDARTNR > WS-SPAR-IDARTNR                 
051220              IF W01173-IDARTNR = WS-SPAR-IDARTNR                         
051230                 PERFORM HA-FLYTTA-W01173                                 
051240              END-IF                                                      
051250              PERFORM S05-LAES-W01173                                     
051260            END-PERFORM                                                   
051800        END-IF                                                            
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200                                                                          
052300                                                                          
052400 HA-FLYTTA-W01173 SECTION.                                                
052500                                                                          
052600     MOVE W01173-KVBR-TOT    TO DLB-KVBR-TOT                              
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000                                                                          
053100 I-BEHANDLA-W01174 SECTION.                                               
053200                                                                          
053300     IF W01174-IDARTNR = WS-SPAR-IDARTNR                                  
053400        PERFORM IA-FLYTTA-W01174                                          
053500        PERFORM S06-LAES-W01174                                           
053600     ELSE                                                                 
053700        IF W01174-IDARTNR > WS-SPAR-IDARTNR                               
053800           CONTINUE                                                       
053900        ELSE                                                              
053950           PERFORM UNTIL W01174-IDARTNR > WS-SPAR-IDARTNR                 
053960              IF W01174-IDARTNR = WS-SPAR-IDARTNR                         
053970                 PERFORM IA-FLYTTA-W01174                                 
053980              END-IF                                                      
053990              PERFORM S06-LAES-W01174                                     
053991            END-PERFORM                                                   
054500        END-IF                                                            
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000                                                                          
055100 IA-FLYTTA-W01174 SECTION.                                                
055200                                                                          
055300     MOVE +1 TO IX                                                        
055400     PERFORM UNTIL IX > 10                                                
055500        EVALUATE W01174-IDSKYLT(IX)                                       
055600           WHEN 'GB '                                                     
055700           MOVE W01174-BEART(IX) TO DLB-BEART-ENG                         
055800           WHEN 'F  '                                                     
055900           MOVE W01174-BEART(IX) TO DLB-BEART-FRA                         
056000           WHEN 'E  '                                                     
056100           MOVE W01174-BEART(IX) TO DLB-BEART-SPA                         
056200           WHEN 'S  '                                                     
056300           MOVE W01174-BEART(IX) TO DLB-BEART-SVE                         
056400           WHEN 'D  '                                                     
056500           MOVE W01174-BEART(IX) TO DLB-BEART-TYS                         
056600        END-EVALUATE                                                      
056700        ADD +1 TO IX                                                      
056800     END-PERFORM                                                          
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200                                                                          
057300 Z-FINIT SECTION.                                                         
057400     CLOSE W01160                                                         
057500           W01161                                                         
057600           W01168                                                         
057800           W01173                                                         
057900           W01174                                                         
058000           W01172                                                         
058100     SKIP2                                                                
058200     MOVE 'S' TO POSTSUM-OPKOD                                            
058300     CALL POSTSUM USING POSTSUM-PARM                                      
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700                                                                          
058800 S01-LAES-W01160  SECTION.                                                
058900     READ W01160 INTO W01160-AREA                                         
059000     AT END                                                               
059100        SET END-OF-W01160 TO TRUE                                         
059200                                                                          
059300     NOT AT END                                                           
059400        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
059500        MOVE 'W01172D1' TO POSTSUM-DDNAMN2                                
059600        CALL POSTSUM USING POSTSUM-PARM                                   
059700     END-READ                                                             
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100                                                                          
060200 S02-LAES-W01161  SECTION.                                                
060300     READ W01161 INTO W01161-AREA                                         
060400     AT END                                                               
060500        MOVE +99999999  TO W01161-PRL-IDARTNR                             
060600        SET END-OF-W01161 TO TRUE                                         
060700                                                                          
060800     NOT AT END                                                           
060900        MOVE 'W01161' TO POSTSUM-FDNAMN                                   
061000        MOVE 'W01172D2' TO POSTSUM-DDNAMN2                                
061100        CALL POSTSUM USING POSTSUM-PARM                                   
061200     END-READ                                                             
061300     .                                                                    
061400     EJECT                                                                
061500                                                                          
061600                                                                          
061700 S03-LAES-W01168  SECTION.                                                
061800     READ W01168 INTO W01168-AREA                                         
061900     AT END                                                               
062000        MOVE +99999999  TO W01168-ART-IDARTNR                             
062100        SET END-OF-W01168 TO TRUE                                         
062200                                                                          
062300     NOT AT END                                                           
062400        MOVE 'W01168' TO POSTSUM-FDNAMN                                   
062500        MOVE 'W01172D3' TO POSTSUM-DDNAMN2                                
062600        CALL POSTSUM USING POSTSUM-PARM                                   
062700     END-READ                                                             
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100                                                                          
064700 S05-LAES-W01173  SECTION.                                                
064800     READ W01173 INTO W01173-AREA                                         
064900     AT END                                                               
065000        MOVE +99999999  TO W01173-IDARTNR                                 
065100        SET END-OF-W01173 TO TRUE                                         
065200                                                                          
065300     NOT AT END                                                           
065400        MOVE 'W01173' TO POSTSUM-FDNAMN                                   
065500        MOVE 'W01172D5' TO POSTSUM-DDNAMN2                                
065600        CALL POSTSUM USING POSTSUM-PARM                                   
065700     END-READ                                                             
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100                                                                          
066200 S06-LAES-W01174  SECTION.                                                
066300     READ W01174 INTO W01174-AREA                                         
066400     AT END                                                               
066500        MOVE +99999999  TO W01174-IDARTNR                                 
066600        SET END-OF-W01174 TO TRUE                                         
066700                                                                          
066800     NOT AT END                                                           
066900        MOVE 'W01174' TO POSTSUM-FDNAMN                                   
067000        MOVE 'W01172D6' TO POSTSUM-DDNAMN2                                
067100        CALL POSTSUM USING POSTSUM-PARM                                   
067200     END-READ                                                             
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600                                                                          
067700 S11-SKRIV-DLB SECTION.                                                   
067800                                                                          
067900     WRITE W01172-POST FROM DLB-AREA                                      
068000                                                                          
068100     MOVE 'W01172' TO POSTSUM-FDNAMN                                      
068200     MOVE 'W01172D7' TO POSTSUM-DDNAMN2                                   
068300     CALL POSTSUM USING POSTSUM-PARM                                      
068400     .                                                                    
068500     EJECT                                                                
068600                                                                          
068700                                                                          
068800*S99-ABEND SECTION.                                                       
068900*                                                                         
069000*    SKIP2                                                                
069100*    MOVE 'S' TO POSTSUM-OPKOD                                            
069200*    CALL POSTSUM USING POSTSUM-PARM                                      
069300*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
069400*    .                                                                    
