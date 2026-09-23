000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4031100.                                                
000400*AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500*DATE-WRITTEN.   91/06/24.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BYTER PACKARE PÅ EN ORDERDEL (EJ FLERA).              
001100*        ENDAST ORDERDELAR MED IDUSER = 00000000 FÅR ÄNDRAS.              
001200*                                                                         
001300*        PROGRAMMET UPPATERAR WLORQA (WDQ3)                               
001400*                             WDE4  VIA WDE4A                             
001500*                             WDR4 (ACTION TRANSACTION 4487)              
001600*                             WLXXDJ (WDG2) LÅSNINGSREGISTER              
001700*        PROGRAMMET LÄSER     WDE6                                        
001800*                             WLXXKH (WDR1) PRODKANALTABELL               
001900*                                                                         
002000* CHANGE LOG                                                              
002100*                                                                         
002200* DIGAMBAR/021011                                                         
002300* STRUCURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE               
002400* THE RESPONSE TIME OF THE SCREEN 4312                                    
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T311                                              
002800*        MID:         W4I31101                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W4O31101                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4031100'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  RKOD-ABEND-MED-DUMP         PIC S9(9)  VALUE +33   COMP SYNC.        
005000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +250  COMP SYNC.        
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300 77  WS-IDANSTNR                 PIC X(5)    VALUE SPACE.                 
005400 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005600 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
005700 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
005800                                                                          
005900 77  WS-IDANSTNR-NUM             PIC 9(5)    VALUE ZERO.                  
006000 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
006100 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
006200 77  WS-IDKOLLI-NUM              PIC 9(5)    VALUE ZERO.                  
006300 77  WS-IDPRODNR-NUM             PIC 9(7)    VALUE ZERO.                  
006400                                                                          
006500 77  WS-IDUSER-NUM               PIC 9(8)    VALUE ZERO.                  
006600 77  WS-IDPLKLST                 PIC X(3)    VALUE SPACE.                 
006700 77  WS-IDPLKLST-NUM             PIC 9(3)    VALUE ZERO.                  
006800 77  WS-IDPURAD-FOM              PIC 9(5)    VALUE ZERO.                  
006900 77  WS-IDPURAD-TOM              PIC 9(5)    VALUE ZERO.                  
007000 77  WS-IDORDNR5-X               PIC X(5)    VALUE SPACE.                 
007100 77  WS-KVORDRAD-SPAR            PIC 9(5)    VALUE ZERO.                  
007200 77  WS-IDRADNR-FOM-NUM          PIC 9(4)    VALUE ZERO.                  
007300 77  WS-IDRADNR-TOM-NUM          PIC 9(4)    VALUE ZERO.                  
007400                                                                          
007500 77  WS-IDDISTR-SPAR             PIC 9(4)    VALUE ZERO.                  
007600 77  WS-IDKUNDNR-SPAR            PIC 9(6)    VALUE ZERO.                  
007700 77  WS-IDKUNDRF-SPAR            PIC X(10).                               
007800 77  WS-IDPRODNR-SPAR            PIC 9(7).                                
007900 77  WS-IDPLKLST-SPAR            PIC 9(3).                                
008000 77  WS-IDDC                     PIC X(2).                                
008100 77  WS-IDDC-NUM                 PIC S9(2)    VALUE +0.                   
008200                                                                          
008300 01  WS-IDKUNDRF                 PIC X(10).                               
008400 01  FILLER REDEFINES WS-IDKUNDRF.                                        
008500     03  WS-IDORDNR5             PIC 9(5).                                
008600     03  FILLER                  PIC X(5).                                
008700                                                                          
008800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008900     88  INDATA-OK                           VALUE 'J'.                   
009000     88  INDATA-FEL                          VALUE 'N'.                   
009100                                                                          
009200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009300     88  NYCKLAR-OK                          VALUE 'J'.                   
009400     88  NYCKLAR-FEL                         VALUE 'N'.                   
009500                                                                          
009600 77  DISTRIKT-SW                 PIC X       VALUE 'N'.                   
009700     88  DISTR-IFYLLT                        VALUE 'J'.                   
009800                                                                          
009900 77  PRODNR-SW                   PIC X       VALUE 'N'.                   
010000     88  PRODNR-IFYLLT                       VALUE 'J'.                   
010100                                                                          
010200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010300     88  ALLT-OK                             VALUE 'J'.                   
010400                                                                          
010500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010600     88  EGEN-MID                            VALUE '4311'.                
010700     88  GODK-MID                            VALUE '4311' '4312'          
010800                                                          '4314'          
010900                                                   '4315' '4316'          
011000                                                   '4317' '4318'          
011100                                                   '4319'.                
011200     EJECT                                                                
011300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011400 01  GENERELLA-SUBPROGRAM.                                                
011500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012100*01 -COPY WMEDAREA                                                        
012200     SKIP3                                                                
012300 01  MESSAGE-CODES.                                                       
012400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012500     03  ERR-OTILLATEN-UPPDAT    PIC X(3)    VALUE '007'.                 
012600     03  ERR-EJ-NUMERISKT        PIC X(3)    VALUE '020'.                 
012700     03  ERR-EJ-BADE-PLKLST-INTERVALL                                     
012800                                 PIC X(3)    VALUE '098'.                 
012900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013000     03  ERR-RAD-FEL             PIC X(40)   VALUE                        
013100         'FYLL I RAD FROM OCH RAD TOM'.                                   
013200     03  ERR-PLKLST-FEL          PIC X(40)   VALUE                        
013300         'FYLL I PLOCKLISTA          '.                                   
013400     03  ERR-INF-SAKNAS          PIC X(3)    VALUE '413'.                 
013500     03  ERR-ORDERN-SAKNAS       PIC X(3)    VALUE '701'.                 
013600     03  ERR-PACKNING-PAGAR      PIC X(3)    VALUE '702'.                 
013700     03  ERR-INTERVALL-UTDELAT   PIC X(3)    VALUE '707'.                 
013800     03  ERR-BADE-FOM-OCH-TOM    PIC X(3)    VALUE '739'.                 
013900     03  INF-ORDERN-DELAD        PIC X(3)    VALUE '782'.                 
014000     EJECT                                                                
014100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014400     SKIP3                                                                
014500*01  MID -COPY W4I31101    -PRE MID-                                      
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014800     SKIP3                                                                
014900*01  -COPY WMSGAREA                                                       
015000     EJECT                                                                
015100     03  MOD REDEFINES MSG-AREA.                                          
015200*      05  -COPY W4O31101  -PRE MOD-                                      
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015500     SKIP3                                                                
015600*01  -COPY WMFSAREA                                                       
015700     EJECT                                                                
015800*    --- ARBETS-AREOR                                                     
015900 01  FILLER                      PIC X(16)   VALUE 'WWDIST19'.            
016000     SKIP3                                                                
016100 01  TEST-IDDISTR                PIC 9(05)   VALUE ZERO COMP-3.           
016200 01  FILLER REDEFINES TEST-IDDISTR.                                       
016300*    03  -COPY WWDIST19.                                                  
016400     EJECT                                                                
016500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016900     SKIP3                                                                
017000 01  NYCKLAR-TILL-DLI.                                                    
017100                                                                          
017200*-- TILL WDQ301 DIREKT.                                                   
017300     03  W-WDQ301KY-X.                                                    
017400         05  W-IDORDER-WDQ3      PIC S9(7)   VALUE ZERO COMP-3.           
017500         05  W-IDDC-WDQ3         PIC X(2).                                
017600         05  W-IDPRODNR-WDQ3     PIC S9(7)   VALUE ZERO COMP-3.           
017700         05  W-IDPLKLST-WDQ3     PIC S9(3)   VALUE ZERO COMP-3.           
017800                                                                          
017900*-- TILL WDE601 DIREKT.                                                   
018000     03  W-IDPRODNR-X.                                                    
018100         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
018200                                                                          
018300*-- TILL WDE401  DIREKT                                                   
018400     03  W-WDE4KEY-X.                                                     
018500         05  W-IDDISTR-WDE401    PIC S9(5)   VALUE ZERO COMP-3.           
018600         05  W-IDKUNDNR-WDE401   PIC S9(7)   VALUE ZERO COMP-3.           
018700         05  W-IDKUNDRF-WDE401   PIC  X(10)  VALUE SPACE.                 
018800         05  W-IDPRODNR-WDE401   PIC S9(7)   VALUE ZERO COMP-3.           
018900         05  W-IDPLKLST-WDE401   PIC S9(3)   VALUE ZERO COMP-3.           
019000                                                                          
019100                                                                          
019200*-- TILL WDE4A1  DIREKT                                                   
019300     03  W-WDE4A1KY-MIN-X.                                                
019400         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
019500         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
019600         05  W-IDKUNDRF-MIN      PIC  X(10)  VALUE SPACE.                 
019700         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
019800         05  W-IDPLKLST-MIN      PIC S9(3)   VALUE ZERO COMP-3.           
019900                                                                          
020000     03  W-WDE4A1KY-MAX-X.                                                
020100         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
020200         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
020300         05  W-IDKUNDRF-MAX      PIC  X(10)  VALUE SPACE.                 
020400         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
020500         05  W-IDPLKLST-MAX      PIC S9(3)   VALUE ZERO COMP-3.           
020600                                                                          
020700*-- TILL WDE401 VIA WDE4A                                                 
020800     03  W-IDGMTREF-MIN-X.                                                
020900         05  W-IDDISTR-IDGM-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
021000         05  W-IDKUNDNR-IDGM-MIN PIC S9(7)   VALUE ZERO COMP-3.           
021100         05  W-IDKUNDRF-IDGM-MIN PIC  X(10)  VALUE SPACE.                 
021200                                                                          
021300     03  W-IDGMTREF-MAX-X.                                                
021400         05  W-IDDISTR-IDGM-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
021500         05  W-IDKUNDNR-IDGM-MAX PIC S9(7)   VALUE ZERO COMP-3.           
021600         05  W-IDKUNDRF-IDGM-MAX PIC  X(10)  VALUE SPACE.                 
021700                                                                          
021800*-- TILL WDE411 DIREKT                                                    
021900     03  W-IDPURAD-X.                                                     
022000         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
022100                                                                          
022200*-- TILL WDR1   IDHTYP 4447                                               
022300     03  W-4447-IDHTYP-X.                                                 
022400         05  W-4447-IDHTYP       PIC  X(4)    VALUE '4447'.               
022500         05  W-4447-IDDC         PIC  X(2).                               
022600         05  W-4447-LOW-VALUE    PIC  X(24)   VALUE LOW-VALUE.            
022700                                                                          
022800     03  W-IDPRC-X.                                                       
022900         05  W-4448-IDPRC.                                                
023000           07 W-4448-IDPRCBAS     PIC X(3)    VALUE SPACE.                
023100           07 W-4448-IDPRCVAR     PIC X       VALUE SPACE.                
023200         05  W-4448-LOW-VALUE     PIC X       VALUE LOW-VALUE.            
023300                                                                          
023400*-- TILL WDR4   IDHTYP 4487                                               
023500     03  W-4487-IDHTYP-X.                                                 
023600         05  W-4487-IDHTYP       PIC  X(4)    VALUE '4487'.               
023700         05  W-4487-IDDC         PIC  X(2).                               
023800         05  W-4487-LOW-VALUE    PIC  X(24)   VALUE LOW-VALUE.            
023900                                                                          
024000     03  W-4488-IDHTYP-X.                                                 
024100         05  W-4488-KDPRCGRP     PIC  X(5)    VALUE SPACE.                
024200                                                                          
024300     03  W-4490-IDHTYP-X.                                                 
024400         05  W-4490-DARFS        PIC  9(12)   VALUE ZERO.                 
024500         05  W-4490-IDPRODNR     PIC  S9(7)   VALUE ZERO COMP-3.          
024600         05  W-4490-IDPLKLST     PIC  S9(3)   VALUE ZERO COMP-3.          
024700                                                                          
024800*-- TILL WDG2   IDHTYP 4305  LÅSNINGSREGISTER                             
024900     03  W-4305-IDHTYP-X.                                                 
025000         05  W-4305-IDHTYP       PIC  X(4)    VALUE '4305'.               
025100         05  W-4305-IDDC         PIC  X(2).                               
025200         05  W-4305-LOW-VALUE    PIC  X(24)   VALUE LOW-VALUE.            
025300                                                                          
025400     03  W-4306-IDHTYP-X.                                                 
025500         05  W-4306-IDPRODNR     PIC  S9(7)   VALUE ZERO COMP-3.          
025600         05  W-4306-LOW-VALUE    PIC   X(6)   VALUE LOW-VALUE.            
025700                                                                          
025800     SKIP2                                                                
025900*    --- STATUS-KOD FRÅN IMS                                              
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FINNS                       VALUE '  '.                  
026200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026400     88  BASEN-SLUT                          VALUE 'GB'.                  
026500     SKIP2                                                                
026600 01  GODK-STATUSKODER.                                                    
026700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026800     SKIP3                                                                
026900 01  SSA1                        PIC X(96).                               
027000 01  SSA2                        PIC X(64).                               
027100 01  SSA3                        PIC X(64).                               
027200     EJECT                                                                
027300*    --- IMS FUNKTIONSKODER                                               
027400*01  -COPY W0003                                                          
027500     EJECT                                                                
027600*    ---  DLI INPUT-OUTPUT AREA                                           
027700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027800     SKIP3                                                                
027900 01  DLI-IO-AREA.                                                         
028000     03  IO-AREA                 PIC X(416)  VALUE SPACE.                 
028100     SKIP3                                                                
028200     03  WLORQA01 REDEFINES IO-AREA.                                      
028300*        05  -COPY WDQ301                                                 
028400     EJECT                                                                
028500     03  WDE4A01 REDEFINES IO-AREA.                                       
028600*        05  -COPY WDE4A1                                                 
028700     EJECT                                                                
028800     03  WDE401 REDEFINES IO-AREA.                                        
028900*        05  -COPY WDE401                                                 
029000     EJECT                                                                
029100     03  WDE411 REDEFINES IO-AREA.                                        
029200*        05  -COPY WDE411                                                 
029300     EJECT                                                                
029400     03  WDE601 REDEFINES IO-AREA.                                        
029500*        05  -COPY WDE601                                                 
029600     EJECT                                                                
029700     03  4487-AREA REDEFINES IO-AREA.                                     
029800*        05  -COPY WDGX4487                                               
029900     SKIP3                                                                
030000     03  WDGX4490 REDEFINES IO-AREA.                                      
030100*        05  -COPY WDGX4490                                               
030200     SKIP3                                                                
030300     03  WL448711 REDEFINES IO-AREA.                                      
030400*        05  -COPY WDGX4488                                               
030500     EJECT                                                                
030600     03  WLXXKH01 REDEFINES IO-AREA.                                      
030700*        05  -COPY WDGX4447                                               
030800     SKIP3                                                                
030900     03  WLXXKH11 REDEFINES IO-AREA.                                      
031000*        05  -COPY WDGX4448                                               
031100     EJECT                                                                
031200     03  WLXXDJ01 REDEFINES IO-AREA.                                      
031300*        05  -COPY WDGX4305                                               
031400     SKIP3                                                                
031500     03  WLXXDJ11 REDEFINES IO-AREA.                                      
031600*        05  -COPY WDGX4306                                               
031700     EJECT                                                                
031800 LINKAGE SECTION.                                                         
031900                                                                          
032000*01  -COPY W0009   -PRE MSG-                                              
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE WDE401-                                            
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE WDE4A-                                             
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE WDE4-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE WDE6-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE ORQA-                                              
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01  -COPY W0008  -PRE 4487-                                              
033800     05  FILLER                  PIC X.                                   
033900     EJECT                                                                
034000*01  -COPY W0008  -PRE XXKH-                                              
034100     05  FILLER                  PIC X.                                   
034200     EJECT                                                                
034300*01  -COPY W0008  -PRE XXDJ-                                              
034400     05  FILLER                  PIC X.                                   
034500     EJECT                                                                
034600 PROCEDURE DIVISION  USING MSG-PCB WDE401-PCB WDE4A-PCB WDE4-PCB          
034700                 WDE6-PCB ORQA-PCB   4487-PCB XXKH-PCB XXDJ-PCB.          
034800     ENTRY 'DLITCBL' USING MSG-PCB WDE401-PCB WDE4A-PCB WDE4-PCB          
034900                 WDE6-PCB ORQA-PCB   4487-PCB XXKH-PCB XXDJ-PCB.          
035000                                                                          
035100     PERFORM IMS-GET-MSG                                                  
035200     IF SEGMENT-FINNS                                                     
035300       PERFORM A-INIT                                                     
035400       PERFORM B-KOLLA-NYCKLAR                                            
035500       IF NYCKLAR-OK                                                      
035600          PERFORM F-LAES-VISA-INFO                                        
035700          IF ALLT-OK                                                      
035800             PERFORM G-KOLLA-INPUT                                        
035900             IF INDATA-OK                                                 
036000               PERFORM H-UPPDATERA                                        
036100             END-IF                                                       
036200          END-IF                                                          
036300       END-IF                                                             
036400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
036500       PERFORM IMS-INSERT-MSG                                             
036600     END-IF                                                               
036700                                                                          
036800     MOVE ZERO TO RETURN-CODE                                             
036900     GOBACK                                                               
037000     .                                                                    
037100     EJECT                                                                
037200 A-INIT SECTION.                                                          
037300                                                                          
037400     IF MSG-DUBBLA-TRANSKODER                                             
037500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I31101                 
037600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
037700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037800     ELSE                                                                 
037900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I31101                  
038000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
038100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038200     END-IF                                                               
038300                                                                          
038400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
038600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038700                                                                          
038800     MOVE LOW-VALUE TO MSG-AREA                                           
038900     MOVE 'W4O311N1' TO MFS-IDMOD                                         
039000     MOVE '4311' TO MOD-IDTRANS                                           
039100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039200                                                                          
039300     IF NOT EGEN-MID                                                      
039400       MOVE SPACE TO MFS-KDTRTYP                                          
039500       MOVE '7' TO MFS-IDPFK                                              
039600     END-IF                                                               
039700                                                                          
039800     IF ENGLISH-TEXT                                                      
039900       MOVE +2 TO SPRAK-IX                                                
040000       MOVE 'B  ' TO MED-IDSKYLT                                          
040100     ELSE                                                                 
040200       MOVE +1 TO SPRAK-IX                                                
040300       MOVE 'S  ' TO MED-IDSKYLT                                          
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 B-KOLLA-NYCKLAR SECTION.                                                 
040800                                                                          
040900     MOVE JA TO NYCKLAR-SW                                                
041000     MOVE NEJ TO DISTRIKT-SW                                              
041100     MOVE NEJ TO PRODNR-SW                                                
041200     MOVE LOW-VALUE               TO W-WDE4A1KY-MIN-X                     
041300                                     W-WDQ301KY-X                         
041400                                     W-IDGMTREF-MIN-X                     
041500     MOVE HIGH-VALUE              TO W-WDE4A1KY-MAX-X                     
041600                                     W-IDGMTREF-MAX-X                     
041700                                                                          
041800     PERFORM BB-KOLLA-IDDISTR                                             
041900     PERFORM BC-KOLLA-IDKUNDNR                                            
042000     PERFORM BD-KOLLA-IDORDNR5                                            
042100     PERFORM BE-KOLLA-IDPRODNR                                            
042200     PERFORM BA-KOLLA-IDANSTNR                                            
042300     PERFORM BF-KOLLA-IDDC                                                
042400                                                                          
042500     IF GODK-MID OR NYCKLAR-OK                                            
042600       MOVE WS-IDANSTNR TO MOD-IDANSTNR-UT                                
042700       INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE            
042800*                                                                         
042900       MOVE WS-IDDISTR  TO MOD-IDDISTR-UT                                 
043000       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
043100*                                                                         
043200       MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                
043300       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
043400*                                                                         
043500       MOVE WS-IDORDNR5-X TO MOD-IDORDNR5-UT                              
043600       INSPECT MOD-IDORDNR5-UT REPLACING LEADING ZERO BY SPACE            
043700*                                                                         
043800       MOVE WS-IDPRODNR TO MOD-IDPRODNR-UT                                
043900       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
044000*                                                                         
044100       MOVE WS-IDDC     TO MOD-IDDC-UT                                    
044200*                                                                         
044300     ELSE                                                                 
044400       MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-UT                            
044500       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
044600       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-UT                            
044700       MOVE MFS-RENSA-FAELT TO MOD-IDORDNR5-UT                            
044800       MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                            
044900       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
045000     END-IF                                                               
045100                                                                          
045200     IF NYCKLAR-FEL                                                       
045300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045400       CALL WMEDKONV USING MED-WMEDAREA                                   
045500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045600       PERFORM MFS-RENSA-FAELT-IN                                         
045700       PERFORM MFS-RENSA-FAELT-UT                                         
045800     END-IF                                                               
045900                                                                          
046000     IF NYCKLAR-OK                                                        
046100        INSPECT MID-IDPRODNR REPLACING LEADING SPACE BY ZERO              
046200        IF MID-IDPRODNR NUMERIC AND MID-IDPRODNR > ZERO                   
046300          CONTINUE                                                        
046400        ELSE                                                              
046500          MOVE '7'      TO MFS-IDPFK                                      
046600          MOVE SPACE    TO MFS-KDTRTYP                                    
046700        END-IF                                                            
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 BA-KOLLA-IDANSTNR SECTION.                                               
047200                                                                          
047300     MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-IN                              
047400                                                                          
047500     IF MID-IDANSTNR-IN = ALL '+'                                         
047600       MOVE MID-IDANSTNR-UT TO WS-IDANSTNR                                
047700       INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO                
047800     ELSE                                                                 
047900       MOVE MID-IDANSTNR-IN TO WS-IDANSTNR                                
048000       MOVE '7'         TO MFS-IDPFK                                      
048100       MOVE SPACE       TO MFS-KDTRTYP                                    
048200     END-IF                                                               
048300                                                                          
048400     IF WS-IDANSTNR NUMERIC AND WS-IDANSTNR > ZERO                        
048500       CONTINUE                                                           
048600     ELSE                                                                 
048700      MOVE NEJ TO NYCKLAR-SW                                              
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 BB-KOLLA-IDDISTR SECTION.                                                
049200                                                                          
049300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
049400                                                                          
049500     IF MID-IDDISTR-IN = ALL '+'                                          
049600       MOVE MID-IDDISTR-UT TO WS-IDDISTR                                  
049700       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
049800     ELSE                                                                 
049900       MOVE MID-IDDISTR-IN TO WS-IDDISTR                                  
050000       MOVE '7'         TO MFS-IDPFK                                      
050100       MOVE SPACE       TO MFS-KDTRTYP                                    
050200     END-IF                                                               
050300                                                                          
050400     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
050500* * * * SATSORDER FÅR EJ GÅ DENNA VÄGEN * * * * * * * * * * * *           
050600       MOVE WS-IDDISTR TO TEST-IDDISTR                                    
050700       IF DIST19-SATS                                                     
050800          MOVE NEJ TO NYCKLAR-SW                                          
050900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
051000       ELSE                                                               
051100          MOVE WS-IDDISTR TO W-IDDISTR-IDGM-MIN                           
051200                             W-IDDISTR-IDGM-MAX                           
051300                             W-IDDISTR-MIN                                
051400                             W-IDDISTR-MAX                                
051500          MOVE JA TO DISTRIKT-SW                                          
051600       END-IF                                                             
051700     ELSE                                                                 
051800       MOVE NEJ TO NYCKLAR-SW                                             
051900     END-IF                                                               
052000     .                                                                    
052100     EJECT                                                                
052200 BC-KOLLA-IDKUNDNR SECTION.                                               
052300                                                                          
052400     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
052500                                                                          
052600     IF MID-IDKUNDNR-IN = ALL '+'                                         
052700       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
052800       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
052900     ELSE                                                                 
053000       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                                
053100       MOVE '7'         TO MFS-IDPFK                                      
053200       MOVE SPACE       TO MFS-KDTRTYP                                    
053300     END-IF                                                               
053400     IF WS-IDKUNDNR NUMERIC                                               
053500       MOVE WS-IDKUNDNR TO W-IDKUNDNR-IDGM-MIN                            
053600                           W-IDKUNDNR-IDGM-MAX                            
053700                           W-IDKUNDNR-MIN                                 
053800                           W-IDKUNDNR-MAX                                 
053900     ELSE                                                                 
054000       MOVE NEJ TO NYCKLAR-SW                                             
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 BD-KOLLA-IDORDNR5 SECTION.                                               
054500                                                                          
054600     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR5-IN                              
054700     MOVE SPACE TO WS-IDKUNDRF                                            
054800                                                                          
054900     IF MID-IDORDNR5-IN = ALL '+'                                         
055000       MOVE MID-IDORDNR5-UT TO WS-IDORDNR5-X                              
055100       INSPECT WS-IDORDNR5-X REPLACING LEADING SPACE BY ZERO              
055200     ELSE                                                                 
055300       MOVE MID-IDORDNR5-IN TO WS-IDORDNR5-X                              
055400       MOVE '7'         TO MFS-IDPFK                                      
055500       MOVE SPACE       TO MFS-KDTRTYP                                    
055600     END-IF                                                               
055700                                                                          
055800     IF WS-IDORDNR5-X NUMERIC AND WS-IDORDNR5-X > ZERO                    
055900       MOVE WS-IDORDNR5-X   TO WS-IDORDNR5                                
056000       MOVE WS-IDKUNDRF     TO W-IDKUNDRF-IDGM-MIN                        
056100                               W-IDKUNDRF-IDGM-MAX                        
056200                               W-IDKUNDRF-MIN                             
056300                               W-IDKUNDRF-MAX                             
056400     ELSE                                                                 
056500       MOVE NEJ TO NYCKLAR-SW                                             
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 BE-KOLLA-IDPRODNR SECTION.                                               
057000                                                                          
057100     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
057200                                                                          
057300     IF MID-IDPRODNR-IN = ALL '+'                                         
057400       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
057500       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
057600     ELSE                                                                 
057700       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
057800       MOVE '7'         TO MFS-IDPFK                                      
057900       MOVE SPACE       TO MFS-KDTRTYP                                    
058000     END-IF                                                               
058100                                                                          
058200     IF WS-IDPRODNR NUMERIC AND WS-IDPRODNR > ZERO                        
058300       MOVE WS-IDPRODNR TO W-IDPRODNR                                     
058400                           MOD-IDPRODNR                                   
058500       MOVE JA TO PRODNR-SW                                               
058600       IF WS-IDDISTR = ZERO         AND                                   
058700          WS-IDKUNDNR = ZERO        AND                                   
058800          WS-IDORDNR5-X = ZERO                                            
058900             MOVE JA TO NYCKLAR-SW                                        
059000       END-IF                                                             
059100     ELSE                                                                 
059200       IF NOT DISTR-IFYLLT                                                
059300         MOVE NEJ TO NYCKLAR-SW                                           
059400       END-IF                                                             
059500     END-IF                                                               
059600                                                                          
059700     IF MID-IDDISTR-IN      = ALL '+' AND                                 
059800        MID-IDKUNDNR-IN     = ALL '+' AND                                 
059900        MID-IDORDNR5-IN     = ALL '+' AND                                 
060000        MID-IDPRODNR-IN NOT = ALL '+'                                     
060100                                                                          
060200        MOVE SPACE              TO WS-IDDISTR                             
060300                                   WS-IDKUNDNR                            
060400                                   WS-IDORDNR5-X                          
060500        MOVE NEJ                TO DISTRIKT-SW                            
060600     END-IF                                                               
060700                                                                          
060800     IF PRODNR-IFYLLT  AND DISTR-IFYLLT                                   
060900        MOVE NEJ TO PRODNR-SW                                             
061000        MOVE SPACE TO WS-IDPRODNR                                         
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
061400 BF-KOLLA-IDDC            SECTION.                                        
061500                                                                          
061600     IF MID-IDDC-IN = ALL '+'                                             
061700       IF MID-IDDC-UT = SPACE                                             
061800         MOVE NEJ                     TO NYCKLAR-SW                       
061900       ELSE                                                               
062000         MOVE MID-IDDC-UT                 TO WS-IDDC                      
062100       END-IF                                                             
062200     ELSE                                                                 
062300       MOVE MID-IDDC-IN                   TO WS-IDDC                      
062400       MOVE '7'                           TO MFS-IDPFK                    
062500       MOVE SPACE                         TO MFS-KDTRTYP                  
062600     END-IF                                                               
062700                                                                          
062800     IF WS-IDDC IS > SPACE                                                
062900       MOVE WS-IDDC                 TO WS-IDDC-NUM                        
063000     ELSE                                                                 
063100       MOVE NEJ                     TO NYCKLAR-SW                         
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 F-LAES-VISA-INFO SECTION.                                                
063600                                                                          
063700     IF WS-IDPRODNR > ZERO                                                
063800        PERFORM FA-LAES-WDE601                                            
063900     ELSE                                                                 
064000        PERFORM FB-LAES-WDE401                                            
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 FA-LAES-WDE601 SECTION.                                                  
064500                                                                          
064600     MOVE JA TO ALLT-SW                                                   
064700     PERFORM IMS-GU-WDE6-WDE601                                           
064800                                                                          
064900     IF SEGMENT-FINNS               AND                                   
065000        VORD-IDDC = WS-IDDC-NUM                                           
065100* * * * SATSORDER FÅR EJ GÅ DENNA VÄGEN * * * * * * * * * * * *           
065200        MOVE VORD-IDDISTR TO TEST-IDDISTR                                 
065300        IF DIST19-SATS                                                    
065400          PERFORM MFS-RENSA-FAELT-IN                                      
065500          PERFORM MFS-RENSA-FAELT-UT                                      
065600          PERFORM MFS-FORM-ATTR                                           
065700          MOVE ERR-OTILLATEN-UPPDAT   TO MED-IDMFSFEL                     
065800          CALL WMEDKONV USING MED-WMEDAREA                                
065900          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
066000          MOVE NEJ TO ALLT-SW                                             
066100        ELSE                                                              
066200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
066300          MOVE VORD-IDPRODNR          TO MOD-IDPRODNR                     
066400        END-IF                                                            
066500     ELSE                                                                 
066600        PERFORM MFS-RENSA-FAELT-IN                                        
066700        PERFORM MFS-RENSA-FAELT-UT                                        
066800        PERFORM MFS-FORM-ATTR                                             
066900                                                                          
067000                                                                          
067100        MOVE ERR-ORDERN-SAKNAS      TO MED-IDMFSFEL                       
067200                                                                          
067300        CALL WMEDKONV USING MED-WMEDAREA                                  
067400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
067500        MOVE NEJ TO ALLT-SW                                               
067600     END-IF                                                               
067700     .                                                                    
067800     EJECT                                                                
067900 FB-LAES-WDE401 SECTION.                                                  
068000                                                                          
068100     MOVE NEJ TO ALLT-SW                                                  
068200     PERFORM IMS-GU-WDE4-WDE401-SEQ                                       
068300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR ALLT-OK                
068400        IF SEGMENT-FINNS AND KORD-IDDC     = WS-IDDC-NUM AND              
068500           KORD-KVORDRAD-LEVPL             = +0       AND                 
068600           KORD-IDUSER                     = ZERO     AND                 
068700           KORD-KVORDRAD-PACK              = +0                           
068800           MOVE KORD-IDPRODNR       TO W-IDPRODNR                         
068900                                       MOD-IDPRODNR                       
069000           MOVE JA          TO ALLT-SW                                    
069100        ELSE                                                              
069200           PERFORM IMS-GN-WDE4-WDE401-SEQ                                 
069300        END-IF                                                            
069400     END-PERFORM                                                          
069500     IF ALLT-OK                                                           
069600        CONTINUE                                                          
069700     ELSE                                                                 
069800        PERFORM MFS-RENSA-FAELT-IN                                        
069900        PERFORM MFS-RENSA-FAELT-UT                                        
070000        PERFORM MFS-FORM-ATTR                                             
070100                                                                          
070200        MOVE ERR-ORDERN-SAKNAS      TO MED-IDMFSFEL                       
070300                                                                          
070400        CALL WMEDKONV USING MED-WMEDAREA                                  
070500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 G-KOLLA-INPUT SECTION.                                                   
071000                                                                          
071100     MOVE JA  TO INDATA-SW                                                
071200     PERFORM GC-KOLLA-OM-NUMERIC                                          
071300     IF INDATA-SW NOT = 'C' OR 'E'                                        
071400        IF MID-IDPLKLST NOT = ALL '+'                                     
071500           IF MID-IDRADNR-FOM = ALL '+'                                   
071600              IF MID-IDRADNR-TOM = ALL '+'                                
071700                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPLKLST-ATTR            
071800              ELSE                                                        
071900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-TOM-ATTR           
072000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR              
072100                 MOVE 'B' TO INDATA-SW                                    
072200              END-IF                                                      
072300           ELSE                                                           
072400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR              
072500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR                 
072600              MOVE 'B' TO INDATA-SW                                       
072700              IF MID-IDRADNR-TOM NOT = ALL '+'                            
072800                MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-TOM-ATTR            
072900              END-IF                                                      
073000           END-IF                                                         
073100        ELSE                                                              
073200           IF MID-IDRADNR-FOM = ALL '+'  AND                              
073300              MID-IDRADNR-TOM = ALL '+'                                   
073400              MOVE 'D' TO INDATA-SW                                       
073500           END-IF                                                         
073600           IF INDATA-SW NOT = 'D'                                         
073700              IF MID-IDRADNR-FOM = ALL '+'                                
073800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR           
073900                 MOVE 'A' TO INDATA-SW                                    
074000              ELSE                                                        
074100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-FOM-ATTR         
074200              END-IF                                                      
074300                                                                          
074400              IF MID-IDRADNR-TOM = ALL '+'                                
074500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-TOM-ATTR           
074600                 MOVE 'A' TO INDATA-SW                                    
074700              ELSE                                                        
074800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-TOM-ATTR         
074900              END-IF                                                      
075000           END-IF                                                         
075100        END-IF                                                            
075200     END-IF                                                               
075300                                                                          
075400     IF INDATA-OK                                                         
075500        IF PRODNR-IFYLLT                                                  
075600           PERFORM GA-KOLLA-VIA-WDE6                                      
075700        ELSE                                                              
075800           PERFORM GB-KOLLA-VIA-WDE4SEQ                                   
075900        END-IF                                                            
076000     END-IF                                                               
076100                                                                          
076200     IF INDATA-FEL                                                        
076300     AND INDATA-SW > 'F'                                                  
076400       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
076500       CALL WMEDKONV USING MED-WMEDAREA                                   
076600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
076700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
076800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
076900     END-IF                                                               
077000                                                                          
077100     IF INDATA-SW = 'A'                                                   
077200       MOVE ERR-BADE-FOM-OCH-TOM TO MED-IDMFSFEL                          
077300       CALL WMEDKONV USING MED-WMEDAREA                                   
077400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
077500       MOVE NEJ          TO INDATA-SW                                     
077600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
077700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
077800     END-IF                                                               
077900                                                                          
078000     IF INDATA-SW = 'B'                                                   
078100       MOVE ERR-EJ-BADE-PLKLST-INTERVALL TO MED-IDMFSFEL                  
078200       CALL WMEDKONV USING MED-WMEDAREA                                   
078300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
078400       MOVE NEJ          TO INDATA-SW                                     
078500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
078600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
078700     END-IF                                                               
078800                                                                          
078900     IF INDATA-SW = 'C'                                                   
079000       MOVE ERR-EJ-NUMERISKT TO MED-IDMFSFEL                              
079100       CALL WMEDKONV USING MED-WMEDAREA                                   
079200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
079300       MOVE NEJ          TO INDATA-SW                                     
079400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
079500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
079600     END-IF                                                               
079700                                                                          
079800     IF INDATA-SW = 'D'                                                   
079900       MOVE ERR-PLKLST-FEL   TO MED-IDMFSFEL                              
080000       CALL WMEDKONV USING MED-WMEDAREA                                   
080100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
080200       MOVE ERR-PLKLST-FEL   TO MOD-TEMFSFEL                              
080300       MOVE NEJ          TO INDATA-SW                                     
080400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
080500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
080600       MOVE    MFS-ROER-EJ-FAELT TO MOD-IDPRODNR                          
080700       MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDPLKLST-ATTR                     
080800     END-IF                                                               
080900                                                                          
081000     IF INDATA-SW = 'E'                                                   
081100       MOVE ERR-INF-SAKNAS   TO MED-IDMFSFEL                              
081200       CALL WMEDKONV USING MED-WMEDAREA                                   
081300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
081400       MOVE NEJ          TO INDATA-SW                                     
081500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
081600       MOVE    MFS-ROER-EJ-FAELT TO MOD-IDPRODNR                          
081700       MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDPLKLST-ATTR                     
081800     END-IF                                                               
081900                                                                          
082000     IF INDATA-SW = 'F'                                                   
082100       MOVE ERR-INTERVALL-UTDELAT TO MED-IDMFSINF                         
082200       CALL WMEDKONV USING MED-WMEDAREA                                   
082300       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
082400       MOVE NEJ          TO INDATA-SW                                     
082500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
082600       MOVE    MFS-ROER-EJ-FAELT TO MOD-IDPRODNR                          
082700       MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDPLKLST-ATTR                     
082800     END-IF                                                               
082900     .                                                                    
083000     EJECT                                                                
083100 GA-KOLLA-VIA-WDE6 SECTION.                                               
083200                                                                          
083300     IF MID-IDPLKLST NOT = ALL '+'                                        
083400        PERFORM GAA-LAES-KOLLA-IDPLKLST                                   
083500     ELSE                                                                 
083600        PERFORM GAB-LAES-KOLLA-IDRADNR                                    
083700     END-IF                                                               
083800                                                                          
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 GAA-LAES-KOLLA-IDPLKLST SECTION.                                         
084300                                                                          
084400     MOVE NEJ TO ALLT-SW                                                  
084500     PERFORM IMS-GU-WDE6-WDE601                                           
084600                                                                          
084700     MOVE VORD-IDDISTR             TO W-IDDISTR-MIN                       
084800                                      W-IDDISTR-MAX                       
084900     MOVE VORD-IDKUNDNR            TO W-IDKUNDNR-MIN                      
085000                                      W-IDKUNDNR-MAX                      
085100     MOVE LOW-VALUE                TO W-IDKUNDRF-MIN                      
085200     MOVE HIGH-VALUE               TO W-IDKUNDRF-MAX                      
085300                                                                          
085400     MOVE VORD-IDPRODNR            TO WS-IDPRODNR-NUM                     
085500     MOVE MID-IDPLKLST             TO WS-IDPLKLST-NUM                     
085600                                                                          
085700     PERFORM IMS-GU-WDE4A-WDE4A1                                          
085800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
085900        IF SEQA-IDPLKLST = WS-IDPLKLST-NUM  AND                           
086000           SEQA-IDPRODNR = WS-IDPRODNR-NUM                                
086100           PERFORM S01-KOLLA-MOT-WDE401-OM-OK                             
086200        END-IF                                                            
086300        IF NOT ALLT-OK                                                    
086400           PERFORM IMS-GN-WDE4A-WDE4A1                                    
086500        END-IF                                                            
086600     END-PERFORM                                                          
086700                                                                          
086800     IF ALLT-OK                                                           
086900        MOVE KORD-IDDISTR            TO WS-IDDISTR-SPAR                   
087000        MOVE KORD-IDKUNDNR           TO WS-IDKUNDNR-SPAR                  
087100        MOVE KORD-IDKUNDRF           TO WS-IDKUNDRF-SPAR                  
087200        MOVE KORD-IDPRODNR           TO WS-IDPRODNR-SPAR                  
087300        MOVE KORD-IDPLKLST           TO WS-IDPLKLST-SPAR                  
087400     ELSE                                                                 
087500        IF INDATA-SW NOT = 'F'                                            
087600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR                    
087700           MOVE NEJ TO INDATA-SW                                          
087800        END-IF                                                            
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 GAB-LAES-KOLLA-IDRADNR SECTION.                                          
088300                                                                          
088400     MOVE NEJ TO ALLT-SW                                                  
088500     PERFORM IMS-GU-WDE6-WDE601                                           
088600                                                                          
088700     MOVE VORD-IDDISTR             TO W-IDDISTR-MIN                       
088800                                      W-IDDISTR-MAX                       
088900                                      W-IDDISTR-IDGM-MIN                  
089000                                      W-IDDISTR-IDGM-MAX                  
089100     MOVE VORD-IDKUNDNR            TO W-IDKUNDNR-MIN                      
089200                                      W-IDKUNDNR-IDGM-MIN                 
089300                                      W-IDKUNDNR-IDGM-MAX                 
089400                                      W-IDKUNDNR-MAX                      
089500     MOVE LOW-VALUE                TO W-IDKUNDRF-MIN                      
089600                                      W-IDKUNDRF-IDGM-MIN                 
089700     MOVE HIGH-VALUE               TO W-IDKUNDRF-MAX                      
089800                                      W-IDKUNDRF-IDGM-MAX                 
089900     MOVE VORD-IDPRODNR            TO WS-IDPRODNR-NUM                     
090000                                                                          
090100     PERFORM IMS-GU-WDE4-WDE401-SEQ                                       
090200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR ALLT-OK                
090300        IF KORD-IDPRODNR      = WS-IDPRODNR-NUM                           
090400           IF KORD-IDUSER        = 00000000 AND                           
090500              KORD-KVORDRAD-PACK = 00000                                  
090600              MOVE JA TO ALLT-SW                                          
090700              IF MID-IDRADNR-FOM NUMERIC AND                              
090800                 MID-IDRADNR-FOM > ZERO                                   
090900                 MOVE MID-IDRADNR-FOM TO WS-IDPURAD-FOM                   
091000                                          W-IDPURAD                       
091100              END-IF                                                      
091200              MOVE KORD-IDKUNDRF TO W-IDKUNDRF-IDGM-MIN                   
091300                                    W-IDKUNDRF-IDGM-MAX                   
091400                                    W-IDKUNDRF-MIN                        
091500                                    W-IDKUNDRF-MAX                        
091600              MOVE KORD-KVORDRAD     TO WS-KVORDRAD-SPAR                  
091700              MOVE KORD-IDDISTR      TO WS-IDDISTR-SPAR                   
091800              MOVE KORD-IDKUNDNR     TO WS-IDKUNDNR-SPAR                  
091900              MOVE KORD-IDKUNDRF     TO WS-IDKUNDRF-SPAR                  
092000              MOVE KORD-IDPRODNR     TO WS-IDPRODNR-SPAR                  
092100              MOVE KORD-IDPLKLST     TO WS-IDPLKLST-SPAR                  
092200              PERFORM IMS-GET-WDE4-WDE411                                 
092300              IF ORAD-IDPURAD = WS-IDPURAD-FOM                            
092400                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-FOM-ATTR         
092500                 IF MID-IDRADNR-TOM NUMERIC AND                           
092600                    MID-IDRADNR-TOM > ZERO                                
092700                    MOVE MID-IDRADNR-TOM TO WS-IDPURAD-TOM                
092800                 END-IF                                                   
092900                 COMPUTE WS-IDPURAD-FOM = WS-IDPURAD-FOM - 1              
093000                 COMPUTE WS-KVORDRAD-SPAR =                               
093100                         WS-KVORDRAD-SPAR + WS-IDPURAD-FOM                
093200                 IF WS-KVORDRAD-SPAR = WS-IDPURAD-TOM                     
093300                    MOVE MFS-NUM-FAELT-RAETT                              
093400                                        TO MOD-IDRADNR-TOM-ATTR           
093500                 ELSE                                                     
093600                    MOVE NEJ TO INDATA-SW                                 
093700                                ALLT-SW                                   
093800                    MOVE 'GE' TO STATUS-WS                                
093900                    MOVE MFS-NUM-FAELT-FEL                                
094000                                        TO MOD-IDRADNR-TOM-ATTR           
094100                 END-IF                                                   
094200              ELSE                                                        
094300                 MOVE NEJ TO ALLT-SW                                      
094400                 MOVE NEJ TO INDATA-SW                                    
094500                 MOVE MFS-NUM-FAELT-FEL                                   
094600                                        TO MOD-IDRADNR-FOM-ATTR           
094700                 PERFORM IMS-GN-WDE4-WDE401-SEQ                           
094800              END-IF                                                      
094900           ELSE                                                           
095000              MOVE 'F' TO INDATA-SW                                       
095100              PERFORM IMS-GN-WDE4-WDE401-SEQ                              
095200           END-IF                                                         
095300        ELSE                                                              
095400           PERFORM IMS-GN-WDE4-WDE401-SEQ                                 
095500        END-IF                                                            
095600     END-PERFORM                                                          
095700                                                                          
095800     IF ALLT-OK                                                           
095900        MOVE JA TO INDATA-SW                                              
096000     ELSE                                                                 
096100        IF INDATA-SW NOT = 'F'                                            
096200           MOVE NEJ TO INDATA-SW                                          
096300        ELSE                                                              
096400           PERFORM MFS-FORM-ATTR                                          
096500        END-IF                                                            
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 GB-KOLLA-VIA-WDE4SEQ SECTION.                                            
097000                                                                          
097100     IF MID-IDPLKLST NOT = ALL '+'                                        
097200        MOVE MID-IDPLKLST TO WS-IDPLKLST-NUM                              
097300     END-IF                                                               
097400                                                                          
097500     IF WS-IDPLKLST-NUM > ZERO                                            
097600        PERFORM GBA-LAES-KOLLA-IDPLKLST                                   
097700     ELSE                                                                 
097800        PERFORM GBB-LAES-KOLLA-IDRADNR                                    
097900     END-IF                                                               
098000                                                                          
098100     .                                                                    
098200     EJECT                                                                
098300                                                                          
098400 GBA-LAES-KOLLA-IDPLKLST SECTION.                                         
098500                                                                          
098600     MOVE NEJ TO ALLT-SW                                                  
098700     PERFORM IMS-GU-WDE4-WDE401-SEQ                                       
098800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
098900        IF SEGMENT-FINNS AND                                              
099000           KORD-IDDC                    = WS-IDDC-NUM     AND             
099100           KORD-KVORDRAD-LEVPL          = +0              AND             
099200           KORD-IDPLKLST                = WS-IDPLKLST-NUM                 
099300           IF KORD-IDUSER               = ZERO            AND             
099400              KORD-KVORDRAD-PACK        = +0                              
099500              MOVE 'GE'        TO STATUS-WS                               
099600              MOVE JA          TO ALLT-SW                                 
099700           ELSE                                                           
099800              MOVE 'F'         TO INDATA-SW                               
099900              MOVE 'GE'        TO STATUS-WS                               
100000           END-IF                                                         
100100        ELSE                                                              
100200           PERFORM IMS-GN-WDE4-WDE401-SEQ                                 
100300        END-IF                                                            
100400     END-PERFORM                                                          
100500                                                                          
100600     IF ALLT-OK                                                           
100700        MOVE KORD-IDDISTR  TO W-IDDISTR-WDE401                            
100800        MOVE KORD-IDKUNDNR TO W-IDKUNDNR-WDE401                           
100900        MOVE KORD-IDKUNDRF TO W-IDKUNDRF-WDE401                           
101000        MOVE KORD-IDPRODNR TO W-IDPRODNR-WDE401                           
101100                              WS-IDPRODNR-SPAR                            
101200        MOVE KORD-IDPLKLST TO W-IDPLKLST-WDE401                           
101300     ELSE                                                                 
101400        IF INDATA-SW NOT = 'F'                                            
101500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR                    
101600           MOVE NEJ TO INDATA-SW                                          
101700        END-IF                                                            
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100                                                                          
102200 GBB-LAES-KOLLA-IDRADNR SECTION.                                          
102300                                                                          
102400     MOVE NEJ TO ALLT-SW                                                  
102500                                                                          
102600     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR ALLT-OK                
102700        IF KORD-IDDC              = WS-IDDC-NUM  AND                      
102800           KORD-KVORDRAD-LEVPL    = +0                                    
102900           IF KORD-IDUSER         = ZERO         AND                      
103000              KORD-KVORDRAD-PACK  = +0                                    
103100              MOVE JA TO ALLT-SW                                          
103200              IF MID-IDRADNR-FOM NUMERIC  AND                             
103300                 MID-IDRADNR-FOM > ZERO                                   
103400                 MOVE MID-IDRADNR-FOM TO WS-IDPURAD-FOM                   
103500              END-IF                                                      
103600              IF MID-IDRADNR-TOM NUMERIC  AND                             
103700                 MID-IDRADNR-TOM > ZERO                                   
103800                 MOVE MID-IDRADNR-TOM TO WS-IDPURAD-TOM                   
103900              END-IF                                                      
104000              MOVE KORD-KVORDRAD TO WS-KVORDRAD-SPAR                      
104100              MOVE KORD-IDDISTR  TO W-IDDISTR-WDE401                      
104200              MOVE KORD-IDKUNDNR TO W-IDKUNDNR-WDE401                     
104300              MOVE KORD-IDKUNDRF TO W-IDKUNDRF-WDE401                     
104400              MOVE KORD-IDPRODNR TO W-IDPRODNR-WDE401                     
104500                                    WS-IDPRODNR-SPAR                      
104600              MOVE KORD-IDPLKLST TO W-IDPLKLST-WDE401                     
104700              PERFORM IMS-GET-WDE4-WDE411                                 
104800              IF ORAD-IDPURAD = WS-IDPURAD-FOM                            
104900                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-FOM-ATTR         
105000                 COMPUTE WS-IDPURAD-FOM = WS-IDPURAD-FOM - 1              
105100                 COMPUTE WS-KVORDRAD-SPAR =                               
105200                         WS-KVORDRAD-SPAR + WS-IDPURAD-FOM                
105300                 IF WS-KVORDRAD-SPAR = WS-IDPURAD-TOM                     
105400                    MOVE MFS-NUM-FAELT-RAETT                              
105500                                          TO MOD-IDRADNR-TOM-ATTR         
105600                 ELSE                                                     
105700                    MOVE NEJ TO ALLT-SW                                   
105800                                INDATA-SW                                 
105900                    MOVE 'GE' TO STATUS-WS                                
106000                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-TOM-ATTR         
106100                 END-IF                                                   
106200              ELSE                                                        
106300                 MOVE NEJ TO ALLT-SW                                      
106400                             INDATA-SW                                    
106500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR           
106600                 PERFORM IMS-GN-WDE4-WDE401-SEQ                           
106700              END-IF                                                      
106800           ELSE                                                           
106900              MOVE 'F' TO INDATA-SW                                       
107000              PERFORM IMS-GN-WDE4-WDE401-SEQ                              
107100           END-IF                                                         
107200        ELSE                                                              
107300           PERFORM IMS-GN-WDE4-WDE401-SEQ                                 
107400        END-IF                                                            
107500     END-PERFORM                                                          
107600                                                                          
107700     IF ALLT-OK                                                           
107800        MOVE JA TO INDATA-SW                                              
107900     ELSE                                                                 
108000        IF INDATA-SW NOT = 'F'                                            
108100           MOVE NEJ TO INDATA-SW                                          
108200        ELSE                                                              
108300           PERFORM MFS-FORM-ATTR                                          
108400        END-IF                                                            
108500     END-IF                                                               
108600     .                                                                    
108700     EJECT                                                                
108800 GC-KOLLA-OM-NUMERIC SECTION.                                             
108900                                                                          
109000     IF EGEN-MID                                                          
109100        IF MID-IDPLKLST NOT = ALL '+'                                     
109200           IF MID-IDPLKLST NUMERIC                                        
109300              MOVE MID-IDPLKLST TO WS-IDPLKLST-NUM                        
109400              MOVE WS-IDPLKLST-NUM TO MOD-IDPLKLST                        
109500              INSPECT MOD-IDPLKLST REPLACING LEADING ZERO BY SPACE        
109600              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPLKLST-ATTR               
109700           ELSE                                                           
109800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR                 
109900              MOVE 'C' TO INDATA-SW                                       
110000           END-IF                                                         
110100        ELSE                                                              
110200           MOVE MFS-RENSA-FAELT TO MOD-IDPLKLST                           
110300        END-IF                                                            
110400                                                                          
110500        IF MID-IDRADNR-FOM NOT = ALL '+'                                  
110600           IF MID-IDRADNR-FOM NUMERIC                                     
110700              MOVE MID-IDRADNR-FOM TO WS-IDRADNR-FOM-NUM                  
110800              MOVE WS-IDRADNR-FOM-NUM TO MOD-IDRADNR-FOM                  
110900           INSPECT MOD-IDRADNR-FOM REPLACING LEADING ZERO BY SPACE        
111000              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-FOM-ATTR            
111100           ELSE                                                           
111200              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR              
111300              MOVE 'C' TO INDATA-SW                                       
111400           END-IF                                                         
111500        ELSE                                                              
111600           MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-FOM                        
111700        END-IF                                                            
111800                                                                          
111900        IF MID-IDRADNR-TOM NOT = ALL '+'                                  
112000           IF MID-IDRADNR-TOM NUMERIC                                     
112100              MOVE MID-IDRADNR-TOM TO WS-IDRADNR-TOM-NUM                  
112200              MOVE WS-IDRADNR-TOM-NUM TO MOD-IDRADNR-TOM                  
112300           INSPECT MOD-IDRADNR-TOM REPLACING LEADING ZERO BY SPACE        
112400              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-TOM-ATTR            
112500           ELSE                                                           
112600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-TOM-ATTR              
112700              MOVE 'C' TO INDATA-SW                                       
112800           END-IF                                                         
112900        ELSE                                                              
113000           MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-TOM                        
113100        END-IF                                                            
113200     ELSE                                                                 
113300        PERFORM MFS-RENSA-FAELT-IN                                        
113400        MOVE '+++'         TO MID-IDPLKLST                                
113500        MOVE '++++'        TO MID-IDRADNR-FOM                             
113600        MOVE '++++'        TO MID-IDRADNR-TOM                             
113700        MOVE 'E' TO INDATA-SW                                             
113800     END-IF                                                               
113900     .                                                                    
114000     EJECT                                                                
114100 H-UPPDATERA SECTION.                                                     
114200                                                                          
114300     IF PRODNR-IFYLLT                                                     
114400        MOVE WS-IDDISTR-SPAR           TO W-IDDISTR-WDE401                
114500        MOVE WS-IDKUNDNR-SPAR          TO W-IDKUNDNR-WDE401               
114600        MOVE WS-IDKUNDRF-SPAR          TO W-IDKUNDRF-WDE401               
114700        MOVE WS-IDPRODNR-SPAR          TO W-IDPRODNR-WDE401               
114800        MOVE WS-IDPLKLST-SPAR          TO W-IDPLKLST-WDE401               
114900     END-IF                                                               
115000                                                                          
115100     PERFORM HA-LAES-UPPDAT-IDHTYP-4306                                   
115200                                                                          
115300     IF ALLT-OK                                                           
115400        PERFORM IMS-GHU-WDE4-WDE401                                       
115500        IF KORD-IDUSER >= ZERO                                            
115600           MOVE KORD-IDORDER           TO W-IDORDER-WDQ3                  
115700           MOVE KORD-IDDC              TO W-IDDC-WDQ3                     
115800           MOVE KORD-IDPRODNR          TO W-IDPRODNR-WDQ3                 
115900           MOVE KORD-IDPLKLST          TO W-IDPLKLST-WDQ3                 
116000                                                                          
116100           IF SEGMENT-FINNS                                               
116200              MOVE WS-IDANSTNR         TO WS-IDANSTNR-NUM                 
116300              MOVE WS-IDANSTNR-NUM     TO WS-IDUSER-NUM                   
116400              MOVE WS-IDUSER-NUM       TO KORD-IDUSER                     
116500              PERFORM IMS-REPL-WDE4-WDE401                                
116600           END-IF                                                         
116700                                                                          
116800           PERFORM IMS-GHU-ORQA-WDQ301                                    
116900                                                                          
117000           IF SEGMENT-FINNS                                               
117100              AND                                                         
117200              ODEL-KDODELSTA = 'U'                                        
117300              MOVE ODEL-IDDC           TO W-4447-IDDC                     
117400                                          W-4487-IDDC                     
117500              MOVE ODEL-DARFS          TO W-4490-DARFS                    
117600              MOVE ODEL-IDPRODNR       TO W-4490-IDPRODNR                 
117700              MOVE ODEL-IDPLKLST       TO W-4490-IDPLKLST                 
117800              MOVE ODEL-IDPRC          TO W-4448-IDPRC                    
117900                                                                          
118000              MOVE WS-IDUSER-NUM       TO ODEL-IDUSER                     
118100              PERFORM IMS-REPL-ORQA-WDQ301                                
118200                                                                          
118300              PERFORM IMS-GU-XXKH-XXKH11                                  
118400              MOVE 4448-KDPRCGRP       TO W-4488-KDPRCGRP                 
118500                                                                          
118600              PERFORM IMS-GHU-WDGX4490                                    
118700              IF SEGMENT-FINNS                                            
118800                 MOVE WS-IDUSER-NUM       TO 4490-IDUSER                  
118900                 PERFORM IMS-REPL-WDGX4490                                
119000              END-IF                                                      
119100                                                                          
119200           END-IF                                                         
119300                                                                          
119400           MOVE INF-ORDERN-DELAD TO MED-IDMFSINF                          
119500           CALL WMEDKONV USING MED-WMEDAREA                               
119600           MOVE MED-MFSINF TO MOD-TEMFSINF                                
119700           PERFORM MFS-FORM-ATTR                                          
119800           PERFORM MFS-RENSA-FAELT-IN                                     
119900        ELSE                                                              
120000           MOVE ERR-INTERVALL-UTDELAT TO MED-IDMFSINF                     
120100           CALL WMEDKONV USING MED-WMEDAREA                               
120200           MOVE MED-MFSINF TO MOD-TEMFSINF                                
120300           PERFORM MFS-FORM-ATTR                                          
120400           PERFORM MFS-RENSA-FAELT-IN                                     
120500        END-IF                                                            
120600     END-IF                                                               
120700     .                                                                    
120800     EJECT                                                                
120900 HA-LAES-UPPDAT-IDHTYP-4306  SECTION.                                     
121000                                                                          
121100     MOVE JA                           TO ALLT-SW                         
121200     MOVE WS-IDDC                      TO W-4305-IDDC                     
121300     MOVE WS-IDPRODNR-SPAR             TO W-4306-IDPRODNR                 
121400     PERFORM IMS-GHU-XXDJ-XXDJ01                                          
121500     IF SEGMENT-SAKNAS                                                    
121600        MOVE 'ROTEN IDHTYP 4305 SAKNAS PÅ WDG2' TO FELTEXT                
121700        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
121800     END-IF                                                               
121900     PERFORM IMS-GNP-XXDJ-XXDJ11                                          
122000     IF SEGMENT-FINNS                                                     
122100        IF 4306-KDPACLAS = +1 OR +2 OR +3                                 
122200           MOVE ERR-PACKNING-PAGAR    TO MED-IDMFSINF                     
122300           CALL WMEDKONV USING MED-WMEDAREA                               
122400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
122500           PERFORM MFS-FORM-ATTR                                          
122600           PERFORM MFS-RENSA-FAELT-IN                                     
122700           MOVE NEJ                    TO ALLT-SW                         
122800        ELSE                                                              
122900           MOVE +5                     TO 4306-KDPACLAS                   
123000           PERFORM IMS-REPL-4306                                          
123100        END-IF                                                            
123200     ELSE                                                                 
123300        MOVE LOW-VALUE                 TO IO-AREA                         
123400        MOVE W-4306-IDPRODNR           TO 4306-IDPRODNR                   
123500        MOVE LOW-VALUE                 TO 4306-LOWVALUE                   
123600        MOVE NEJ                       TO 4306-FLANNULL                   
123700        MOVE +5                        TO 4306-KDPACLAS                   
123800        PERFORM IMS-ISRT-4306                                             
123900     END-IF                                                               
124000     .                                                                    
124100     EJECT                                                                
124200 MFS-RENSA-FAELT-UT SECTION.                                              
124300                                                                          
124400*    --- ALLA UTDATA-FÄLT                                                 
124500     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR                                 
124600     .                                                                    
124700     SKIP2                                                                
124800 MFS-RENSA-FAELT-IN SECTION.                                              
124900                                                                          
125000*    --- ALLA INDATA-FÄLT                                                 
125100     MOVE MFS-RENSA-FAELT TO MOD-IDPLKLST                                 
125200                             MOD-IDRADNR-FOM                              
125300                             MOD-IDRADNR-TOM                              
125400     .                                                                    
125500     EJECT                                                                
125600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
125700                                                                          
125800*    --- ALLA UTDATA-FÄLT                                                 
125900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR                               
126000     .                                                                    
126100     SKIP2                                                                
126200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
126300                                                                          
126400*    --- ALLA INDATA-FÄLT                                                 
126500                                                                          
126600     IF MID-IDPLKLST NOT = ALL '+'                                        
126700        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPLKLST                            
126800     END-IF                                                               
126900                                                                          
127000     IF MID-IDRADNR-FOM NOT = ALL '+'                                     
127100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-FOM                         
127200     END-IF                                                               
127300                                                                          
127400     IF MID-IDRADNR-TOM NOT = ALL '+'                                     
127500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-TOM                         
127600     END-IF                                                               
127700     .                                                                    
127800     EJECT                                                                
127900 MFS-FORM-ATTR SECTION.                                                   
128000                                                                          
128100*    --- ALLA INDATA-FÄLT                                                 
128200     MOVE MFS-FORMATETS-ATTR TO MOD-IDPLKLST-ATTR                         
128300                                MOD-IDRADNR-FOM-ATTR                      
128400                                MOD-IDRADNR-TOM-ATTR                      
128500     .                                                                    
128600     EJECT                                                                
128700 S01-KOLLA-MOT-WDE401-OM-OK SECTION.                                      
128800                                                                          
128900     MOVE SEQA-IDDISTR  TO W-IDDISTR-WDE401                               
129000     MOVE SEQA-IDKUNDNR TO W-IDKUNDNR-WDE401                              
129100     MOVE SEQA-IDKUNDRF TO W-IDKUNDRF-WDE401                              
129200     MOVE SEQA-IDPRODNR TO W-IDPRODNR-WDE401                              
129300     MOVE SEQA-IDPLKLST TO W-IDPLKLST-WDE401                              
129400                                                                          
129500     PERFORM IMS-GHU-WDE4-WDE401                                          
129600                                                                          
129700     IF KORD-IDUSER = ZERO                                                
129800        IF KORD-KVORDRAD-PACK = 0                                         
129900           MOVE JA TO ALLT-SW                                             
130000           MOVE 'GE' TO STATUS-WS                                         
130100        END-IF                                                            
130200     END-IF                                                               
130300                                                                          
130400     IF NOT ALLT-OK                                                       
130500        MOVE 'F' TO INDATA-SW                                             
130600     END-IF                                                               
130700     .                                                                    
130800     EJECT                                                                
130900* --- IMS SEKTIONER ---                                                   
131000     SKIP3                                                                
131100 IMS-GET-MSG SECTION.                                                     
131200                                                                          
131300     MOVE '  QC' TO GODK-STATUSKODER                                      
131400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
131500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800     SKIP3                                                                
131900 IMS-INSERT-MSG SECTION.                                                  
132000                                                                          
132100     IF ENGLISH-TEXT                                                      
132200       MOVE 'N' TO MFS-KDHUVOMR                                           
132300     ELSE                                                                 
132400       MOVE '0' TO MFS-KDHUVOMR                                           
132500     END-IF                                                               
132600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132700     MOVE SPACE TO GODK-STATUSKODER                                       
132800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
132900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133000     PERFORM IMS-STATUSKONTROLL                                           
133100     .                                                                    
133200     EJECT                                                                
133300 IMS-GHU-ORQA-WDQ301 SECTION.                                             
133400                                                                          
133500     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
133600          DELIMITED BY SIZE INTO SSA1                                     
133700     MOVE '  GE' TO GODK-STATUSKODER                                      
133800     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA SSA1                     
133900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200     SKIP3                                                                
134300 IMS-REPL-ORQA-WDQ301  SECTION.                                           
134400                                                                          
134500     MOVE '  ' TO GODK-STATUSKODER                                        
134600     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA                         
134700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000     EJECT                                                                
135100 IMS-GU-WDE4A-WDE4A1 SECTION.                                             
135200     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
135300                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
135400          DELIMITED BY SIZE INTO SSA1                                     
135500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
135600     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
135700     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     SKIP2                                                                
136100 IMS-GN-WDE4A-WDE4A1 SECTION.                                             
136200     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
136300                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
136400          DELIMITED BY SIZE INTO SSA1                                     
136500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
136600     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
136700     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     EJECT                                                                
137100 IMS-GHU-WDE4-WDE401 SECTION.                                             
137200     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
137300          DELIMITED BY SIZE INTO SSA1                                     
137400     MOVE '  ' TO GODK-STATUSKODER                                        
137500     CALL CBLTDLI USING GHU WDE401-PCB DLI-IO-AREA SSA1                   
137600     MOVE WDE401-STATUS-CODE TO STATUS-WS                                 
137700     PERFORM IMS-STATUSKONTROLL                                           
137800     .                                                                    
137900     EJECT                                                                
138000 IMS-REPL-WDE4-WDE401 SECTION.                                            
138100                                                                          
138200     MOVE '  ' TO GODK-STATUSKODER                                        
138300     CALL CBLTDLI USING REPL WDE401-PCB DLI-IO-AREA                       
138400     MOVE WDE401-STATUS-CODE TO STATUS-WS                                 
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700     EJECT                                                                
138800 IMS-GU-WDE4-WDE401-SEQ SECTION.                                          
138900     STRING 'WDE401  (WDE4ASEQ>=' W-IDGMTREF-MIN-X                        
139000                    '&WDE4ASEQ<=' W-IDGMTREF-MAX-X ')'                    
139100          DELIMITED BY SIZE INTO SSA1                                     
139200     MOVE '  GE' TO GODK-STATUSKODER                                      
139300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
139400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     .                                                                    
139700     EJECT                                                                
139800 IMS-GN-WDE4-WDE401-SEQ SECTION.                                          
139900     STRING 'WDE401  (WDE4ASEQ>=' W-IDGMTREF-MIN-X                        
140000                    '&WDE4ASEQ<=' W-IDGMTREF-MAX-X ')'                    
140100          DELIMITED BY SIZE INTO SSA1                                     
140200     MOVE '  GE' TO GODK-STATUSKODER                                      
140300     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-AREA SSA1                      
140400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
140500     PERFORM IMS-STATUSKONTROLL                                           
140600     .                                                                    
140700     EJECT                                                                
140800 IMS-GET-WDE4-WDE411 SECTION.                                             
140900     MOVE   'WDE411'  TO  SSA1                                            
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA SSA1                     
141200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP3                                                                
141600 IMS-GU-WDE6-WDE601 SECTION.                                              
141700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
141800          DELIMITED BY SIZE INTO SSA1                                     
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
142100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-GU-XXKH-XXKH11 SECTION.                                              
142600                                                                          
142700     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
142800          DELIMITED BY SIZE INTO SSA1                                     
142900     STRING 'WLXXKH11(WDGXKEY  =' W-IDPRC-X ')'                           
143000          DELIMITED BY SIZE INTO SSA2                                     
143100     MOVE '  ' TO GODK-STATUSKODER                                        
143200     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
143300     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
143400     PERFORM IMS-STATUSKONTROLL                                           
143500     .                                                                    
143600     SKIP3                                                                
143700 IMS-GHU-WDGX4490    SECTION.                                             
143800                                                                          
143900     STRING 'WDR401  (WDGXKEY  =' W-4487-IDHTYP-X ')'                     
144000          DELIMITED BY SIZE INTO SSA1                                     
144100     STRING 'WDGX4488(KDPRCGRP =' W-4488-IDHTYP-X ')'                     
144200          DELIMITED BY SIZE INTO SSA2                                     
144300     STRING 'WDGX4490(KY4490   =' W-4490-IDHTYP-X ')'                     
144400          DELIMITED BY SIZE INTO SSA3                                     
144500     MOVE '  GE' TO GODK-STATUSKODER                                      
144600     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
144700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
144800     PERFORM IMS-STATUSKONTROLL                                           
144900     .                                                                    
145000     EJECT                                                                
145100 IMS-REPL-WDGX4490 SECTION.                                               
145200                                                                          
145300     MOVE '  ' TO GODK-STATUSKODER                                        
145400     CALL CBLTDLI USING REPL 4487-PCB DLI-IO-AREA                         
145500     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     .                                                                    
145800     EJECT                                                                
145900 IMS-GHU-XXDJ-XXDJ01 SECTION.                                             
146000                                                                          
146100     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-IDHTYP-X ')'                     
146200          DELIMITED BY SIZE INTO SSA1                                     
146300     MOVE '  GE' TO GODK-STATUSKODER                                      
146400     CALL CBLTDLI USING GHU XXDJ-PCB DLI-IO-AREA SSA1                     
146500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
146600     PERFORM IMS-STATUSKONTROLL                                           
146700     .                                                                    
146800     EJECT                                                                
146900 IMS-GNP-XXDJ-XXDJ11 SECTION.                                             
147000                                                                          
147100     STRING 'WLXXDJ11(WDGXKEY  =' W-4306-IDHTYP-X ')'                     
147200          DELIMITED BY SIZE INTO SSA1                                     
147300     MOVE '  GE' TO GODK-STATUSKODER                                      
147400     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA SSA1                    
147500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     EJECT                                                                
147900 IMS-REPL-4306 SECTION.                                                   
148000                                                                          
148100     MOVE '  ' TO GODK-STATUSKODER                                        
148200     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA                         
148300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
148400     PERFORM IMS-STATUSKONTROLL                                           
148500     .                                                                    
148600     EJECT                                                                
148700 IMS-ISRT-4306 SECTION.                                                   
148800                                                                          
148900     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-IDHTYP-X ')'                     
149000          DELIMITED BY SIZE INTO SSA1                                     
149100     MOVE 'WLXXDJ11' TO SSA2                                              
149200     MOVE '  ' TO GODK-STATUSKODER                                        
149300     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA SSA1 SSA2               
149400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     .                                                                    
149700     EJECT                                                                
149800 IMS-STATUSKONTROLL SECTION.                                              
149900                                                                          
150000     SET STATUS-IX TO 1                                                   
150100     SEARCH GODK-STATUS                                                   
150200       AT END                                                             
150300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
150400         DELIMITED BY SIZE INTO FELTEXT                                   
150500         CALL FELLOG                                                      
150600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
150700     END-SEARCH                                                           
150800     .                                                                    
