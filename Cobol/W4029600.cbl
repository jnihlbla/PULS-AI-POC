000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4029600.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   JUNI-91.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR OMRÄKNING AV PRIS PÅ PRO-                    
001100*        FORMARADER. PROGRAMMET ÄR EN BAKGRUNDSMMP SOM                    
001200*        STARTAR OM SIG SJÄLV EFTER 200 ORDERRADER                        
001300*        GENOM ATT LÄGGA UPP EN NY TRANS PÅ IMS-KÖN.                      
001310*        MANUELLT SATTA PRIS OMRÄKNAS EJ.                                 
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001700*        PROGRAMMET UPPDATERAR WLPROC (WDE8)                              
001800*        PROGRAMMET UPPDATERAR WLPROD (WDE9)                              
002700*                                                                         
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W4T296X                                             
003100*                                                                         
003200*                                                                         
003300*    UTDATA.                                                              
003400*        TRANSAKTION: W4T296X                                             
003500*                                                                         
003600                                                                          
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W4029600'.            
004300                                                                          
004310 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004320                                                                          
004400 77  JA                          PIC X      VALUE 'J'.                    
004500 77  NEJ                         PIC X      VALUE 'N'.                    
004900 77  MAX-ORAD                    PIC S9(4)  VALUE +200  COMP SYNC.        
005000 77  ORAD-IX                     PIC S9(4)  VALUE +1    COMP SYNC.        
005010 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +66   COMP SYNC.        
005020 77  WS-PRISDIFF                 PIC S9(7)V9(4)         COMP-3.           
005030 77  WS-RADBELOPP                PIC S9(7)V9(2)         COMP-3.           
005100 77  WS-DATUM                    PIC 9(6).                                
006800 77  WS-KLOCKAN                  PIC 9(8).                                
011600                                                                          
012500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
012600     88  ALLT-OK                             VALUE 'J'.                   
012700                                                                          
012800 77  PROFORMA-SW                 PIC X       VALUE 'N'.                   
012900     88  PROFORMA-KLAR                       VALUE 'J'.                   
012910                                                                          
012920 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012930     88  GODK-MID                            VALUE '4264' '4296'.         
013000                                                                          
013300 77  MANUELLA-PRISER-SW          PIC X(4)    VALUE 'J'.                   
013500     88  MANUELLA-PRISER                     VALUE 'J'.                   
013900     EJECT                                                                
013910*    ----DISTR-DEALER-PRICE-----                                          
013920*01  -COPY WWDIST79                                                       
013930*                                                                         
013940     EJECT                                                                
014000*    --- SUBPROGRAM                                                       
014100 01  GENERELLA-SUBPROGRAM.                                                
014400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015100     EJECT                                                                
018000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018300     SKIP3                                                                
018400*01  MID -COPY W4I29601C0                                                 
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018800     SKIP3                                                                
018900*01  -COPY WMSGAREAC0                                                     
019100     EJECT                                                                
019700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019800*                                                                         
019900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020000                                                                          
020100 01  NYCKLAR-TILL-DLI.                                                    
020200                                                                          
021700*----> PROFORMAHUVUD                                                      
021710                                                                          
021800     03  W-E801KY-X.                                                      
022500         05  W-E801KY-IDDISTR    PIC S9(5)    COMP-3.                     
022600         05  W-E801KY-IDKUNDNR   PIC S9(7)    COMP-3.                     
022700         05  W-E801KY-IDKUNDRF   PIC X(10).                               
022710                                                                          
022820*----> PROFORMARAD                                                        
022830                                                                          
022840     03  W-E901KY-X.                                                      
022850         05  W-E901KY-IDORDER    PIC S9(7)    COMP-3.                     
022861         05  W-E901KY-IDARTNR    PIC S9(9)    COMP-3.                     
022862         05  W-E901KY-IDLOPNR    PIC S9(3)    COMP-3.                     
022880                                                                          
022890     03  W-E901KY-MIN-X.                                                  
022891         05  W-E901KY-MIN-IDORDER  PIC S9(7)    COMP-3.                   
022893         05  W-E901KY-MIN-IDARTNR  PIC S9(9)    COMP-3.                   
022894         05  W-E901KY-MIN-IDLOPNR  PIC S9(3)    COMP-3.                   
022895                                                                          
022896     03  W-E901KY-MAX-X.                                                  
022897         05  W-E901KY-MAX-IDORDER  PIC S9(7)    COMP-3.                   
022899         05  W-E901KY-MAX-IDARTNR  PIC S9(9)    COMP-3.                   
022900         05  W-E901KY-MAX-IDLOPNR  PIC S9(3)    COMP-3.                   
023000                                                                          
029100*    --- STATUS-KOD FRÅN IMS                                              
029200 01  STATUS-WS                   PIC XX.                                  
029300     88  SEGMENT-FINNS                       VALUE '  '.                  
029500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029600     88  BASEN-SLUT                          VALUE 'GB'.                  
029700     SKIP2                                                                
029800 01  GODK-STATUSKODER.                                                    
029900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030000     SKIP3                                                                
030100 01  SSA1                        PIC X(96).                               
030200     EJECT                                                                
032600*    --- IMS FUNKTIONSKODER                                               
032700*01  -COPY W0003                                                          
032900     EJECT                                                                
033000*    ---  DLI INPUT-OUTPUT AREA                                           
033100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
033200     SKIP3                                                                
033300 01  DLI-IO-AREA1.                                                        
033400     03  WLPROC01.                                                        
033500*        05  -COPY WDE801                                                 
033700     EJECT                                                                
033710 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
033720     SKIP3                                                                
033730 01  DLI-IO-AREA2.                                                        
033740     03  WLPROD01.                                                        
033750*        05  -COPY WDE901                                                 
033760     EJECT                                                                
042300 LINKAGE SECTION.                                                         
042400                                                                          
042500*01  -COPY W0009      -PRE MSG-                                           
042700     EJECT                                                                
042800*01  -COPY W0009      -PRE ALT1-                                          
043000     EJECT                                                                
043200*01  -COPY W0008      -PRE PROC-                                          
043400     05  FILLER                  PIC X.                                   
043500     EJECT                                                                
043600*01  -COPY W0008      -PRE PROD-                                          
043800     05  FILLER                  PIC X.                                   
043900     EJECT                                                                
050100 PROCEDURE DIVISION  USING MSG-PCB                                        
050200                           ALT1-PCB                                       
050300                           PROC-PCB                                       
050400                           PROD-PCB.                                      
051600                                                                          
051700     ENTRY 'DLITCBL' USING MSG-PCB                                        
051800                           ALT1-PCB                                       
051900                           PROC-PCB                                       
052000                           PROD-PCB.                                      
053300                                                                          
053400     PERFORM IMS-GET-MSG                                                  
053500     IF SEGMENT-FINNS                                                     
053600        PERFORM A-INIT                                                    
053900        IF ALLT-OK                                                        
054000           PERFORM B-LAES-PROFORMAHUVUD                                   
054100           IF ALLT-OK                                                     
054200              PERFORM C-BEHANDLA-PROFORMARADER                            
054300              PERFORM D-UPPDAT-PROFORMAHUVUD                              
054500              PERFORM E-SKICKA-IMSTRANS                                   
054600           END-IF                                                         
055300        END-IF                                                            
055500     END-IF                                                               
055600                                                                          
055700     MOVE ZERO TO RETURN-CODE                                             
055800     GOBACK                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 A-INIT SECTION.                                                          
056200                                                                          
056300     IF MSG-KDTRANS-1  NOT = 'W4T296X '                                   
056400        MOVE NEJ TO ALLT-SW                                               
056500     END-IF                                                               
056600                                                                          
057000     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
057100     IF NOT GODK-MID                                                      
057200        MOVE NEJ TO ALLT-SW                                               
057300     END-IF                                                               
057400                                                                          
057500     IF ALLT-OK                                                           
057600        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I29601                  
057700     END-IF                                                               
057800                                                                          
058500     ACCEPT WS-DATUM   FROM DATE                                          
058600     ACCEPT WS-KLOCKAN FROM TIME                                          
058700     .                                                                    
058800     EJECT                                                                
061800 B-LAES-PROFORMAHUVUD SECTION.                                            
061900                                                                          
062000     MOVE MID-IDDISTR   TO W-E801KY-IDDISTR                               
062100     MOVE MID-IDKUNDNR  TO W-E801KY-IDKUNDNR                              
062200     MOVE MID-IDKUNDRF  TO W-E801KY-IDKUNDRF                              
062300                                                                          
062400     PERFORM IMS-GHU-PROC-WLPROC01                                        
062500                                                                          
062600     IF SEGMENT-FINNS                                                     
062700        IF MID-NYCKEL-GRP NOT = LOW-VALUE                                 
062701           MOVE MID-NYCKEL-GRP TO W-E901KY-X                              
062702           PERFORM IMS-GU-PROD-WLPROD01                                   
062703        END-IF                                                            
062710     ELSE                                                                 
062720        MOVE NEJ TO ALLT-SW                                               
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063020 C-BEHANDLA-PROFORMARADER SECTION.                                        
063030                                                                          
063040     MOVE LOW-VALUE    TO W-E901KY-MIN-X                                  
063050     MOVE HIGH-VALUE   TO W-E901KY-MAX-X                                  
063060     MOVE PHUV-IDORDER TO W-E901KY-MIN-IDORDER                            
063061                          W-E901KY-MAX-IDORDER                            
063070                                                                          
063100     PERFORM UNTIL ORAD-IX > MAX-ORAD                                     
063110        PERFORM CA-LAES-PROFORMARAD                                       
063120        IF SEGMENT-FINNS                                                  
063130           PERFORM CB-JUSTERA-PRIS                                        
063131           ADD 1    TO ORAD-IX                                            
063140        ELSE                                                              
063150           MOVE 999 TO ORAD-IX                                            
063160        END-IF                                                            
063170     END-PERFORM                                                          
063200     .                                                                    
063300     EJECT                                                                
063400 CA-LAES-PROFORMARAD SECTION.                                             
063500                                                                          
063510     PERFORM IMS-GHN-PROD-WLPROD01                                        
063520                                                                          
063530     IF SEGMENT-SAKNAS                                                    
063531        OR                                                                
063532        BASEN-SLUT                                                        
063540        MOVE JA TO PROFORMA-SW                                            
063550     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 CB-JUSTERA-PRIS SECTION.                                                 
063900                                                                          
064000     IF PRAD-KDPRTYP NOT = 'P'                                            
064010        MOVE 'N'             TO MANUELLA-PRISER-SW                        
064011        COMPUTE PRAD-PRARTNTO ROUNDED =                                   
064020                PRAD-PRBPRIS * MID-REOMRTAL                               
064030        END-COMPUTE                                                       
064100        IF PRAD-PRARTNTO = ZERO                                           
064101           MOVE PRAD-PRBPRIS TO PRAD-PRARTNTO                             
064120        END-IF                                                            
064510        PERFORM IMS-REPL-PROD-WLPROD01                                    
064600     END-IF                                                               
064610                                                                          
064611     IF DIST79-DEALER-PRICE                                               
064612       IF PRAD-PRARTNTO-LOC > 0                                           
064620         COMPUTE WS-RADBELOPP = PRAD-PRARTNTO-LOC                         
064630                                            * PRAD-KVBEART-Q              
064632       ELSE                                                               
064633         IF PRAD-PRARTNTO-LOCPREL > 0                                     
064634           COMPUTE WS-RADBELOPP = PRAD-PRARTNTO-LOCPREL                   
064635                                            * PRAD-KVBEART-Q              
064636         END-IF                                                           
064637       END-IF                                                             
064638     ELSE                                                                 
064639       COMPUTE WS-RADBELOPP = PRAD-PRARTNTO                               
064640                                            * PRAD-KVBEART-Q              
064641     END-IF                                                               
064650     ADD WS-RADBELOPP TO MID-SUORDV                                       
064660     ADD WS-RADBELOPP TO MID-SUORDV-LOC                                   
064670     ADD WS-RADBELOPP TO MID-SUORDV-LOCPREL                               
064700     .                                                                    
064701     EJECT                                                                
064710 D-UPPDAT-PROFORMAHUVUD SECTION.                                          
064720                                                                          
064730     IF PROFORMA-KLAR                                                     
064740        IF PHUV-PREMBHNT NOT = +0                                         
064741          IF NOT MANUELLA-PRISER                                          
064742            IF DIST79-DEALER-PRICE                                        
064743              IF MID-SUORDV-LOC > 0                                       
064744                COMPUTE WS-RADBELOPP = (((MID-SUORDV-LOC                  
064745                                      - PHUV-PRAVDRAG)                    
064746                                      * PHUV-REEMBHNT)                    
064747                                      / 100)                              
064748              ELSE                                                        
064749                IF MID-SUORDV-LOCPREL > 0                                 
064750                  COMPUTE WS-RADBELOPP = (((MID-SUORDV-LOCPREL            
064751                                      - PHUV-PRAVDRAG)                    
064752                                      * PHUV-REEMBHNT)                    
064753                                      / 100)                              
064754                END-IF                                                    
064755              END-IF                                                      
064756            ELSE                                                          
064757              COMPUTE WS-RADBELOPP = (((MID-SUORDV                        
064758                                      - PHUV-PRAVDRAG)                    
064759                                      * PHUV-REEMBHNT)                    
064760                                      / 100)                              
064761            END-IF                                                        
064763              MOVE WS-RADBELOPP TO PHUV-PREMBHNT                          
064764           END-IF                                                         
064765        END-IF                                                            
064766        MOVE MID-SUORDV         TO PHUV-SUORDV                            
064767        MOVE MID-SUORDV-LOC     TO PHUV-SUORDV-LOC                        
064768        MOVE MID-SUORDV-LOCPREL TO PHUV-SUORDV-LOCPREL                    
064769        MOVE MID-REOMRTAL       TO PHUV-REOMRTAL                          
064770        MOVE WS-DATUM           TO PHUV-TIUPPDAT                          
064771        MOVE WS-KLOCKAN         TO PHUV-TIUPPTID                          
064772        PERFORM IMS-REPL-PROC-WLPROC01                                    
064780     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 E-SKICKA-IMSTRANS SECTION.                                               
065100                                                                          
065200     IF PROFORMA-KLAR                                                     
065300        CONTINUE                                                          
065400     ELSE                                                                 
065416        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
065420        MOVE 'W4T296X '     TO MSG-KDTRANS-1                              
065421        MOVE '4296'         TO MSG-IDTRANS-1                              
065422        MOVE +1             TO MSG-KDMFSFOR-1                             
065423        MOVE PRAD-IDORDER   TO MID-IDORDER                                
065425        MOVE PRAD-IDARTNR   TO MID-IDARTNR                                
065426        MOVE PRAD-IDLOPNR   TO MID-IDLOPNR                                
065430        MOVE MID-W4I29601   TO MSG-INDATA-MINUS-1-TRANSKOD                
065440        PERFORM IMS-ISRT-MSG-ALT1                                         
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
168600* --- IMS SEKTIONER ---                                                   
168700                                                                          
168800 IMS-GET-MSG SECTION.                                                     
168900                                                                          
169000     MOVE '  QC' TO GODK-STATUSKODER                                      
169100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
169200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
169300     PERFORM IMS-STATUSKONTROLL                                           
169400     .                                                                    
169600                                                                          
169700 IMS-ISRT-MSG-ALT1 SECTION.                                               
169800                                                                          
169900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
170000     MOVE SPACE TO GODK-STATUSKODER                                       
170100     CALL CBLTDLI USING ISRT ALT1-PCB MSG-IO-AREA                         
170200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
170300     PERFORM IMS-STATUSKONTROLL                                           
170400     .                                                                    
170500     EJECT                                                                
170700 IMS-GHU-PROC-WLPROC01 SECTION.                                           
170800                                                                          
170810     STRING 'WLPROC01(WDE801KY =' W-E801KY-X ')'                          
170900          DELIMITED BY SIZE INTO SSA1                                     
171000     MOVE '  GE' TO GODK-STATUSKODER                                      
171100     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA1 SSA1                    
171200     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSKONTROLL                                           
171400     .                                                                    
171401                                                                          
171402                                                                          
171410 IMS-REPL-PROC-WLPROC01 SECTION.                                          
171420                                                                          
171450     MOVE '    ' TO GODK-STATUSKODER                                      
171460     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA1                        
171470     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
171480     PERFORM IMS-STATUSKONTROLL                                           
171490     .                                                                    
171500     EJECT                                                                
171510 IMS-GU-PROD-WLPROD01 SECTION.                                            
171520                                                                          
171530     STRING 'WLPROD01(WDE901KY =' W-E901KY-X ')'                          
171540          DELIMITED BY SIZE INTO SSA1                                     
171550     MOVE '    ' TO GODK-STATUSKODER                                      
171560     CALL CBLTDLI USING GU PROD-PCB DLI-IO-AREA2 SSA1                     
171570     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
171580     PERFORM IMS-STATUSKONTROLL                                           
171590     .                                                                    
171591                                                                          
171592                                                                          
171593 IMS-GHN-PROD-WLPROD01 SECTION.                                           
171594                                                                          
171595     STRING 'WLPROD01(WDE901KY>=' W-E901KY-MIN-X                          
171596                    '&WDE901KY<=' W-E901KY-MAX-X ')'                      
171597          DELIMITED BY SIZE INTO SSA1                                     
171598     MOVE '  GEGB' TO GODK-STATUSKODER                                    
171599     CALL CBLTDLI USING GHN PROD-PCB DLI-IO-AREA2 SSA1                    
171600     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
171601     PERFORM IMS-STATUSKONTROLL                                           
171602     .                                                                    
171603                                                                          
171604                                                                          
171605 IMS-REPL-PROD-WLPROD01 SECTION.                                          
171606                                                                          
171607     MOVE '    ' TO GODK-STATUSKODER                                      
171608     CALL CBLTDLI USING REPL PROD-PCB DLI-IO-AREA2                        
171609     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
171610     PERFORM IMS-STATUSKONTROLL                                           
171611     .                                                                    
171612     EJECT                                                                
201400 IMS-STATUSKONTROLL SECTION.                                              
201500                                                                          
201600     SET STATUS-IX TO 1                                                   
201700     SEARCH GODK-STATUS                                                   
201800       AT END                                                             
201900         STRING 'STATUSKOD FROM IMS ' STATUS-WS                           
202000           DELIMITED BY SIZE INTO FELTEXT                                 
202100         CALL FELLOG                                                      
202200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
202300     END-SEARCH                                                           
202400     .                                                                    
