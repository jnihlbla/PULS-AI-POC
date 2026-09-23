000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6020110.                                                
000400*AUTHOR.         GERRY  CARMICHAEL / ARCHANA BHAT.                        
000500*DATE-WRITTEN.   91/10/16 / MAY 2012.                                     
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN FRÅGEBILD SOM VISAR                             
001100*        INFORMATION OM KONTROLLRAPPORTER                                 
001200*        BEROENDE PÅ IFYLLDA NYCKLAR.HOPP                                 
001300*        KAN SKE TILL OLIKA BILDER I SERIEN                               
001400*        OM MAN ANGE KANTKOD OCH VALD PF-                                 
001500*        TANGENT.                                                         
001600*                                                                         
001800*        PROGRAMMET LÄSER      WDP3                                       
001900*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
002000*        PROGRAMMET LÄSER      W6KVAF (W6H7A1)                            
002100*        PROGRAMMET LÄSER      W6KVAG (W6H7B1)                            
002200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        REQU:   W60201I1                                                 
002600*                                                                         
002700*    UTDATA.                                                              
002800*        RESP:   W60201O1                                                 
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W6020110'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  WS-KDKRSTA                  PIC X       VALUE SPACE.                 
004500 77  WS-FLANNULL                 PIC X       VALUE SPACE.                 
004600 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004700 77  WS-KDBEHX                   PIC X       VALUE SPACE.                 
004800 77  WS-IDTYP                    PIC X       VALUE SPACE.                 
004900 77  WS-IDFTG                    PIC X(2)    VALUE SPACE.                 
005000 77  WS-IDPERSON                 PIC X(3)    VALUE SPACE.                 
005100 77  WS-IDPERSON-NUM             PIC 9(3)    VALUE ZERO.                  
005200 77  WS-KDPERSTYP                PIC X(1)    VALUE SPACE.                 
005300 77  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
005400 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
005410 77  WS-CP-UNICODE               PIC X(4)    VALUE 'UTF8'.                
005420 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
005500*                                                                         
005600 01  ALL-SPACE.                                                           
005700     03 FILLER                   PIC X(50)   VALUE SPACE.                 
005710 01  ALL-PLUS.                                                            
005720     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
005730 01  ALL-UTF8-SPACE.                                                      
005740     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
005750 01  ALL-UTF8-PLUS.                                                       
005760     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
005770                                                                          
005900 01  TAB-IX                      PIC S9(9)   VALUE ZERO.                  
006000 01  FILLER                      PIC X(16)                                
006100                                 VALUE 'TABELL IDARTNR'.                  
006200 01  TAB-ANTAL-IDARTNR           PIC S9(9)   VALUE ZERO.                  
006300 01  TAB-MAX-IDARTNR             PIC S9(9)   VALUE 1000.                  
006400 01  TAB-IDARTNR-GRP.                                                     
006500  03 FILLER       OCCURS 1000.                                            
006600   05 TAB-IDLAND-21              PIC X(2)    VALUE SPACE.                 
006700   05 TAB-IDARTNR-FOM            PIC S9(9)   VALUE ZERO COMP-3.           
006800   05 TAB-IDARTNR-TOM            PIC S9(9)   VALUE ZERO COMP-3.           
006900   05 TAB-IDPERSON-ARTGRP        PIC 9(3)    VALUE ZERO COMP-3.           
007000*                                                                         
007100 01  FILLER                      PIC X(16)                                
007200                                 VALUE 'TABELL IDLEVNR'.                  
007300 01  TAB-ANTAL-IDLEVNR           PIC S9(9)   VALUE ZERO.                  
007400 01  TAB-MAX-IDLEVNR             PIC S9(9)   VALUE 1000.                  
007500 01  TAB-IDLEVNR-GRP.                                                     
007600  03 FILLER       OCCURS 1000.                                            
007700   05 TAB-IDLAND-22              PIC X(2)    VALUE SPACE.                 
007800   05 TAB-IDLEVNR                PIC X(5)    VALUE SPACE.                 
007900   05 TAB-IDPERSON-LEVGRP        PIC 9(3)    VALUE ZERO COMP-3.           
008000*                                                                         
008100 01  FILLER                      PIC X(16)                                
008200                                 VALUE 'TABELL IDFKNGRP'.                 
008300 01  TAB-ANTAL-IDFKNGRP          PIC S9(9)   VALUE ZERO.                  
008400 01  TAB-MAX-IDFKNGRP            PIC S9(9)   VALUE 1000.                  
008500 01  TAB-IDFKNGRP-GRP.                                                    
008600  03 FILLER       OCCURS 1000.                                            
008700   05 TAB-IDLAND-23              PIC X(2)    VALUE SPACE.                 
008800   05 TAB-IDFKNGRP-FOM           PIC S9(5)   VALUE ZERO COMP-3.           
008900   05 TAB-IDFKNGRP-TOM           PIC S9(5)   VALUE ZERO COMP-3.           
009000   05 TAB-IDPERSON-FKNGRP        PIC 9(3)    VALUE ZERO COMP-3.           
009100                                                                          
009200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
009400                                                                          
009500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009600                                                                          
009700                                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000     88  NYCKLAR-FEL                         VALUE 'N'.                   
010100                                                                          
010200 77  INFO-SW                     PIC X       VALUE 'J'.                   
010300     88  INFO-FINNS                          VALUE 'J'.                   
010400     88  INFO-SAKNAS                         VALUE 'N'.                   
010500                                                                          
010600 77  KDBEHX-SW                   PIC X       VALUE 'J'.                   
010700     88  KDBEHX-FOUND                        VALUE 'J'.                   
010800                                                                          
010900 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
011000     88  FIRST-TIME                          VALUE 'J'.                   
011100                                                                          
011200 77  KEY-KOLL                    PIC X       VALUE '1'.                   
011300     88  KDKRSTA                             VALUE '1'.                   
011400     88  IDFTG                               VALUE '2'.                   
011500     88  LOPNR                               VALUE '3'.                   
011600     88  STAT-FTG-LOPNR                      VALUE '4'.                   
011700                                                                          
011800 77  ARTNR-SW                    PIC X       VALUE 'J'.                   
011900     88  ARTNR-IFYLLT                        VALUE 'J'.                   
012000                                                                          
012100 77  STATUS-SW                   PIC X       VALUE 'J'.                   
012200     88  STATUS-IFYLLT                       VALUE 'J'.                   
012300                                                                          
012400 77  FTG-SW                      PIC X       VALUE 'J'.                   
012500     88  FTG-IFYLLT                          VALUE 'J'.                   
012600                                                                          
012700 77  LOPNR-SW                    PIC X       VALUE 'J'.                   
012800     88  LOPNR-IFYLLT                        VALUE 'J'.                   
012900                                                                          
013000 77  IDTYP-SW                    PIC X       VALUE 'J'.                   
013100     88  IDTYP-IFYLLT                        VALUE 'J'.                   
013200                                                                          
013300 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
013400     88  SW-TRAEFF-JA                        VALUE 'J'.                   
013500     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
013600                                                                          
013700 77  SW-AVBRYT                   PIC X       VALUE 'J'.                   
013800     88  SW-AVBRYT-JA                        VALUE 'J'.                   
013900     88  SW-AVBRYT-NEJ                       VALUE 'N'.                   
014000                                                                          
014100                                                                          
014200*      --- VALID IDDC CODES                                               
014300*                                                                         
014400*01    -COPY WWDC99                                                       
014410*01    -COPY WWLNDKON                                                     
014500       EJECT                                                              
014510*01  -COPY WTRAUTF8                                                       
014520*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014540*                                                                         
014600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014700 01  GENERELLA-SUBPROGRAM.                                                
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014910     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
015000     EJECT                                                                
015100*                                                                         
015200 01  MESSAGE-CODES.                                                       
015300     03  ERROR-CODES.                                                     
015400         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
015500     03  INFO-CODES.                                                      
015600         05  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.             
015700         05  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.             
015800         05  INF-PRESS-PF11          PIC X(3)    VALUE '013'.             
015900         05  INF-PART-MISSING        PIC X(3)    VALUE '025'.             
016000         05  INF-INFO-MISSING        PIC X(3)    VALUE '041'.             
016100     EJECT                                                                
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016500     SKIP3                                                                
016600*01  -COPY WMFSAREA                                                       
016700     EJECT                                                                
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900*                                                                         
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
017200     SKIP3                                                                
017300 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDDC-X.                                                        
017600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017700 03  W-IDDC-B6-X.                                                         
               05 W-IDDC-B6            PIC X(2).                                
017800*----> DIREKTNYCKEL TILL WDD301                                           
017900     03  W-IDARTNR-X.                                                     
018000         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
018100     03  W-IDLEVNR-X.                                                     
018200         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
018300     03  W-KDSEGKEY-X.                                                    
018400         05  W-KDSEGKEY          PIC X(1)   VALUE '1'.                    
018500     03  W-KDARBTYP-X.                                                    
018600         05  W-KDARBTYP          PIC X(8)   VALUE SPACE.                  
018700     03  W-IDPERSON-X.                                                    
018800         05  W-IDPERSON          PIC S9(3)  VALUE ZERO COMP-3.            
018900                                                                          
019000*----> DIREKTNYCKEL TILL WDD311                                           
019100     03  W-IDSKYLT-X.                                                     
019200         05  W-IDSKYLT           PIC  X(3)     VALUE SPACE.               
019300                                                                          
019400*----> DIREKTNYCKEL TILL W6H701                                           
019500     03  W-IDKR-X.                                                        
019600         05  W-IDKR              PIC  9(5)    VALUE ZERO.                 
019700                                                                          
019800*----> NYCKLAR TILL W6H7A1                                                
019900     03  W-W6H7A1KY-FOM-X.                                                
020000         05  W-SEQA-IDDC-FOM     PIC  X(2)   VALUE SPACE.                 
020100         05  W-SEQA-DAREGDAT-9KOMPL-FOM                                   
020200                                 PIC  9(8)   VALUE ZERO.                  
020300         05  FILLER              PIC  X(13)  VALUE LOW-VALUE.             
020400     03  W-W6H7A1KY-TOM-X.                                                
020500         05  W-SEQA-IDDC-TOM     PIC  X(2)   VALUE SPACE.                 
020600         05  W-SEQA-DAREGDAT-9KOMPL-TOM                                   
020700                                 PIC  9(8)   VALUE ZERO.                  
020800         05  FILLER              PIC  X(13)  VALUE HIGH-VALUE.            
020900     03  W-KDKRSTA-X.                                                     
021000         05  W-SEQA-KDKRSTA      PIC  X(1)   VALUE SPACE.                 
021100     03  W-IDFTG-X.                                                       
021200         05  W-SEQA-IDFTG        PIC  9(2)   VALUE ZERO.                  
021300     03  W-W6H7A1KY-DIR-X.                                                
021400         05  W-SEQA-IDDC-DIR     PIC  X(2)   VALUE SPACE.                 
021500         05  W-SEQA-DAREGDAT-9KOMPL-DIR                                   
021600                                 PIC  9(8)   VALUE ZERO.                  
021700         05  W-SEQA-KDKRSTA-DIR  PIC  X(1)   VALUE SPACE.                 
021800         05  W-SEQA-IDLOPNRM-DIR PIC S9(9)   VALUE ZERO COMP-3.           
021900         05  W-SEQA-IDFTG-DIR    PIC  9(2)   VALUE ZERO.                  
022000         05  W-SEQA-IDKR-DIR     PIC  9(5)   VALUE ZERO.                  
022100                                                                          
022200*----> NYCKLAR TILL W6H7B1                                                
022300                                                                          
022400     03  W-W6H7B1KY-FOM-X.                                                
022500         05  W-SEQB-IDARTNR-FOM  PIC S9(9)   VALUE ZERO COMP-3.           
022600         05  W-SEQB-DAREGDAT-9KOMPL-FOM                                   
022700                                 PIC  9(8)   VALUE ZERO.                  
022800         05  W-SEQB-IDLEVNR-FOM  PIC  X(5)   VALUE SPACE.                 
022900         05  W-SEQB-KVKRKNTR-FOM PIC S9(1)   VALUE ZERO COMP-3.           
023000         05  FILLER              PIC  9(5)   VALUE ZERO.                  
023100     03  W-W6H7B1KY-TOM-X.                                                
023200         05  W-SEQB-IDARTNR-TOM  PIC S9(9)   VALUE ZERO COMP-3.           
023300         05  W-SEQB-DAREGDAT-9KOMPL-TOM                                   
023400                                 PIC  9(8)   VALUE ZERO.                  
023500         05  W-SEQB-IDLEVNR-TOM  PIC  X(5)   VALUE SPACE.                 
023600         05  W-SEQB-KVKRKNTR-TOM PIC S9(1)   VALUE ZERO COMP-3.           
023700         05  FILLER              PIC  9(5)   VALUE ZERO.                  
023800     03  W-W6H7B1KY-DIR-X.                                                
023900         05  W-SEQB-IDARTNR-DIR  PIC S9(9)   VALUE ZERO COMP-3.           
024000         05  W-SEQB-DAREGDAT-9KOMPL-DIR                                   
024100                                 PIC  9(8)   VALUE ZERO.                  
024200         05  W-SEQB-IDLEVNR-DIR  PIC  X(5)   VALUE SPACE.                 
024300         05  W-SEQB-KVKRKNTR-DIR PIC S9(1)   VALUE ZERO COMP-3.           
024400         05  W-SEQB-IDKR-DIR     PIC  9(5)   VALUE ZERO.                  
024500     03  W-WDP321KY-X.                                                    
024600         05  W-IDLAND-21         PIC X(2)   VALUE SPACE.                  
024700         05  W-IDARTNRF-21       PIC S9(9)  VALUE ZERO COMP-3.            
024800         05  W-IDARTNRT-21       PIC S9(9)  VALUE ZERO COMP-3.            
024900     03  W-WDP322KY-X.                                                    
025000         05  W-IDLAND-22         PIC X(2)   VALUE SPACE.                  
025100         05  W-IDLEVNR-22        PIC X(5)   VALUE SPACE.                  
025200     03  W-WDP323KY-X.                                                    
025300         05  W-IDLAND-23         PIC X(2)   VALUE SPACE.                  
025400         05  W-IDFKNGRPF-23      PIC S9(5)  VALUE +99999 COMP-3.          
025500         05  W-IDFKNGRPT-23      PIC S9(5)  VALUE +99999 COMP-3.          
025600     SKIP2                                                                
025700*    --- STATUS-KOD FRÅN IMS                                              
025800 01  STATUS-WS                   PIC XX.                                  
025900     88  SEGMENT-FINNS                       VALUE '  '.                  
026000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026200     88  BASEN-SLUT                          VALUE 'GB'.                  
026300     SKIP2                                                                
026400 01  GODK-STATUSKODER.                                                    
026500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026600     SKIP3                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'SSA1'.                
026800 01  SSA1                        PIC X(128).                              
026900 01  FILLER                      PIC X(16)   VALUE 'SSA2'.                
027000 01  SSA2                        PIC X(64).                               
027100 01  FILLER                      PIC X(16)   VALUE 'SSA3'.                
027200 01  SSA3                        PIC X(64).                               
027300     EJECT                                                                
027400*    --- IMS FUNKTIONSKODER                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700*    ---  DLI INPUT-OUTPUT AREA                                           
027800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027900     SKIP3                                                                
028000 01  DLI-IO-AREA1.                                                        
028100                                                                          
028200     03  IO-AREA1                PIC X(600)  VALUE SPACE.                 
028300                                                                          
028400     03  W6KVAE01 REDEFINES IO-AREA1.                                     
028500*        05  -COPY W6H701                                                 
028600     EJECT                                                                
028700 01  DLI-IO-AREA2.                                                        
028800     03  WLBENA11.                                                        
028900*        05  -COPY WDD311                                                 
029000     EJECT                                                                
029100 01  DLI-IO-AREA3.                                                        
029200                                                                          
029300     03  IO-AREA3                PIC X(23)  VALUE SPACE.                  
029400                                                                          
029500     03  W6KVAF01 REDEFINES IO-AREA3.                                     
029600*        05  -COPY W6H7A1                                                 
029700     EJECT                                                                
029800 01  DLI-IO-AREA4.                                                        
029900                                                                          
030000     03  IO-AREA4                PIC X(24)  VALUE SPACE.                  
030100                                                                          
030200     03  W6KVAG01 REDEFINES IO-AREA4.                                     
030300*        05  -COPY W6H7B1                                                 
030400     EJECT                                                                
031000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
031100 01  DLI-IO-WDP301.                                                       
031200*    03  -COPY WDP301                                                     
031300                                                                          
031400     EJECT                                                                
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP321'.                      
031600 01  DLI-IO-WDP321.                                                       
031700*    03  -COPY WDP321                                                     
031800     EJECT                                                                
031900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP322'.                      
032000 01  DLI-IO-WDP322.                                                       
032100*    03  -COPY WDP322                                                     
032200     EJECT                                                                
032300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP323'.                      
032400 01  DLI-IO-WDP323.                                                       
032500*    03  -COPY WDP323                                                     
032600     EJECT                                                                
032700     EJECT                                                                
032800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
032900 01  DLI-IO-WDK601.                                                       
033000*    03  -COPY WDK601                                                     
033100     EJECT                                                                
033200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
033300 01  DLI-IO-WDK611.                                                       
033400*    03  -COPY WDK611                                                     
033500     EJECT                                                                
033600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
033700 01  DLI-IO-WDK722.                                                       
033800*    03  -COPY WDK722                                                     
033900     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
           EJECT                                                                
034000 LINKAGE SECTION.                                                         
034100                                                                          
034200 01  REQU-AREA.                                                           
034300*    03 -COPY WZ01REQU                                                    
034400*    03 -COPY W60201I1                                                    
034500     EJECT                                                                
034600 01  RESP-AREA.                                                           
034700*    03 -COPY WZ01RESP                                                    
034800*    03 -COPY W60201O1                                                    
034900     EJECT                                                                
035000 01  MAX-KVRADER                 PIC S9(4) COMP.                          
035100*                                                                         
035200*01  -COPY W0009  -PRE MSG-                                               
035300     EJECT                                                                
035700*01  -COPY W0008  -PRE KVAE-                                              
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE KVAF-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE KVAG-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE BENA-                                              
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008  -PRE WDP3-                                              
037000     05  WDP3-KEY-FB-AREA-KDARBTYP      PIC X(8).                         
037100     05  WDP3-KEY-FB-AREA-IDPERSON      PIC S9(3) COMP-3.                 
037200     EJECT                                                                
037300*01  -COPY W0008  -PRE WDK6-                                              
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008  -PRE WDK7-                                              
037700     05  FILLER                  PIC X.                                   
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
037800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
037900                           KVAE-PCB KVAF-PCB                              
038000                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
038100                           WDK7-PCB WDB6-PCB.                             
038200                                                                          
038300     PERFORM A-INIT                                                       
038400     PERFORM B-KOLLA-NYCKLAR                                              
038500     IF NYCKLAR-OK                                                        
038600         IF REQU-FIRST                                                    
038700           PERFORM C-FOERSTA-SIDA                                         
038800         ELSE                                                             
038900           IF REQU-NEXT                                                   
039000             PERFORM D-NAESTA-SIDA                                        
039100           ELSE                                                           
039200             IF REQU-UPDATE                                               
039300                PERFORM G-KOLLA-INPUT                                     
039400             ELSE                                                         
039500                PERFORM E-SAMMA-SIDA                                      
039600             END-IF                                                       
039700           END-IF                                                         
039800         END-IF                                                           
039900         PERFORM F-LAES-VISA-INFO                                         
040000     END-IF                                                               
040100                                                                          
040200     GOBACK                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 A-INIT SECTION.                                                          
040600                                                                          
040700     MOVE ALL '+'       TO RESP-W60201O1                                  
040800     PERFORM MFS-FORM-ATTR                                                
040900     IF REQU-IDMSGVER = '001'                                             
041000* DO IT ONLY FOR WEB!                                                     
041100       MOVE +1          TO INDX                                           
041200       PERFORM UNTIL INDX > MAX-KVRADER                                   
041300         MOVE ALL X'2B' TO RESP-BEART-LINE (INDX)                         
041400         ADD +1         TO INDX                                           
041500       END-PERFORM                                                        
041600     END-IF                                                               
041700     MOVE 001          TO RESP-IDMSGVER                                   
041800     MOVE SPACE        TO RESP-IDMSG-ERROR                                
041900                          RESP-IDMSG-INFO                                 
042000                          RESP-IDELMT-ERROR                               
042100                                                                          
042200     MOVE REQU-KVRADER TO RESP-KVRADER                                    
042300                                                                          
042400     MOVE LOW-VALUE    TO W-WDP321KY-X                                    
042500                          W-WDP322KY-X                                    
042600                          W-WDP323KY-X                                    
042700     .                                                                    
042800     EJECT                                                                
042900 B-KOLLA-NYCKLAR SECTION.                                                 
043000     MOVE REQU-IDARTNR-KEY TO WS-IDARTNR                                  
043100     IF WS-IDARTNR = ALL '+'                                              
043200       MOVE SPACE TO WS-IDARTNR                                           
043300     END-IF                                                               
043400     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
043500                                                                          
043600                                                                          
043700     MOVE JA  TO NYCKLAR-SW                                               
043800     MOVE NEJ TO ARTNR-SW                                                 
043900                 STATUS-SW                                                
044000                 FTG-SW                                                   
044100                 LOPNR-SW                                                 
044200                 IDTYP-SW                                                 
044300                                                                          
044400     MOVE LOW-VALUE       TO W-W6H7A1KY-FOM-X                             
044500                             W-W6H7A1KY-DIR-X                             
044600                             W-W6H7B1KY-FOM-X                             
044700                             W-W6H7B1KY-DIR-X                             
044800                             W-KDKRSTA-X                                  
044900                             W-IDFTG-X                                    
045000                             W-IDARTNR-X                                  
045100                             W-IDKR-X                                     
045200     MOVE HIGH-VALUE      TO W-W6H7A1KY-TOM-X                             
045300                             W-W6H7B1KY-TOM-X                             
045400                                                                          
045500     MOVE REQU-IDDC-KEY   TO W-IDDC                                       
045600                             WS-IDDC                                      
                                   W-IDDC-B6                                    
045700                                                                          
045800     IF CDC OR NDC-CN OR NDC-US                                           
045900       MOVE W-IDDC        TO W-SEQA-IDDC-FOM                              
046000                             W-SEQA-IDDC-TOM                              
046100     ELSE                                                                 
046200       MOVE NEJ           TO NYCKLAR-SW                                   
046300       MOVE 'IDDC'        TO RESP-IDELMT-ERROR                            
046400     END-IF                                                               
046500                                                                          
046600     IF NYCKLAR-OK                                                        
046700       IF NDC-CN                                                          
046701         MOVE WC-LAND-CN    TO W-IDLAND-21                                
046702       ELSE                                                               
046710         IF NDC-US                                                        
046800           MOVE WC-LAND-US  TO W-IDLAND-21                                
047110         ELSE                                                             
047120           MOVE WC-LAND-SE  TO W-IDLAND-21                                
047200         END-IF                                                           
047201       END-IF                                                             
047202       MOVE W-IDLAND-21     TO W-IDLAND-22                                
047210     END-IF                                                               
047300                                                                          
047400     IF REQU-FLANNULL-KEY = ALL '+' OR SPACE                              
047500        MOVE SPACE                TO WS-FLANNULL                          
047600     ELSE                                                                 
047700       IF REQU-FLANNULL-KEY = 'J' OR 'Y' OR 'X'                           
047800         MOVE   JA                TO WS-FLANNULL                          
047900       ELSE                                                               
048000         IF REQU-FLANNULL-KEY = 'N'                                       
048100           MOVE  NEJ              TO WS-FLANNULL                          
048200         ELSE                                                             
048300           MOVE REQU-FLANNULL-KEY TO WS-FLANNULL                          
048400           MOVE 'FLANNULL'        TO RESP-IDELMT-ERROR                    
048500           MOVE NEJ               TO NYCKLAR-SW                           
048600         END-IF                                                           
048700       END-IF                                                             
048800     END-IF                                                               
048900                                                                          
049000     MOVE REQU-KDKRSTA-KEY  TO WS-KDKRSTA                                 
049100     IF WS-KDKRSTA = ALL '+'                                              
049200       MOVE '0' TO WS-KDKRSTA                                             
049300     END-IF                                                               
049400     IF WS-KDKRSTA NUMERIC                                                
049500       MOVE WS-KDKRSTA      TO W-SEQA-KDKRSTA                             
049600       MOVE JA              TO STATUS-SW                                  
049700       MOVE '1'             TO KEY-KOLL                                   
049800     ELSE                                                                 
049900       MOVE 'KDKRSTA'       TO RESP-IDELMT-ERROR                          
050000       MOVE NEJ             TO NYCKLAR-SW                                 
050100     END-IF                                                               
050200                                                                          
050300     IF WS-IDARTNR NUMERIC                                                
050400       IF WS-IDARTNR = ZERO                                               
050500         MOVE NEJ            TO ARTNR-SW                                  
050600       ELSE                                                               
050700         MOVE WS-IDARTNR     TO W-SEQB-IDARTNR-FOM                        
050800                                W-SEQB-IDARTNR-TOM                        
050900         MOVE JA             TO ARTNR-SW                                  
051000       END-IF                                                             
051100     ELSE                                                                 
051200       MOVE NEJ              TO NYCKLAR-SW                                
051300       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                         
051400     END-IF                                                               
051500                                                                          
051600     MOVE REQU-KDPERSTYP-KEY TO WS-KDPERSTYP                              
051700     IF WS-KDPERSTYP = ALL '+'                                            
051800       MOVE SPACE TO WS-KDPERSTYP                                         
051900     END-IF                                                               
052000     IF WS-KDPERSTYP = SPACE                                              
052100     OR WS-KDPERSTYP = 'A'                                                
052200     OR WS-KDPERSTYP = 'Q'                                                
052300       CONTINUE                                                           
052400     ELSE                                                                 
052500       MOVE NEJ              TO NYCKLAR-SW                                
052600       MOVE 'KDPERSTYP'      TO RESP-IDELMT-ERROR                         
052700     END-IF                                                               
052800                                                                          
052900     IF WS-KDPERSTYP = 'Q'                                                
053000       MOVE 'QUAL'           TO W-KDARBTYP                                
053100     END-IF                                                               
053200                                                                          
053300     MOVE REQU-IDPERSON-KEY TO WS-IDPERSON                                
053400     IF WS-IDPERSON = ALL '+'                                             
053500       MOVE SPACE TO WS-IDPERSON                                          
053600     END-IF                                                               
053700     INSPECT WS-IDPERSON REPLACING LEADING SPACE BY ZERO                  
053800     IF WS-IDPERSON NUMERIC                                               
053900       MOVE WS-IDPERSON      TO W-IDPERSON                                
054000                                WS-IDPERSON-NUM                           
054100     ELSE                                                                 
054200       MOVE NEJ              TO NYCKLAR-SW                                
054300       MOVE 'IDPERSON'       TO RESP-IDELMT-ERROR                         
054400     END-IF                                                               
054500                                                                          
054600     MOVE REQU-KDBEHX-KEY   TO WS-KDBEHX                                  
054700     IF WS-KDBEHX = ALL '+'                                               
054800       MOVE SPACE TO WS-KDBEHX                                            
054900     END-IF                                                               
055000     INSPECT WS-KDBEHX REPLACING LEADING SPACE BY ZERO                    
055100     IF WS-KDBEHX = ZERO                                                  
055200       MOVE NEJ              TO LOPNR-SW                                  
055300     ELSE                                                                 
055400       IF WS-KDBEHX NOT = SPACE                                           
055500         IF WS-KDBEHX = 'I' OR (WS-KDBEHX = 'L' OR 'S')                   
055600           MOVE JA           TO LOPNR-SW                                  
055700           MOVE '3'          TO KEY-KOLL                                  
055800         ELSE                                                             
055900           MOVE NEJ          TO NYCKLAR-SW                                
056000           MOVE 'KDBEHX'     TO RESP-IDELMT-ERROR                         
056100         END-IF                                                           
056200       END-IF                                                             
056300     END-IF                                                               
056400                                                                          
056500     MOVE REQU-IDTYP-KEY     TO WS-IDTYP                                  
056600     IF WS-IDTYP  = ALL '+'                                               
056700       MOVE SPACE TO WS-IDTYP                                             
056800     END-IF                                                               
056900     INSPECT WS-IDTYP REPLACING LEADING SPACE BY ZERO                     
057000     IF WS-IDTYP = ZERO                                                   
057100       MOVE NEJ              TO IDTYP-SW                                  
057200     ELSE                                                                 
057300       IF WS-IDTYP NOT = SPACE                                            
057400         IF WS-IDTYP = 'A' OR WS-IDTYP = 'T'                              
057500           MOVE JA           TO IDTYP-SW                                  
057600         ELSE                                                             
057700           MOVE NEJ          TO NYCKLAR-SW                                
057800           MOVE 'IDTYP'      TO RESP-IDELMT-ERROR                         
057900         END-IF                                                           
058000       END-IF                                                             
058100     END-IF                                                               
058200                                                                          
058810     EVALUATE REQU-IDSPRAK                                                
058820       WHEN 'ZH'                                                          
058830        MOVE 'RCN'           TO W-IDSKYLT                                 
058840        MOVE WS-CP-UNICODE   TO TRAUTF8-KDCP                              
058850       WHEN 'SV'                                                          
058860        MOVE 'S  '           TO W-IDSKYLT                                 
058870        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
058880       WHEN OTHER                                                         
058890        MOVE 'GB '           TO W-IDSKYLT                                 
058891        MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                              
058892     END-EVALUATE                                                         
058893*                                                                         
058900     IF NYCKLAR-FEL                                                       
059000       MOVE ERR-INVALID-KEY   TO RESP-IDMSG-ERROR                         
059100       MOVE ZERO              TO RESP-KVRADER                             
           ELSE                                                                 
             PERFORM IMS-GU-WDB601                                              
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500 C-FOERSTA-SIDA SECTION.                                                  
059600                                                                          
059700     MOVE INF-FIRST-PAGE      TO RESP-IDMSG-INFO                          
059800     PERFORM MFS-RENSA-FAELT-IN                                           
059900     .                                                                    
060000     EJECT                                                                
060100 D-NAESTA-SIDA SECTION.                                                   
060200                                                                          
060300     MOVE REQU-IDDC-START     TO W-SEQA-IDDC-FOM                          
060400                                 W-SEQA-IDDC-TOM                          
060500                                 W-SEQA-IDDC-DIR                          
060600                                                                          
060700     MOVE REQU-KDKRSTA-START  TO W-SEQA-KDKRSTA                           
060800                                 W-SEQA-KDKRSTA-DIR                       
060900                                                                          
061000     MOVE REQU-IDLOPNRM-START TO W-SEQA-IDLOPNRM-DIR                      
061100     MOVE REQU-TIREGDAT-START TO W-SEQA-DAREGDAT-9KOMPL-DIR               
061200     IF REQU-TIREGDAT-START NOT = ZERO                                    
061300       IF REQU-TIREGDAT-START < 500000                                    
061400         MOVE 20              TO W-SEQA-DAREGDAT-9KOMPL-DIR (1:2)         
061500       ELSE                                                               
061600         IF REQU-TIREGDAT-START < 999999                                  
061700           MOVE 19            TO W-SEQA-DAREGDAT-9KOMPL-DIR (1:2)         
061800         ELSE                                                             
061900           MOVE 99999999      TO W-SEQA-DAREGDAT-9KOMPL-DIR               
062000         END-IF                                                           
062100       END-IF                                                             
062200     END-IF                                                               
062300     COMPUTE W-SEQA-DAREGDAT-9KOMPL-DIR =                                 
062400                   99999999 - W-SEQA-DAREGDAT-9KOMPL-DIR                  
062500                                                                          
062600     MOVE REQU-IDFTG-START    TO W-SEQA-IDFTG                             
062700                                 W-SEQA-IDFTG-DIR                         
062800                                                                          
062900     MOVE REQU-IDARTNR-START  TO W-SEQB-IDARTNR-FOM                       
063000                                 W-SEQB-IDARTNR-TOM                       
063100                                 W-SEQB-IDARTNR-DIR                       
063200                                                                          
063300     MOVE REQU-DAREGDAT-9KOMPL-START                                      
063400                              TO W-SEQB-DAREGDAT-9KOMPL-DIR               
063500                                                                          
063600     MOVE REQU-IDLEVNR-START  TO W-SEQB-IDLEVNR-DIR                       
063700     MOVE REQU-KVKRKNTR-START TO W-SEQB-KVKRKNTR-DIR                      
063800                                                                          
063900     MOVE REQU-IDKR-START     TO W-IDKR                                   
064000                                 W-SEQA-IDKR-DIR                          
064100                                 W-SEQB-IDKR-DIR                          
064200     .                                                                    
064300     EJECT                                                                
064400 E-SAMMA-SIDA SECTION.                                                    
064500                                                                          
064600     MOVE REQU-IDDC-START     TO W-SEQA-IDDC-FOM                          
064700                                 W-SEQA-IDDC-TOM                          
064800                                 W-SEQA-IDDC-DIR                          
064900                                                                          
065000     MOVE REQU-TIREGDAT-START TO W-SEQA-DAREGDAT-9KOMPL-DIR               
065100     IF REQU-TIREGDAT-START NOT = ZERO                                    
065200       IF REQU-TIREGDAT-START < 500000                                    
065300         MOVE 20              TO W-SEQA-DAREGDAT-9KOMPL-DIR (1:2)         
065400       ELSE                                                               
065500         IF REQU-TIREGDAT-START < 999999                                  
065600           MOVE 19            TO W-SEQA-DAREGDAT-9KOMPL-DIR (1:2)         
065700         ELSE                                                             
065800           MOVE 99999999      TO W-SEQA-DAREGDAT-9KOMPL-DIR               
065900         END-IF                                                           
066000       END-IF                                                             
066100     END-IF                                                               
066200     COMPUTE W-SEQA-DAREGDAT-9KOMPL-DIR =                                 
066300             99999999 - W-SEQA-DAREGDAT-9KOMPL-DIR                        
066400                                                                          
066500     MOVE REQU-KDKRSTA-START  TO W-SEQA-KDKRSTA                           
066600                                 W-SEQA-KDKRSTA-DIR                       
066700                                                                          
066800     MOVE REQU-IDLOPNRM-START TO W-SEQA-IDLOPNRM-DIR                      
066900     MOVE ZERO                TO W-SEQA-IDFTG                             
067000                                 W-SEQA-IDFTG-DIR                         
067100                                                                          
067200     MOVE REQU-IDARTNR-START  TO W-SEQB-IDARTNR-FOM                       
067300                                 W-SEQB-IDARTNR-TOM                       
067400                                 W-SEQB-IDARTNR-DIR                       
067500                                                                          
067600     MOVE REQU-IDKR-START     TO W-IDKR                                   
067700                                 W-SEQA-IDKR-DIR                          
067800                                 W-SEQB-IDKR-DIR                          
067900     MOVE REQU-DAREGDAT-9KOMPL-START                                      
068000                              TO W-SEQB-DAREGDAT-9KOMPL-DIR               
068100                                                                          
068200     MOVE REQU-IDLEVNR-START  TO W-SEQB-IDLEVNR-DIR                       
068300     MOVE REQU-KVKRKNTR-START TO W-SEQB-KVKRKNTR-DIR                      
068400                                                                          
068500     MOVE +1                     TO INDX                                  
068600     PERFORM UNTIL INDX          >  REQU-KVRADER                          
068700       IF REQU-KDBEHX-UPDATE-LINE(INDX) = ALL '+'                         
068800         MOVE SPACE              TO RESP-KDBEHX-UPDATE-LINE(INDX)         
068900       ELSE                                                               
069000         MOVE INF-PRESS-PF11     TO RESP-IDMSG-INFO                       
069100         MOVE SPACES             TO RESP-IDELMT-ERROR                     
069200         PERFORM EA-REQU-INDATA-TILL-RESP                                 
069300                                                                          
069400* EFTER EA- SKA DENNA LOOP AVSLUTAS                                       
069500         MOVE +500               TO INDX                                  
069600       END-IF                                                             
069700                                                                          
069800       ADD +1                    TO INDX                                  
069900     END-PERFORM                                                          
070000     .                                                                    
070100     EJECT                                                                
070200 EA-REQU-INDATA-TILL-RESP SECTION.                                        
070300                                                                          
070400     MOVE +1                      TO INDX                                 
070500     PERFORM UNTIL INDX > REQU-KVRADER                                    
070600       IF REQU-KDBEHX-UPDATE-LINE(INDX) NOT = ALL '+'                     
070700         MOVE REQU-KDBEHX-UPDATE-LINE(INDX)                               
070800                                  TO RESP-KDBEHX-UPDATE-LINE(INDX)        
070900         MOVE MFS-ADD-LAES-IN-FAELT                                       
071000                                  TO RESP-KDBEHX-LINE-ATTR(INDX)          
071100       ELSE                                                               
071200         MOVE MFS-RENSA-FAELT     TO RESP-KDBEHX-LINE-ATTR(INDX)          
071300       END-IF                                                             
071400                                                                          
071500       ADD +1                     TO INDX                                 
071600     END-PERFORM                                                          
071700     .                                                                    
071800     EJECT                                                                
071900 F-LAES-VISA-INFO SECTION.                                                
072000                                                                          
072100     MOVE 0                  TO RESP-KVRADER                              
072200     IF WS-KDPERSTYP = 'Q'                                                
072300                                                                          
072400       PERFORM IMS-GU-P311                                                
072500       IF SEGMENT-FINNS                                                   
072600                                                                          
072700         PERFORM IMS-GNP-P321                                             
072800         MOVE 1              TO TAB-IX                                    
072900         MOVE ZERO           TO TAB-ANTAL-IDARTNR                         
073000                                                                          
073100         PERFORM UNTIL SEGMENT-SAKNAS                                     
073200         OR TAB-IX > TAB-MAX-IDARTNR                                      
073300                                                                          
073400           MOVE IART-IDLANDX2 TO TAB-IDLAND-21   (TAB-IX)                 
073500           MOVE IART-IDARTNR-FOM                                          
073600                             TO TAB-IDARTNR-FOM (TAB-IX)                  
073700           MOVE IART-IDARTNR-TOM                                          
073800                             TO TAB-IDARTNR-TOM (TAB-IX)                  
073900           MOVE WS-IDPERSON-NUM TO TAB-IDPERSON-ARTGRP (TAB-IX)           
074000           MOVE TAB-IX       TO TAB-ANTAL-IDARTNR                         
074100           ADD 1             TO TAB-IX                                    
074200           PERFORM IMS-GNP-P321                                           
074300         END-PERFORM                                                      
074400                                                                          
074500         PERFORM IMS-GNP-P322                                             
074600         MOVE 1              TO TAB-IX                                    
074700         MOVE ZERO           TO TAB-ANTAL-IDLEVNR                         
074800                                                                          
074900         PERFORM UNTIL SEGMENT-SAKNAS                                     
075000         OR TAB-IX > TAB-MAX-IDLEVNR                                      
075100                                                                          
075200           MOVE ILEV-IDLANDX2 TO TAB-IDLAND-22 (TAB-IX)                   
075300           MOVE ILEV-IDLEVNR  TO TAB-IDLEVNR (TAB-IX)                     
075400           MOVE WS-IDPERSON-NUM TO TAB-IDPERSON-LEVGRP (TAB-IX)           
075500           MOVE TAB-IX       TO TAB-ANTAL-IDLEVNR                         
075600           ADD 1             TO TAB-IX                                    
075700           PERFORM IMS-GNP-P322                                           
075800         END-PERFORM                                                      
075900                                                                          
076000         PERFORM IMS-GNP-P323                                             
076100         MOVE 1              TO TAB-IX                                    
076200         MOVE ZERO           TO TAB-ANTAL-IDFKNGRP                        
076300                                                                          
076400         PERFORM UNTIL SEGMENT-SAKNAS                                     
076500         OR TAB-IX > TAB-MAX-IDFKNGRP                                     
076600                                                                          
076700           MOVE IFKN-IDLANDX2                                             
076800                             TO TAB-IDLAND-23    (TAB-IX)                 
076900           MOVE IFKN-IDFKNGRP-FOM                                         
077000                             TO TAB-IDFKNGRP-FOM (TAB-IX)                 
077100           MOVE IFKN-IDFKNGRP-TOM                                         
077200                             TO TAB-IDFKNGRP-TOM (TAB-IX)                 
077300           MOVE WS-IDPERSON-NUM TO TAB-IDPERSON-FKNGRP (TAB-IX)           
077400           MOVE TAB-IX       TO TAB-ANTAL-IDFKNGRP                        
077500           ADD 1             TO TAB-IX                                    
077600           PERFORM IMS-GNP-P323                                           
077700         END-PERFORM                                                      
077800                                                                          
077900       END-IF                                                             
078000     END-IF                                                               
078100                                                                          
078200     MOVE +1 TO INDX                                                      
078300     MOVE NEJ TO INFO-SW                                                  
078400                                                                          
078500     IF ARTNR-IFYLLT                                                      
078600       PERFORM FA-LAES-KR-BSEQ                                            
078700     ELSE                                                                 
078800       PERFORM FB-LAES-KR-ASEQ                                            
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 FA-LAES-KR-BSEQ SECTION.                                                 
079300     IF NOT REQU-FIRST                                                    
079400       PERFORM IMS-GU-KVAG-DIREKT                                         
079500     ELSE                                                                 
079600       PERFORM IMS-GU-KVAG-SEQB                                           
079700     END-IF                                                               
079800     IF SEGMENT-FINNS                                                     
079900       PERFORM FAA-SPAR-START-SEQB                                        
080000       PERFORM FAB-SPAR-NEXT-SEQB                                         
080100       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
080200         INDX > MAX-KVRADER                                               
080300         MOVE SEQB-IDKR      TO W-IDKR                                    
080400         PERFORM IMS-GU-KVAE-DIREKT                                       
080500         IF SEGMENT-FINNS                                                 
080600           IF KR-FLANNULL = WS-FLANNULL                                   
080700             IF KR-IDDC  = W-IDDC                                         
080800               IF IDTYP-IFYLLT                                            
080900                 IF WS-IDTYP = 'A'                                        
081000                   IF KR-IDKRFEL (1:1) = 'P' OR                           
081100                      KR-IDKRFEL (1:1) = 'K'                              
081200                     PERFORM FAC-LAES-OCH-REDIGERA-ART                    
081300                     PERFORM IMS-GN-KVAG-SEQB                             
081400                   ELSE                                                   
081500                     PERFORM IMS-GN-KVAG-SEQB                             
081600                   END-IF                                                 
081700                 ELSE                                                     
081800                   IF KR-IDKRFEL (1:1) NOT = 'P' AND                      
081900                      KR-IDKRFEL (1:1) NOT = 'K'                          
082000                     PERFORM FAC-LAES-OCH-REDIGERA-ART                    
082100                     PERFORM IMS-GN-KVAG-SEQB                             
082200                   ELSE                                                   
082300                     PERFORM IMS-GN-KVAG-SEQB                             
082400                   END-IF                                                 
082500                 END-IF                                                   
082600               ELSE                                                       
082700                 PERFORM FAC-LAES-OCH-REDIGERA-ART                        
082800                 PERFORM IMS-GN-KVAG-SEQB                                 
082900               END-IF                                                     
083000             ELSE                                                         
083100               PERFORM IMS-GN-KVAG-SEQB                                   
083200             END-IF                                                       
083300           ELSE                                                           
083400             PERFORM IMS-GN-KVAG-SEQB                                     
083500           END-IF                                                         
083600         END-IF                                                           
083700       END-PERFORM                                                        
083800       IF INFO-SAKNAS                                                     
083900          MOVE INF-PART-MISSING    TO RESP-IDMSG-INFO                     
084000          MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                   
084100          PERFORM MFS-RENSA-FAELT-UT                                      
084200       END-IF                                                             
084300       IF SEGMENT-FINNS                                                   
084400         PERFORM FAB-SPAR-NEXT-SEQB                                       
084500         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
084600       END-IF                                                             
084700     ELSE                                                                 
084800        MOVE INF-PART-MISSING      TO RESP-IDMSG-INFO                     
084900        MOVE 'IDARTNR'             TO RESP-IDELMT-ERROR                   
085000        PERFORM MFS-RENSA-FAELT-UT                                        
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 FAA-SPAR-START-SEQB SECTION.                                             
085500     MOVE SEQB-IDARTNR  TO RESP-IDARTNR-START                             
085600     MOVE SEQB-DAREGDAT-9KOMPL                                            
085700                        TO RESP-DAREGDAT-9KOMPL-START                     
085800     MOVE SEQB-IDLEVNR  TO RESP-IDLEVNR-START                             
085900     MOVE SEQB-KVKRKNTR TO RESP-KVKRKNTR-START                            
086000     MOVE SEQB-IDKR     TO RESP-IDKR-START                                
086100     .                                                                    
086200     EJECT                                                                
086300 FAB-SPAR-NEXT-SEQB SECTION.                                              
086400     MOVE SEQB-IDARTNR  TO RESP-IDARTNR-NEXT                              
086500     MOVE SEQB-DAREGDAT-9KOMPL                                            
086600                        TO RESP-DAREGDAT-9KOMPL-NEXT                      
086700     MOVE SEQB-IDLEVNR  TO RESP-IDLEVNR-NEXT                              
086800     MOVE SEQB-KVKRKNTR TO RESP-KVKRKNTR-NEXT                             
086900     MOVE SEQB-IDKR     TO RESP-IDKR-NEXT                                 
087000     .                                                                    
087100     EJECT                                                                
087200 FAC-LAES-OCH-REDIGERA-ART SECTION.                                       
087300     EVALUATE KEY-KOLL                                                    
087400       WHEN '1'                                                           
087500         IF KR-KDKRSTA = WS-KDKRSTA                                       
087600           PERFORM S02-KTRL-SOEKNING                                      
087700           IF SW-TRAEFF-JA                                                
087800             PERFORM S01-REDIGERA-SKRIV-BILD                              
087900             MOVE JA TO INFO-SW                                           
088000             ADD +1 TO INDX                                               
088100                       RESP-KVRADER                                       
088200           END-IF                                                         
088300         END-IF                                                           
088400       WHEN '3'                                                           
088500         IF KR-KDKRSTA = WS-KDKRSTA AND                                   
088600            ((KR-IDLOPNRM > ZERO AND  WS-KDBEHX = 'I') OR                 
088700             (KR-IDLOPNRM = ZERO AND (WS-KDBEHX = 'L' OR 'S')))           
088800               PERFORM S01-REDIGERA-SKRIV-BILD                            
088900               MOVE JA TO INFO-SW                                         
089000               ADD +1 TO INDX                                             
089100                         RESP-KVRADER                                     
089200         END-IF                                                           
089300     END-EVALUATE                                                         
089400     .                                                                    
089500     EJECT                                                                
089600 FB-LAES-KR-ASEQ SECTION.                                                 
089700     EVALUATE KEY-KOLL                                                    
089800       WHEN '1'                                                           
089900         PERFORM FBA-LAES-MED-STATUS-CLAG                                 
090000       WHEN '3'                                                           
090100         PERFORM FBC-LAES-MED-STATUS-LOPNR-CLAG                           
090200     END-EVALUATE                                                         
090300     .                                                                    
090400     EJECT                                                                
090500 FBA-LAES-MED-STATUS-CLAG       SECTION.                                  
090600     IF NOT REQU-FIRST                                                    
090700       PERFORM IMS-GU-KVAF-DIREKT                                         
090800     ELSE                                                                 
090900       PERFORM IMS-GU-KVAF-SEQA-KDKRSTA                                   
091000     END-IF                                                               
091100     IF SEGMENT-FINNS                                                     
091200       PERFORM S03-SPAR-SEQA-START                                        
091300       PERFORM S05-SPAR-SEQA-NEXT                                         
091400       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
091500         INDX > MAX-KVRADER                                               
091600         MOVE SEQA-IDKR      TO W-IDKR                                    
091700         PERFORM IMS-GU-KVAE-DIREKT                                       
091800         IF SEGMENT-FINNS                                                 
091900           IF KR-FLANNULL = WS-FLANNULL                                   
092000             IF IDTYP-IFYLLT                                              
092100               IF WS-IDTYP = 'A'                                          
092200                 IF KR-IDKRFEL (1:1) = 'P' OR                             
092300                    KR-IDKRFEL (1:1) = 'K'                                
092400                   PERFORM S02-KTRL-SOEKNING                              
092500                   IF SW-TRAEFF-JA                                        
092600                     PERFORM S01-REDIGERA-SKRIV-BILD                      
092700                     MOVE JA TO INFO-SW                                   
092800                     ADD +1 TO INDX                                       
092900                               RESP-KVRADER                               
093000                   END-IF                                                 
093100                   PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                       
093200                 ELSE                                                     
093300                   PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                       
093400                 END-IF                                                   
093500               ELSE                                                       
093600                 IF KR-IDKRFEL (1:1) NOT = 'P' AND                        
093700                    KR-IDKRFEL (1:1) NOT = 'K'                            
093800                   PERFORM S02-KTRL-SOEKNING                              
093900                   IF SW-TRAEFF-JA                                        
094000                     PERFORM S01-REDIGERA-SKRIV-BILD                      
094100                     MOVE JA TO INFO-SW                                   
094200                     ADD +1 TO INDX                                       
094300                               RESP-KVRADER                               
094400                   END-IF                                                 
094500                   PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                       
094600                 ELSE                                                     
094700                   PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                       
094800                 END-IF                                                   
094900               END-IF                                                     
095000             ELSE                                                         
095100               PERFORM S02-KTRL-SOEKNING                                  
095200               IF SW-TRAEFF-JA                                            
095300                 PERFORM S01-REDIGERA-SKRIV-BILD                          
095400                 MOVE JA TO INFO-SW                                       
095500                 ADD +1    TO INDX                                        
095600                              RESP-KVRADER                                
095700               END-IF                                                     
095800               PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                           
095900             END-IF                                                       
096000           ELSE                                                           
096100             PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                             
096200           END-IF                                                         
096300         END-IF                                                           
096400       END-PERFORM                                                        
096500       IF INFO-SAKNAS                                                     
096600         MOVE INF-INFO-MISSING     TO RESP-IDMSG-INFO                     
096700         MOVE 'INF'                TO RESP-IDELMT-ERROR                   
096800         PERFORM MFS-RENSA-FAELT-UT                                       
096900       END-IF                                                             
097000       IF SEGMENT-FINNS                                                   
097100         PERFORM S05-SPAR-SEQA-NEXT                                       
097200         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
097300       END-IF                                                             
097400     ELSE                                                                 
097500       MOVE INF-INFO-MISSING       TO RESP-IDMSG-INFO                     
097600       MOVE 'INF'                  TO RESP-IDELMT-ERROR                   
097700       PERFORM MFS-RENSA-FAELT-UT                                         
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100 FBC-LAES-MED-STATUS-LOPNR-CLAG SECTION.                                  
098200     MOVE JA TO FIRST-TIME-SW                                             
098300     IF NOT REQU-FIRST                                                    
098400       PERFORM IMS-GU-KVAF-DIREKT                                         
098500     ELSE                                                                 
098600       PERFORM IMS-GU-KVAF-SEQA-KDKRSTA                                   
098700     END-IF                                                               
098800     IF SEGMENT-FINNS                                                     
098900       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
099000         INDX > MAX-KVRADER                                               
099100         IF (SEQA-IDLOPNRM > ZERO AND  WS-KDBEHX = 'I') OR                
099200            (SEQA-IDLOPNRM = ZERO AND (WS-KDBEHX = 'L' OR 'S'))           
099300           IF FIRST-TIME                                                  
099400             PERFORM S03-SPAR-SEQA-START                                  
099500             PERFORM S05-SPAR-SEQA-NEXT                                   
099600             MOVE NEJ TO FIRST-TIME-SW                                    
099700           END-IF                                                         
099800           MOVE SEQA-IDKR    TO W-IDKR                                    
099900           PERFORM IMS-GU-KVAE-DIREKT                                     
100000           IF SEGMENT-FINNS                                               
100100             IF KR-FLANNULL = WS-FLANNULL                                 
100200               IF IDTYP-IFYLLT                                            
100300                 IF WS-IDTYP = 'A'                                        
100400                   IF KR-IDKRFEL (1:1) = 'P' OR                           
100500                      KR-IDKRFEL (1:1) = 'K'                              
100600                     PERFORM S02-KTRL-SOEKNING                            
100700                     IF SW-TRAEFF-JA                                      
100800                       PERFORM S01-REDIGERA-SKRIV-BILD                    
100900                       MOVE JA TO INFO-SW                                 
101000                       ADD +1 TO INDX                                     
101100                                 RESP-KVRADER                             
101200                     END-IF                                               
101300                     PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                     
101400                   ELSE                                                   
101500                     PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                     
101600                   END-IF                                                 
101700                 ELSE                                                     
101800                   IF KR-IDKRFEL (1:1) NOT = 'P' AND                      
101900                      KR-IDKRFEL (1:1) NOT = 'K'                          
102000                     PERFORM S02-KTRL-SOEKNING                            
102100                     IF SW-TRAEFF-JA                                      
102200                       PERFORM S01-REDIGERA-SKRIV-BILD                    
102300                       MOVE JA TO INFO-SW                                 
102400                       ADD +1 TO INDX                                     
102500                                 RESP-KVRADER                             
102600                     END-IF                                               
102700                     PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                     
102800                   ELSE                                                   
102900                     PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                     
103000                   END-IF                                                 
103100                 END-IF                                                   
103200               ELSE                                                       
103300                 PERFORM S02-KTRL-SOEKNING                                
103400                 IF SW-TRAEFF-JA                                          
103500                   PERFORM S01-REDIGERA-SKRIV-BILD                        
103600                   MOVE JA TO INFO-SW                                     
103700                   ADD +1  TO INDX                                        
103800                              RESP-KVRADER                                
103900                 END-IF                                                   
104000                 PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                         
104100               END-IF                                                     
104200             ELSE                                                         
104300               PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                           
104400             END-IF                                                       
104500           END-IF                                                         
104600         ELSE                                                             
104700           PERFORM IMS-GN-KVAF-SEQA-KDKRSTA                               
104800         END-IF                                                           
104900       END-PERFORM                                                        
105000       IF INFO-SAKNAS                                                     
105100          MOVE INF-INFO-MISSING    TO RESP-IDMSG-INFO                     
105200          MOVE 'INF'               TO RESP-IDELMT-ERROR                   
105300          PERFORM MFS-RENSA-FAELT-UT                                      
105400       END-IF                                                             
105500       IF SEGMENT-FINNS                                                   
105600         PERFORM S05-SPAR-SEQA-NEXT                                       
105700         MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                     
105800       END-IF                                                             
105900     ELSE                                                                 
106000        MOVE INF-INFO-MISSING      TO RESP-IDMSG-INFO                     
106100        MOVE 'INF'                 TO RESP-IDELMT-ERROR                   
106200        PERFORM MFS-RENSA-FAELT-UT                                        
106300     END-IF                                                               
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 G-KOLLA-INPUT SECTION.                                                   
106800     MOVE NEJ              TO KDBEHX-SW                                   
106900     MOVE +1               TO INDX                                        
107000     PERFORM UNTIL INDX > REQU-KVRADER OR KDBEHX-FOUND                    
107100       IF REQU-KDBEHX-UPDATE-LINE(INDX) NOT = ALL '+'                     
107200          MOVE JA          TO KDBEHX-SW                                   
107300          MOVE MFS-ALFA-FAELT-RAETT                                       
107400                           TO RESP-KDBEHX-LINE-ATTR(INDX)                 
107500       END-IF                                                             
107600       ADD +1              TO INDX                                        
107700     END-PERFORM                                                          
107800     .                                                                    
107900     EJECT                                                                
108000 S01-REDIGERA-SKRIV-BILD SECTION.                                         
108100     MOVE KR-IDARTNR         TO RESP-IDARTNR-LINE(INDX)                   
108200                                W-IDARTNR                                 
108300                                                                          
108310     PERFORM S01A-GET-BEART-INFO                                          
108400*'   PERFORM IMS-GU-BENA                                                  
108500**   IF SEGMENT-FINNS                                                     
108600*'     MOVE TEXT-BEART       TO RESP-BEART-LINE(INDX)                     
108700**   END-IF                                                               
108800                                                                          
108900     COMPUTE WS-DAREGDAT = 99999999 - KR-DAREGDAT-9KOMPL                  
109000     MOVE WS-DAREGDAT (3:6)  TO RESP-TIREGDAT-LINE(INDX)                  
109100     MOVE KR-IDLOPNRM        TO RESP-IDLOPNRM-LINE(INDX)                  
109200     MOVE KR-IDLEVNR         TO RESP-IDLEVNR-LINE(INDX)                   
109300                                                                          
109400     IF KR-IDKRFEL(1:1) = 'P' OR                                          
109500        KR-IDKRFEL(1:1) = 'K'                                             
109600       MOVE 'A'              TO RESP-TYP-LINE(INDX)                       
109700     ELSE                                                                 
109800       IF KR-IDKRFEL(1:1) = ' '                                           
109900         MOVE ' '            TO RESP-TYP-LINE(INDX)                       
110000       ELSE                                                               
110100         MOVE 'T'            TO RESP-TYP-LINE(INDX)                       
110200       END-IF                                                             
110300     END-IF                                                               
110400                                                                          
110500     IF KR-KVART-RET > ZERO                                               
110600       MOVE KR-KVART-RET     TO RESP-KVART-RET-LINE(INDX)                 
110700     ELSE                                                                 
110800       MOVE ALL-SPACE        TO RESP-KVART-RET-LINE(INDX)                 
110900     END-IF                                                               
111000                                                                          
111100     MOVE KR-BEKRBEH         TO RESP-BEKRBEH-LINE(INDX)                   
111200     MOVE KR-IDKR            TO RESP-IDKR-LINE(INDX)                      
111300     INSPECT RESP-IDKR-LINE(INDX) REPLACING LEADING ZERO BY SPACE         
111400     .                                                                    
111500     EJECT                                                                
111510 S01A-GET-BEART-INFO  SECTION.                                            
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
111512     PERFORM IMS-GU-BENA                                                  
111513     IF SEGMENT-FINNS                                                     
111514        MOVE TEXT-BEART        TO TRAUTF8-TECONV-FROM                     
111515     ELSE                                                                 
111516        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
111517                                   TEXT-BEART                             
111518        MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                           
111519     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-BENA                                                 
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
111520                                                                          
111521     IF REQU-IDMSGVER = '001'                                             
111522*    CALL FROM WEB AND CHINA                                              
111523*    CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE           
111524        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
111525        MOVE TRAUTF8-TECONV-TO  TO RESP-BEART-LINE(INDX)                  
111526     ELSE                                                                 
111527*    CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                         
111528        MOVE TEXT-BEART         TO RESP-BEART-LINE(INDX)                  
111529     END-IF                                                               
111531     .                                                                    
111540     EJECT                                                                
111600 S02-KTRL-SOEKNING SECTION.                                               
111700                                                                          
111800     MOVE JA                 TO SW-TRAEFF                                 
111900     IF WS-KDPERSTYP = 'Q'                                                
112000       MOVE 1                TO TAB-IX                                    
112100       MOVE NEJ              TO SW-TRAEFF                                 
112200                                SW-AVBRYT                                 
112300       MOVE KR-IDARTNR       TO W-IDARTNR                                 
112400       PERFORM IMS-GU-K601                                                
112500       IF SEGMENT-SAKNAS                                                  
112600         MOVE JA             TO SW-AVBRYT                                 
112700       END-IF                                                             
112800                                                                          
112900       PERFORM UNTIL TAB-IX > TAB-ANTAL-IDARTNR                           
113000       OR SW-TRAEFF-JA                                                    
113100       OR SW-AVBRYT-JA                                                    
113200         IF  ((KR-IDARTNR >= TAB-IDARTNR-FOM (TAB-IX)                     
113300          AND KR-IDARTNR <= TAB-IDARTNR-TOM (TAB-IX))                     
113400          AND (WS-IDLANDX2 = TAB-IDLAND-21 (TAB-IX)))                     
113500           IF WS-IDPERSON-NUM = TAB-IDPERSON-ARTGRP (TAB-IX)              
113600             MOVE JA         TO SW-TRAEFF                                 
113700           ELSE                                                           
113800             MOVE JA         TO SW-AVBRYT                                 
113900           END-IF                                                         
114000         END-IF                                                           
114100                                                                          
114200         ADD 1               TO TAB-IX                                    
114300       END-PERFORM                                                        
114400                                                                          
114500       IF SW-TRAEFF-JA                                                    
114600       OR SW-AVBRYT-JA                                                    
114700                                                                          
114800         CONTINUE                                                         
114900       ELSE                                                               
115000         MOVE 1              TO TAB-IX                                    
115100                                                                          
115200         PERFORM UNTIL TAB-IX > TAB-ANTAL-IDLEVNR                         
115300         OR SW-TRAEFF-JA                                                  
115400         OR SW-AVBRYT-JA                                                  
115500           IF KR-IDLEVNR = TAB-IDLEVNR (TAB-IX)                           
115600             AND WS-IDLANDX2 = TAB-IDLAND-22 (TAB-IX)                     
115700             IF WS-IDPERSON-NUM = TAB-IDPERSON-LEVGRP (TAB-IX)            
115800               MOVE JA       TO SW-TRAEFF                                 
115900             ELSE                                                         
116000               MOVE JA       TO SW-AVBRYT                                 
116100             END-IF                                                       
116200           END-IF                                                         
116300                                                                          
116400           ADD 1             TO TAB-IX                                    
116500         END-PERFORM                                                      
116600                                                                          
116700         IF SW-TRAEFF-JA                                                  
116800         OR SW-AVBRYT-JA                                                  
116900                                                                          
117000           CONTINUE                                                       
117100         ELSE                                                             
117200                                                                          
117300           MOVE 1            TO TAB-IX                                    
117400                                                                          
117500           PERFORM UNTIL TAB-IX > TAB-ANTAL-IDFKNGRP                      
117600           OR SW-TRAEFF-JA                                                
117700           OR SW-AVBRYT-JA                                                
117800             IF  ((ART-IDFKNGRP >= TAB-IDFKNGRP-FOM (TAB-IX)              
117900             AND ART-IDFKNGRP <= TAB-IDFKNGRP-TOM (TAB-IX))               
118000             AND (WS-IDLANDX2   = TAB-IDLAND-23    (TAB-IX)))             
118100               IF WS-IDPERSON-NUM = TAB-IDPERSON-FKNGRP (TAB-IX)          
118200                 MOVE JA     TO SW-TRAEFF                                 
118300               ELSE                                                       
118400                 MOVE JA     TO SW-AVBRYT                                 
118500               END-IF                                                     
118600             END-IF                                                       
118700                                                                          
118800             ADD 1           TO TAB-IX                                    
118900           END-PERFORM                                                    
119000         END-IF                                                           
119100       END-IF                                                             
119200     END-IF                                                               
119300                                                                          
119400     IF WS-KDPERSTYP = 'A'                                                
119500       MOVE KR-IDARTNR       TO W-IDARTNR                                 
119600       IF NDC-CN OR NDC-US                                                
119700         PERFORM IMS-GU-K722                                              
119800         IF SEGMENT-FINNS                                                 
119900         AND WS-IDPERSON-NUM = XLAG-IDANSK                                
120000             MOVE JA           TO SW-TRAEFF                               
120100         ELSE                                                             
120302           MOVE NEJ            TO SW-TRAEFF                               
120310         END-IF                                                           
120400       ELSE                                                               
120500         PERFORM IMS-GU-K611                                              
120600         IF SEGMENT-FINNS                                                 
120700         AND WS-IDPERSON-NUM = CLAG-IDANSK                                
120800           MOVE JA             TO SW-TRAEFF                               
120900         ELSE                                                             
121000           MOVE NEJ            TO SW-TRAEFF                               
121100         END-IF                                                           
121200       END-IF                                                             
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600 S03-SPAR-SEQA-START SECTION.                                             
121700     MOVE SEQA-IDDC          TO RESP-IDDC-START                           
121800     COMPUTE WS-DAREGDAT = 99999999 - SEQA-DAREGDAT-9KOMPL                
121900     MOVE WS-DAREGDAT (3:6)  TO RESP-TIREGDAT-START                       
122000     MOVE SEQA-KDKRSTA       TO RESP-KDKRSTA-START                        
122100     MOVE SEQA-IDLOPNRM      TO RESP-IDLOPNRM-START                       
122200     MOVE SEQA-IDFTG         TO RESP-IDFTG-START                          
122300     MOVE SEQA-IDKR          TO RESP-IDKR-START                           
122400     .                                                                    
122500     EJECT                                                                
122600 S05-SPAR-SEQA-NEXT SECTION.                                              
122700     MOVE SEQA-IDDC          TO RESP-IDDC-NEXT                            
122800     COMPUTE WS-DAREGDAT = 99999999 - SEQA-DAREGDAT-9KOMPL                
122900     MOVE WS-DAREGDAT (3:6)  TO RESP-TIREGDAT-NEXT                        
123000     MOVE SEQA-KDKRSTA       TO RESP-KDKRSTA-NEXT                         
123100     MOVE SEQA-IDLOPNRM      TO RESP-IDLOPNRM-NEXT                        
123200     MOVE SEQA-IDFTG         TO RESP-IDFTG-NEXT                           
123300     MOVE SEQA-IDKR          TO RESP-IDKR-NEXT                            
123400     .                                                                    
123500     EJECT                                                                
123600 MFS-RENSA-FAELT-UT SECTION.                                              
123700                                                                          
123800*    --- ALLA UTDATA-FÄLT                                                 
123900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
124000     MOVE ALL-SPACE       TO RESP-KDKRSTA-START                           
124100                             RESP-KDKRSTA-NEXT                            
124200                             RESP-IDARTNR-START                           
124300                             RESP-IDARTNR-NEXT                            
124400                             RESP-DAREGDAT-9KOMPL-START                   
124500                             RESP-DAREGDAT-9KOMPL-NEXT                    
124600                             RESP-TIREGDAT-START                          
124700                             RESP-TIREGDAT-NEXT                           
124800                             RESP-IDLOPNRM-START                          
124900                             RESP-IDLOPNRM-NEXT                           
125000                             RESP-IDFTG-START                             
125100                             RESP-IDFTG-NEXT                              
125200                             RESP-IDKR-START                              
125300                             RESP-IDKR-NEXT                               
125400                             RESP-IDLEVNR-START                           
125500                             RESP-IDLEVNR-NEXT                            
125600                             RESP-IDDC-START                              
125700                             RESP-IDDC-NEXT                               
125800*                                                                         
125900     MOVE +1              TO INDX                                         
125910     IF REQU-IDMSGVER = '001'                                             
125920       MOVE ALL-UTF8-SPACE  TO RESP-BEART-LINE(INDX)                      
125930     ELSE                                                                 
125940       MOVE ALL-SPACE       TO RESP-BEART-LINE(INDX)                      
125950     END-IF                                                               
125960                                                                          
126000     PERFORM UNTIL INDX > MAX-KVRADER                                     
126100       MOVE ALL-SPACE        TO  RESP-IDARTNR-LINE(INDX)                  
126300                                 RESP-TIREGDAT-LINE(INDX)                 
126400                                 RESP-IDKR-LINE(INDX)                     
126500                                 RESP-IDLOPNRM-LINE(INDX)                 
126600                                 RESP-IDLEVNR-LINE(INDX)                  
126700                                 RESP-TYP-LINE(INDX)                      
126800                                 RESP-KVART-RET-LINE(INDX)                
126900                                 RESP-BEKRBEH-LINE(INDX)                  
127000                                 RESP-IDKR-LINE(INDX)                     
127100       ADD +1                 TO INDX                                     
127200     END-PERFORM                                                          
127300     .                                                                    
127400     SKIP2                                                                
127500 MFS-RENSA-FAELT-IN SECTION.                                              
127600                                                                          
127700*    --- ALLA INDATA-FÄLT                                                 
127800     MOVE +1              TO INDX                                         
127900     PERFORM UNTIL INDX > MAX-KVRADER                                     
128000       MOVE SPACE         TO RESP-KDBEHX-UPDATE-LINE(INDX)                
128100       ADD +1             TO INDX                                         
128200     END-PERFORM                                                          
128300     .                                                                    
128400     EJECT                                                                
128500 MFS-FORM-ATTR SECTION.                                                   
128600                                                                          
128700*    --- ALLA INDATA-FÄLT                                                 
128800     MOVE +1                   TO INDX                                    
128900     PERFORM UNTIL INDX > MAX-KVRADER                                     
129000       MOVE MFS-FORMATETS-ATTR TO RESP-KDBEHX-LINE-ATTR(INDX)             
129100       ADD +1                  TO INDX                                    
129200     END-PERFORM                                                          
129300     .                                                                    
129400     SKIP2                                                                
129500* --- IMS SEKTIONER ---                                                   
130500 IMS-GU-BENA SECTION.                                                     
130600                                                                          
130700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
131000          DELIMITED BY SIZE INTO SSA2                                     
131100     MOVE '  GE' TO GODK-STATUSKODER                                      
131200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
131300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
131400     PERFORM IMS-STATUSKONTROLL                                           
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800 IMS-GU-KVAE-DIREKT SECTION.                                              
131900     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
132200     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA1 SSA1                     
132300     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
132400     PERFORM IMS-STATUSKONTROLL                                           
132500     .                                                                    
132600     EJECT                                                                
132700 IMS-GU-KVAF-DIREKT SECTION.                                              
132800     STRING 'W6KVAF01(W6H7A1KY =' W-W6H7A1KY-DIR-X ')'                    
132900          DELIMITED BY SIZE INTO SSA1                                     
133000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
133100     CALL CBLTDLI USING GU KVAF-PCB DLI-IO-AREA3 SSA1                     
133200     MOVE KVAF-STATUS-CODE TO STATUS-WS                                   
133300     PERFORM IMS-STATUSKONTROLL                                           
133400     .                                                                    
133500     EJECT                                                                
133600 IMS-GU-KVAG-DIREKT SECTION.                                              
133700     STRING 'W6KVAG01(W6H7B1KY =' W-W6H7B1KY-DIR-X ')'                    
133800          DELIMITED BY SIZE INTO SSA1                                     
133900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
134000     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA4 SSA1                     
134100     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
134200     PERFORM IMS-STATUSKONTROLL                                           
134300     .                                                                    
134400     EJECT                                                                
134500 IMS-GU-KVAF-SEQA-KDKRSTA SECTION.                                        
134600     STRING 'W6KVAF01(W6H7A1KY>=' W-W6H7A1KY-FOM-X                        
134700                    '&W6H7A1KY<=' W-W6H7A1KY-TOM-X                        
134800                    '&KDKRSTA  =' W-SEQA-KDKRSTA ')'                      
134900          DELIMITED BY SIZE INTO SSA1                                     
135000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
135100     CALL CBLTDLI USING GU KVAF-PCB DLI-IO-AREA3 SSA1                     
135200     MOVE KVAF-STATUS-CODE TO STATUS-WS                                   
135300     PERFORM IMS-STATUSKONTROLL                                           
135400     .                                                                    
135500     EJECT                                                                
135600 IMS-GN-KVAF-SEQA-KDKRSTA SECTION.                                        
135700     STRING 'W6KVAF01(W6H7A1KY >' W-W6H7A1KY-FOM-X                        
135800                    '&W6H7A1KY<=' W-W6H7A1KY-TOM-X                        
135900                    '&KDKRSTA  =' W-SEQA-KDKRSTA ')'                      
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
136200     CALL CBLTDLI USING GN KVAF-PCB DLI-IO-AREA3 SSA1                     
136300     MOVE KVAF-STATUS-CODE TO STATUS-WS                                   
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136600     EJECT                                                                
136700 IMS-GU-KVAG-SEQB SECTION.                                                
136800     STRING 'W6KVAG01(W6H7B1KY>=' W-W6H7B1KY-FOM-X                        
136900                    '&W6H7B1KY<=' W-W6H7B1KY-TOM-X ')'                    
137000          DELIMITED BY SIZE INTO SSA1                                     
137100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
137200     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA4 SSA1                     
137300     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
137400     PERFORM IMS-STATUSKONTROLL                                           
137500     .                                                                    
137600     EJECT                                                                
137700 IMS-GN-KVAG-SEQB SECTION.                                                
137800     STRING 'W6KVAG01(W6H7B1KY >' W-W6H7B1KY-FOM-X                        
137900                    '&W6H7B1KY<=' W-W6H7B1KY-TOM-X ')'                    
138000          DELIMITED BY SIZE INTO SSA1                                     
138100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
138200     CALL CBLTDLI USING GN KVAG-PCB DLI-IO-AREA4 SSA1                     
138300     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     EJECT                                                                
138700 IMS-GU-P311 SECTION.                                                     
138800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
138900            DELIMITED BY SIZE INTO SSA1                                   
139000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
139100            DELIMITED BY SIZE INTO SSA2                                   
139200     MOVE '  GE' TO GODK-STATUSKODER                                      
139300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP301 SSA1 SSA2               
139400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     .                                                                    
139700     SKIP3                                                                
139800 IMS-GNP-P321 SECTION.                                                    
139900     STRING 'WDP321  (WDP321KY>=' W-WDP321KY-X ')'                        
140000            DELIMITED BY SIZE INTO SSA1                                   
140100     MOVE '  GE' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP321 SSA1                   
140300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     SKIP3                                                                
140700 IMS-GNP-P322 SECTION.                                                    
140800     STRING 'WDP322  (WDP322KY>=' W-WDP322KY-X ')'                        
140900            DELIMITED BY SIZE INTO SSA1                                   
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP322 SSA1                   
141200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     SKIP3                                                                
141600 IMS-GNP-P323 SECTION.                                                    
141700     STRING 'WDP323  (WDP323KY>=' W-WDP323KY-X ')'                        
141800            DELIMITED BY SIZE INTO SSA1                                   
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP323 SSA1                   
142100     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     SKIP3                                                                
142500 IMS-GU-K601 SECTION.                                                     
142600                                                                          
142700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
142800          DELIMITED BY SIZE INTO SSA1                                     
142900     MOVE '  GE' TO GODK-STATUSKODER                                      
143000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
143100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
143200     PERFORM IMS-STATUSKONTROLL                                           
143300     .                                                                    
143400     EJECT                                                                
143500 IMS-GU-K611 SECTION.                                                     
143600                                                                          
143700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
143800          DELIMITED BY SIZE INTO SSA1                                     
143900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
144000          DELIMITED BY SIZE INTO SSA2                                     
144100     MOVE '  GE' TO GODK-STATUSKODER                                      
144200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
144300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
144400     PERFORM IMS-STATUSKONTROLL                                           
144500     .                                                                    
144600     EJECT                                                                
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
144700 IMS-GU-K722 SECTION.                                                     
144800                                                                          
144900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
145000          DELIMITED BY SIZE INTO SSA1                                     
145100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
145200          DELIMITED BY SIZE INTO SSA2                                     
145300     MOVE 'WDK722  ' TO SSA3                                              
145400     MOVE '  GE' TO GODK-STATUSKODER                                      
145500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
145600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
145700     PERFORM IMS-STATUSKONTROLL                                           
145800     .                                                                    
145900     EJECT                                                                
146000 IMS-STATUSKONTROLL SECTION.                                              
146100                                                                          
146200     SET STATUS-IX TO 1                                                   
146300     SEARCH GODK-STATUS                                                   
146400       AT END                                                             
146500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
146600         DELIMITED BY SIZE INTO FELTEXT                                   
146700         CALL FELLOG                                                      
146800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
146900     END-SEARCH                                                           
147000     .                                                                    
