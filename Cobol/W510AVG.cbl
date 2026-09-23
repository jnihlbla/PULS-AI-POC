000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W510AVG.                                                 
000500 AUTHOR.         GUN LÖFGREN.                                             
000600 DATE-WRITTEN.   96/11/10.                                                
000700 DATE-COMPILED.                                                           
000800*    FUNKTION:                                                            
000900*        RÄKNAR UT NY AVERAGE COST MED HÄNSYN TAGEN TILL                  
001000*        URSPRUNGSHÄNDELSE.                                               
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W510AVG '.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 01  W-ARBETS-AREOR.                                                      
003800                                                                          
003900     03  PRODKOD-TEST            PIC X       VALUE 'N'.                   
004000         88 PRODKOD-SAKNAS                   VALUE 'N'.                   
004100         88 PRODKOD-FINNS                    VALUE 'J'.                   
004200     03  WS-PRAVCOST             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
004300     03  WS-IND                  PIC 9(2)    VALUE ZERO.                  
004400     03  WS-MARKUP               PIC 9V9(3)  VALUE ZERO.                  
004500     03  WS-MARKUP1              PIC 9V9(3)  VALUE ZERO.                  
004600     03  WS-MARKUP2              PIC 9V9(3)  VALUE ZERO.                  
004800     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
004900     03  WS-PRKURS               PIC 9(6)V9(5)  VALUE ZERO.               
005000     03  WS-PRKURS1              PIC 9(6)V9(5)  VALUE ZERO.               
005100     03  WS-PRKURS2              PIC 9(6)V9(5)  VALUE ZERO.               
005200     03  WP-PRKURS               PIC 9(6)V9(5)  VALUE ZERO.               
005300     03  W-DATE-AAMM             PIC 9(4)    VALUE ZERO.                  
005400     03  WS-KDVALISO-HUV         PIC X(3)    VALUE 'SEK'.                 
005500                                                                          
005600     EJECT                                                                
005700*    -- VALID IDDC CODES                                                  
005800*                                                                         
005900*01  -COPY WWDC99                                                         
006000                                                                          
006100*****   REMARKUP-FAKTOR-TABELL                                            
006200*01  -COPY WWMARKUP                                                       
006300     EJECT                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
007000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
007100     SKIP2                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
007300*01 -COPY W335CURR                                                        
007400*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
007500*01  -COPY W510CURR                                                       
007600     EJECT                                                                
007700                                                                          
007800*    --- PARAMETRAR TILL ABEND                                            
007900                                                                          
008000 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008100 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700                                                                          
008800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100                                                                          
009200                                                                          
009300 01  NYCKLAR-TILL-DLI.                                                    
009400     03  W-IDDC-X.                                                        
009500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009600     03  W-KDSEGKEY-X.                                                    
009700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009800     03  W-KDPRODSL-X.                                                    
009900         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
009910     03  W-IDFKNGRP-X.                                                    
009920         05  W-IDFKNGRP          PIC S9(5)   VALUE ZERO COMP-3.           
010000                                                                          
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010700     SKIP2                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300 01  SSA3                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012000     SKIP2                                                                
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
012200 01  DLI-IO-WDB601.                                                       
012300*    03  -COPY WDB601                                                     
012400     EJECT                                                                
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
012700 01  DLI-IO-WDB617.                                                       
012800*    03  -COPY WDB617                                                     
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB621'.                      
013000 01  DLI-IO-WDB621.                                                       
013100*    03  -COPY WDB621                                                     
013110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB622'.                      
013120 01  DLI-IO-WDB622.                                                       
013130*    03  -COPY WDB622                                                     
013200     EJECT                                                                
013300                                                                          
013400 LINKAGE SECTION.                                                         
013500     SKIP2                                                                
013600 01  W510AVG-AREA.                                                        
013700*    03  -COPY W510AVG                                                    
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE 9305-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008  -PRE WDB6-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500                                                                          
014600 PROCEDURE DIVISION USING W510AVG-AREA 9305-PCB                           
014700                          WDB6-PCB.                                       
014800 MAIN SECTION.                                                            
014900                                                                          
015000     PERFORM A-INIT                                                       
015100                                                                          
015200     EVALUATE AVG-KDCALL                                                  
015300                                                                          
015400       WHEN 010                                                           
015500          PERFORM B-REFILL                                                
015600       WHEN 011                                                           
015700          PERFORM B-REFILL-SC                                             
015800       WHEN 012                                                           
015900          PERFORM B-REFILL-IN                                             
016000       WHEN 020                                                           
016100          PERFORM C-TRANSFER-US                                           
016200       WHEN 021                                                           
016300          PERFORM C-TRANSFER-SC                                           
016400       WHEN 030                                                           
016500          PERFORM C-TRANSFER-US-CANADA                                    
016600       WHEN 040                                                           
016700          PERFORM D-ST-TERMINAL                                           
016800       WHEN 041                                                           
016900          PERFORM D-ST-TERMINAL-CN                                        
017000       WHEN 042                                                           
017100          PERFORM D-ST-TERMINAL-IN                                        
017200       WHEN 043                                                           
017300          PERFORM D-ST-TERMINAL-SC                                        
017400       WHEN 060                                                           
017500          PERFORM F-VOR                                                   
017600       WHEN 061                                                           
017700          PERFORM F-VOR-SC                                                
017800       WHEN 062                                                           
017900          PERFORM F-VOR-IN                                                
018000       WHEN 080                                                           
018100          PERFORM G-BACKA-AVG                                             
018200       WHEN 081                                                           
018300          PERFORM G-BACKA-AVG-SC                                          
018400       WHEN 082                                                           
018500          PERFORM G-BACKA-AVG-IN                                          
018600       WHEN OTHER                                                         
018700***              FEL KOD PÅ KDCALL (KDCALL) FINNS EJ                      
018800          MOVE '2'        TO AVG-KDSVAR                                   
018900     END-EVALUATE                                                         
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500                                                                          
019600 A-INIT    SECTION.                                                       
019700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
019800* GET MARKUP VALUE FROM WDB617                                  *         
020000* GET MARKUP VALUE FROM WDB622 FOR TW(PER FUNCTION GRP)         *         
020100* GET KDVALISO VALUE FROM WDB601                                *         
020200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
020300     ACCEPT DAGENS-DATUM  FROM DATE                                       
020400     MOVE SPACE               TO AVG-KDSVAR                               
020500     MOVE ZERO                TO WS-PRAVCOST                              
020600     MOVE AVG-TIAA            TO W-DATE-AAMM(1:2)                         
020700     MOVE AVG-TIMM            TO W-DATE-AAMM(3:2)                         
020800     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
020900     MOVE AVG-PRKURS          TO WP-PRKURS                                
021000     MOVE AVG-IDDC            TO WS-IDDC                                  
021100                                 W-IDDC                                   
021200     IF XDC-NON-VCC-OWNED                                                 
021300       PERFORM IMS-GU-WDB601                                              
021400       IF SEGMENT-FINNS                                                   
021500         PERFORM IMS-GNP-WDB617                                           
021600         IF SEGMENT-FINNS                                                 
021700           IF PROC-TILANDCO >  DAGENS-DATUM                               
021800             MOVE PROC-RELANDCO-TO   TO WS-MARKUP1                        
021900           ELSE                                                           
022000             MOVE PROC-RELANDCO-FROM TO WS-MARKUP1                        
022100           END-IF                                                         
022700           IF DCS-TAIWAN                                                  
022800             PERFORM AB-GET-MARKUP-TW                                     
022900           END-IF                                                         
023000           ADD  1                 TO WS-MARKUP1                           
023100           MOVE 1                 TO WS-MARKUP2                           
023200         ELSE                                                             
023300           MOVE '3'               TO AVG-KDSVAR                           
023400         END-IF                                                           
023500       ELSE                                                               
023600         MOVE '3'                 TO AVG-KDSVAR                           
023700       END-IF                                                             
023800     END-IF                                                               
023900     MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                    
024000     MOVE 'M'                     TO CURR-KDVALTYP                        
024100     .                                                                    
024200     EJECT                                                                
024300                                                                          
027300 AB-GET-MARKUP-TW SECTION.                                                
027400                                                                          
028410     MOVE AVG-IDFKNGRP       TO W-IDFKNGRP                                
028420     PERFORM IMS-GU-WDB622                                                
028430     IF SEGMENT-FINNS                                                     
028440       IF FGAD-TILANDCO >  DAGENS-DATUM                                   
028451         MOVE FGAD-RELANDCO-FG-TO   TO WS-MARKUP1                         
028460       ELSE                                                               
028471         MOVE FGAD-RELANDCO-FG-FROM TO WS-MARKUP1                         
028480       END-IF                                                             
028494     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 B-REFILL  SECTION.                                                       
028800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
028900* REFILL CDC TO NDC                                             *         
029000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
029100                                                                          
029200     PERFORM I-SOEK-MARKUP                                                
029300                                                                          
029400     IF PRODKOD-SAKNAS                                                    
029500       MOVE '1'         TO AVG-KDSVAR                                     
029600     ELSE                                                                 
029700       IF AVG-PRKURS = ZERO                                               
029800         IF NDC-CA                                                        
029900           MOVE 'CAD'   TO CURR-KDVALISO-ROW                              
030000         ELSE                                                             
030100           MOVE 'USD'   TO CURR-KDVALISO-ROW                              
030200         END-IF                                                           
030300         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
030400         IF CURR-KDSVAR = ' '                                             
030500           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
030600         ELSE                                                             
030700           MOVE ZERO        TO WS-PRAVCOST                                
030800           MOVE '3'         TO AVG-KDSVAR                                 
030900         END-IF                                                           
031000       END-IF                                                             
031100       IF AVG-KDSVAR = SPACE                                              
031200         COMPUTE WS-PRAVCOST  ROUNDED =                                   
031300               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
031400               ((AVG-KVANTMOT * AVG-PRARTNTO * WS-MARKUP) /               
031500                 AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)             
031600          ON SIZE ERROR                                                   
031700***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
031800            MOVE -1       TO WS-PRAVCOST                                  
031900         END-COMPUTE                                                      
032000         IF WS-PRAVCOST < ZERO                                            
032100           COMPUTE WS-PRAVCOST ROUNDED =                                  
032200              AVG-PRARTNTO * WS-MARKUP / AVG-PRKURS                       
032300            ON SIZE ERROR                                                 
032400              MOVE ZERO     TO WS-PRAVCOST                                
032500              MOVE '4'      TO AVG-KDSVAR                                 
032600           END-COMPUTE                                                    
032700         END-IF                                                           
032800       END-IF                                                             
032900       IF NDC-CA AND WP-PRKURS = 1.0                                      
033000         PERFORM S02-W335CURR                                             
033100       END-IF                                                             
033200       MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                               
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700 B-REFILL-SC  SECTION.                                                    
033800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
033900* REFILL CDC TO NDC-CN OR LDC-CN OR XDC-NON-VCC-OWNED           *         
034000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
034100     IF AVG-KDSVAR = SPACE                                                
034200      MOVE JA               TO PRODKOD-TEST                               
034300                                                                          
034400      IF PRODKOD-SAKNAS                                                   
034500       MOVE '1'         TO AVG-KDSVAR                                     
034600      ELSE                                                                
034700       IF AVG-PRKURS = ZERO                                               
034800         MOVE DCS-KDVALISO TO CURR-KDVALISO-ROW                           
034900         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
035000         IF CURR-KDSVAR = ' '                                             
035100           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
035200         ELSE                                                             
035300           MOVE ZERO        TO WS-PRAVCOST                                
035400           MOVE '3'         TO AVG-KDSVAR                                 
035500         END-IF                                                           
035600       END-IF                                                             
035700       IF AVG-KDSVAR = SPACE                                              
035800         COMPUTE WS-PRAVCOST  ROUNDED =                                   
035900               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
036000               ((AVG-KVANTMOT * AVG-PRARTNTO * WS-MARKUP1) /              
036100                 AVG-PRKURS)) /                                           
036400                (AVG-KVLS-OLD + AVG-KVANTMOT)                             
036500          ON SIZE ERROR                                                   
036600***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
036700            MOVE -1       TO WS-PRAVCOST                                  
036800         END-COMPUTE                                                      
036900         IF WS-PRAVCOST < ZERO                                            
037000           COMPUTE WS-PRAVCOST ROUNDED =                                  
037100             (AVG-PRARTNTO * WS-MARKUP1) / AVG-PRKURS                     
037400            ON SIZE ERROR                                                 
037500              MOVE ZERO     TO WS-PRAVCOST                                
037600              MOVE '4'      TO AVG-KDSVAR                                 
037700           END-COMPUTE                                                    
037800         END-IF                                                           
037900       END-IF                                                             
037910**** AS BRAZIL WILL NOT FOLLOW THE NORMAL AVERAGE COST CALCULATION        
037911**** ONLY WHEN THERE IS NO AVERAGE COST THE AVERAGE COST WILL             
037912**** BE UPDATED WITH A NEW VALUE OTHERWISE THE OLD VALUE WILL BE U        
037920       IF NDC-BR                                                          
037930         IF AVG-PRAVCOST-OLD = ZERO                                       
037931           MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                           
037940         ELSE                                                             
037941           MOVE AVG-PRAVCOST-OLD TO AVG-PRAVCOST-NEW                      
037942         END-IF                                                           
037950       ELSE                                                               
037951         MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                             
037960       END-IF                                                             
038100      END-IF                                                              
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 B-REFILL-IN  SECTION.                                                    
038700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
038800* REFILL CDC TO NDC-IN                                                    
038900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
039000     MOVE JA               TO PRODKOD-TEST                                
039100                                                                          
039200     IF PRODKOD-SAKNAS                                                    
039300       MOVE '1'         TO AVG-KDSVAR                                     
039400     ELSE                                                                 
039500       IF AVG-PRKURS = ZERO                                               
039600         MOVE 'INR'     TO CURR-KDVALISO-ROW                              
039700         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
039800         IF CURR-KDSVAR = ' '                                             
039900           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
040000         ELSE                                                             
040100           MOVE ZERO        TO WS-PRAVCOST                                
040200           MOVE '3'         TO AVG-KDSVAR                                 
040300         END-IF                                                           
040400       END-IF                                                             
040500       IF AVG-KDSVAR = SPACE                                              
040600         COMPUTE WS-PRAVCOST  ROUNDED =                                   
040700               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
040800               ((AVG-KVANTMOT * AVG-PRARTNTO * WS-MARKUP1) /              
040900                 AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)             
041000          ON SIZE ERROR                                                   
041100***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
041200            MOVE -1       TO WS-PRAVCOST                                  
041300         END-COMPUTE                                                      
041400         IF WS-PRAVCOST < ZERO                                            
041500           COMPUTE WS-PRAVCOST ROUNDED =                                  
041600              AVG-PRARTNTO * WS-MARKUP1 / AVG-PRKURS                      
041700            ON SIZE ERROR                                                 
041800              MOVE ZERO     TO WS-PRAVCOST                                
041900              MOVE '4'      TO AVG-KDSVAR                                 
042000           END-COMPUTE                                                    
042100         END-IF                                                           
042200       END-IF                                                             
042300       MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                               
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800 C-TRANSFER-US  SECTION.                                                  
042900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
043000* TRANSFER WITHIN THE US                                        *         
043100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
043200                                                                          
043300     COMPUTE WS-PRAVCOST  ROUNDED =                                       
043400           ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                           
043500            (AVG-KVANTMOT * AVG-PRARTNTO)) /                              
043600            (AVG-KVLS-OLD + AVG-KVANTMOT)                                 
043700       ON SIZE ERROR                                                      
043800***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
043900         MOVE -1           TO WS-PRAVCOST                                 
044000         MOVE '4'          TO AVG-KDSVAR                                  
044100     END-COMPUTE                                                          
044200     IF WS-PRAVCOST < ZERO                                                
044300       MOVE AVG-PRARTNTO   TO WS-PRAVCOST                                 
044400     END-IF                                                               
044500                                                                          
044600     IF NDC-CA AND WP-PRKURS = 1.0                                        
044700       PERFORM S02-W335CURR                                               
044800     END-IF                                                               
044900     MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                            
045000     MOVE 1                TO AVG-REMARKUP                                
045100     .                                                                    
045200     EJECT                                                                
045300                                                                          
045400 C-TRANSFER-SC  SECTION.                                                  
045500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
045600* TRANSFER WITHIN THE CN                                        *         
045700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
045800                                                                          
045900     COMPUTE WS-PRAVCOST  ROUNDED =                                       
046000           ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                           
046100            (AVG-KVANTMOT * AVG-PRARTNTO)) /                              
046200            (AVG-KVLS-OLD + AVG-KVANTMOT)                                 
046300       ON SIZE ERROR                                                      
046400***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
046500         MOVE -1           TO WS-PRAVCOST                                 
046600         MOVE '4'          TO AVG-KDSVAR                                  
046700     END-COMPUTE                                                          
046800     IF WS-PRAVCOST < ZERO                                                
046900       MOVE AVG-PRARTNTO   TO WS-PRAVCOST                                 
047000     END-IF                                                               
047100                                                                          
047300     MOVE 1                TO AVG-REMARKUP                                
047310**** AS BRAZIL WILL NOT FOLLOW THE NORMAL AVERAGE COST CALCULATION        
047320**** ONLY WHEN THERE IS NO AVERAGE COST THE AVERAGE COST WILL             
047330**** BE UPDATED WITH A NEW VALUE OTHERWISE THE OLD VALUE WILL BE U        
047340     IF NDC-BR                                                            
047350       IF AVG-PRAVCOST-OLD = ZERO                                         
047360         MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                             
047370       ELSE                                                               
047380         MOVE AVG-PRAVCOST-OLD TO AVG-PRAVCOST-NEW                        
047390       END-IF                                                             
047391     ELSE                                                                 
047392       MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                               
047393     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600                                                                          
047700 C-TRANSFER-US-CANADA  SECTION.                                           
047800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
047900* TRANSFER BETWEEN US AND CANADA                                *         
048000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
048100                                                                          
048200     PERFORM I-SOEK-MARKUP                                                
048300                                                                          
048400     IF PRODKOD-SAKNAS                                                    
048500       MOVE '1'         TO AVG-KDSVAR                                     
048600     ELSE                                                                 
048700       IF AVG-PRKURS = ZERO                                               
048800         PERFORM S01-KURSER                                               
048900         IF AVG-KDSVAR = SPACE                                            
049000         COMPUTE WS-PRKURS = WS-PRKURS2 / WS-PRKURS1                      
049100         MOVE WS-PRKURS TO AVG-PRKURS                                     
049200         END-IF                                                           
049300       END-IF                                                             
049400       IF AVG-KDSVAR = SPACE                                              
049500       COMPUTE WS-PRAVCOST  ROUNDED =                                     
049600             ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                         
049700             ((AVG-KVANTMOT * AVG-PRARTNTO * WS-MARKUP) /                 
049800               AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)               
049900         ON SIZE ERROR                                                    
050000***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
050100           MOVE -1      TO WS-PRAVCOST                                    
050200       END-COMPUTE                                                        
050300       IF WS-PRAVCOST < ZERO                                              
050400         COMPUTE WS-PRAVCOST  ROUNDED =                                   
050500           AVG-PRARTNTO * WS-MARKUP / AVG-PRKURS                          
050600         ON SIZE ERROR                                                    
050700           MOVE ZERO    TO WS-PRAVCOST                                    
050800           MOVE '4'     TO AVG-KDSVAR                                     
050900         END-COMPUTE                                                      
051000       END-IF                                                             
051100       IF NDC-CA AND WP-PRKURS = 1.0                                      
051200         PERFORM S02-W335CURR                                             
051300       END-IF                                                             
051400       MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                               
051500       END-IF                                                             
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 D-ST-TERMINAL  SECTION.                                                  
052100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
052200* ST-TERMINAL                                                   *         
052300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
052400*                                                                         
052500* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
052600* ** INMATAD VALUTA                                    **                 
052700                                                                          
052800     PERFORM I-SOEK-MARKUP                                                
052900                                                                          
053000     IF PRODKOD-SAKNAS                                                    
053100       MOVE '1'              TO AVG-KDSVAR                                
053200     ELSE                                                                 
053300       IF (AVG-KDVALISO = 'USD'  AND NDC-US)                              
053400       OR (AVG-KDVALISO = 'CAD'  AND NDC-CA)                              
053500         MOVE 1                TO WS-MARKUP                               
053600         MOVE 1                TO AVG-REMARKUP                            
053700       END-IF                                                             
053800       PERFORM S01-KURSER                                                 
053900       IF AVG-KDSVAR = SPACE                                              
054000         COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                          
054100         COMPUTE WS-PRAVCOST  ROUNDED =                                   
054200                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
054300*                (((AVG-KVANTMOT * AVG-PRARTBEL * WS-MARKUP) +            
054400                 (((AVG-KVANTMOT * AVG-PRARTBEL) +                        
054500                   (AVG-KVANTMOT * AVG-PRARTNTO) +                        
054600                   (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP)) *        
054700                    WS-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)           
054800           ON SIZE ERROR                                                  
054900***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
055000             MOVE -1             TO WS-PRAVCOST                           
055100         END-COMPUTE                                                      
055200         IF WS-PRAVCOST < ZERO                                            
055300           COMPUTE WS-PRAVCOST  ROUNDED =                                 
055400               AVG-PRARTBEL * WS-MARKUP * WS-PRKURS                       
055500           ON SIZE ERROR                                                  
055600               MOVE ZERO         TO WS-PRAVCOST                           
055700               MOVE '4'          TO AVG-KDSVAR                            
055800           END-COMPUTE                                                    
055900         END-IF                                                           
056000       END-IF                                                             
056100       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
056200     END-IF                                                               
056300                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 D-ST-TERMINAL-CN     SECTION.                                            
056800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
056900* ST-TERMINAL                                                   *         
057000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
057100*                                                                         
057200* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
057300* ** INMATAD VALUTA                                    **                 
057400                                                                          
057500     IF AVG-KDSVAR = SPACE                                                
057600      MOVE JA               TO PRODKOD-TEST                               
057700                                                                          
057800      IF PRODKOD-SAKNAS                                                   
057900        MOVE '1'              TO AVG-KDSVAR                               
058000      ELSE                                                                
058100        IF AVG-PRKURS = ZERO                                              
058200          IF AVG-KDVALISO = DCS-KDVALISO                                  
058300            MOVE 1              TO WS-MARKUP                              
058400          END-IF                                                          
058500          PERFORM S03-KURSER                                              
058600          IF AVG-KDSVAR = SPACE                                           
058700            COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                       
058800            COMPUTE WS-PRAVCOST ROUNDED =                                 
058900                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
059000                 (((AVG-KVANTMOT * AVG-PRARTBEL) +                        
059100                   (AVG-KVANTMOT * AVG-PRARTNTO) +                        
059200                   (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP)) *        
059300                    WS-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)           
059400            ON SIZE ERROR                                                 
059500*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
059600               MOVE -1           TO WS-PRAVCOST                           
059700           END-COMPUTE                                                    
059800           IF WS-PRAVCOST < ZERO                                          
059900             COMPUTE WS-PRAVCOST ROUNDED =                                
060000                 (AVG-PRARTBEL * WS-PRKURS) +                             
060100                 (AVG-PRARTBEL * AVG-REMARKUP * WS-PRKURS)                
060200             ON SIZE ERROR                                                
060300                 MOVE ZERO       TO WS-PRAVCOST                           
060400                 MOVE '4'        TO AVG-KDSVAR                            
060500             END-COMPUTE                                                  
060600           END-IF                                                         
060700         END-IF                                                           
060800       ELSE                                                               
060900         IF AVG-KDSVAR = SPACE                                            
061000           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
061100           COMPUTE WS-PRAVCOST ROUNDED =                                  
061200                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
061300                 (((AVG-KVANTMOT * AVG-PRARTBEL) +                        
061400                   (AVG-KVANTMOT * AVG-PRARTNTO) +                        
061500                   (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP)) *        
061600                    AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)          
061700                                                                          
061800             ON SIZE ERROR                                                
061900******  HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
062000               MOVE -1           TO WS-PRAVCOST                           
062100           END-COMPUTE                                                    
062200           IF WS-PRAVCOST < ZERO                                          
062300             COMPUTE WS-PRAVCOST ROUNDED =                                
062400                 (AVG-PRARTBEL * AVG-PRKURS) +                            
062500                 (AVG-PRARTBEL * AVG-REMARKUP * AVG-PRKURS)               
062600             ON SIZE ERROR                                                
062700                 MOVE ZERO       TO WS-PRAVCOST                           
062800                 MOVE '4'        TO AVG-KDSVAR                            
062900             END-COMPUTE                                                  
063000           END-IF                                                         
063100         END-IF                                                           
063200        END-IF                                                            
063310**** AS BRAZIL WILL NOT FOLLOW THE NORMAL AVERAGE COST CALCULATION        
063320**** ONLY WHEN THERE IS NO AVERAGE COST THE AVERAGE COST WILL             
063330**** BE UPDATED WITH A NEW VALUE OTHERWISE THE OLD VALUE WILL BE U        
063340        IF NDC-BR                                                         
063350          IF AVG-PRAVCOST-OLD = ZERO                                      
063360            MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                          
063370          ELSE                                                            
063380            MOVE AVG-PRAVCOST-OLD TO AVG-PRAVCOST-NEW                     
063390          END-IF                                                          
063391        ELSE                                                              
063392          MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                            
063393        END-IF                                                            
063400      END-IF                                                              
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900 D-ST-TERMINAL-SC     SECTION.                                            
064000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
064100* ST-TERMINAL                                                   *         
064200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
064300*                                                                         
064400* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
064500* ** INMATAD VALUTA                                    **                 
064600                                                                          
064700     IF AVG-KDSVAR = SPACE                                                
064800      MOVE JA               TO PRODKOD-TEST                               
064900                                                                          
065000      IF PRODKOD-SAKNAS                                                   
065100        MOVE '1'              TO AVG-KDSVAR                               
065200      ELSE                                                                
065300        IF AVG-PRKURS = ZERO                                              
065400          IF AVG-KDVALISO = DCS-KDVALISO                                  
065500            MOVE 1              TO WS-MARKUP                              
065600          END-IF                                                          
065700          PERFORM S01-KURSER                                              
065800          IF AVG-KDSVAR = SPACE                                           
065900            COMPUTE WS-PRAVCOST ROUNDED =                                 
066000                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
066100                   (AVG-KVANTMOT * AVG-PRARTBEL))                         
066200                    / (AVG-KVLS-OLD + AVG-KVANTMOT)                       
066300            ON SIZE ERROR                                                 
066400*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
066500               MOVE -1           TO WS-PRAVCOST                           
066600           END-COMPUTE                                                    
066700           IF WS-PRAVCOST < ZERO                                          
066800             COMPUTE WS-PRAVCOST ROUNDED =                                
066900                 AVG-PRARTBEL                                             
067000             ON SIZE ERROR                                                
067100                 MOVE ZERO       TO WS-PRAVCOST                           
067200                 MOVE '4'        TO AVG-KDSVAR                            
067300             END-COMPUTE                                                  
067400           END-IF                                                         
067500         END-IF                                                           
067600       ELSE                                                               
067700         IF AVG-KDSVAR = SPACE                                            
067800           COMPUTE WS-PRAVCOST ROUNDED =                                  
067900                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
068000                   (AVG-KVANTMOT * AVG-PRARTBEL))                         
068100                   / (AVG-KVLS-OLD + AVG-KVANTMOT)                        
068200             ON SIZE ERROR                                                
068300******  HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
068400               MOVE -1           TO WS-PRAVCOST                           
068500           END-COMPUTE                                                    
068600           IF WS-PRAVCOST < ZERO                                          
068700             COMPUTE WS-PRAVCOST ROUNDED =                                
068800                 AVG-PRARTBEL                                             
068900             ON SIZE ERROR                                                
069000                 MOVE ZERO       TO WS-PRAVCOST                           
069100                 MOVE '4'        TO AVG-KDSVAR                            
069200             END-COMPUTE                                                  
069300           END-IF                                                         
069400         END-IF                                                           
069500        END-IF                                                            
069600        MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                         
069700      END-IF                                                              
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100                                                                          
070200 D-ST-TERMINAL-IN  SECTION.                                               
070300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
070400* ST-TERMINAL                                                   *         
070500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
070600*                                                                         
070700* ** HÄR KONTROLLERAS ATT KURS FINNS UPPLAGD FÖR       **                 
070800* ** INMATAD VALUTA                                    **                 
070900                                                                          
071000     MOVE JA               TO PRODKOD-TEST                                
071100                                                                          
071200     IF PRODKOD-SAKNAS                                                    
071300       MOVE '1'              TO AVG-KDSVAR                                
071400     ELSE                                                                 
071500       IF AVG-PRKURS = ZERO                                               
071600         IF (AVG-KDVALISO = 'INR' AND NDC-IN)                             
071700           MOVE 1              TO WS-MARKUP                               
071800         END-IF                                                           
071900         PERFORM S03-KURSER                                               
072000         IF AVG-KDSVAR = SPACE                                            
072100           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
072200           COMPUTE WS-PRAVCOST ROUNDED =                                  
072300                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
072400                 (((AVG-KVANTMOT * AVG-PRARTBEL) +                        
072500                   (AVG-KVANTMOT * AVG-PRARTNTO) +                        
072600                   (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP)) *        
072700                    WS-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)           
072800             ON SIZE ERROR                                                
072900*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
073000               MOVE -1           TO WS-PRAVCOST                           
073100           END-COMPUTE                                                    
073200           IF WS-PRAVCOST < ZERO                                          
073300             COMPUTE WS-PRAVCOST ROUNDED =                                
073400                 (AVG-PRARTBEL * WS-PRKURS) +                             
073500                 (AVG-PRARTBEL * AVG-REMARKUP * WS-PRKURS)                
073600             ON SIZE ERROR                                                
073700                 MOVE ZERO       TO WS-PRAVCOST                           
073800                 MOVE '4'        TO AVG-KDSVAR                            
073900             END-COMPUTE                                                  
074000           END-IF                                                         
074100         END-IF                                                           
074200       ELSE                                                               
074300         IF AVG-KDSVAR = SPACE                                            
074400           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
074500           COMPUTE WS-PRAVCOST ROUNDED =                                  
074600                  ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                    
074700                 (((AVG-KVANTMOT * AVG-PRARTBEL) +                        
074800                   (AVG-KVANTMOT * AVG-PRARTNTO) +                        
074900                   (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP)) *        
075000                    AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVANTMOT)          
075100             ON SIZE ERROR                                                
075200******  HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
075300               MOVE -1           TO WS-PRAVCOST                           
075400           END-COMPUTE                                                    
075500           IF WS-PRAVCOST < ZERO                                          
075600             COMPUTE WS-PRAVCOST ROUNDED =                                
075700                 (AVG-PRARTBEL * AVG-PRKURS) +                            
075800                 (AVG-PRARTBEL * AVG-REMARKUP * AVG-PRKURS)               
075900             ON SIZE ERROR                                                
076000                 MOVE ZERO       TO WS-PRAVCOST                           
076100                 MOVE '4'        TO AVG-KDSVAR                            
076200             END-COMPUTE                                                  
076300           END-IF                                                         
076400         END-IF                                                           
076500       END-IF                                                             
076600       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000                                                                          
077100                                                                          
077200 F-VOR  SECTION.                                                          
077300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
077400* VOR                                                           *         
077500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
077600                                                                          
077700     PERFORM I-SOEK-MARKUP                                                
077800                                                                          
077900     IF PRODKOD-SAKNAS                                                    
078000       MOVE '1'              TO AVG-KDSVAR                                
078100     ELSE                                                                 
078200       IF AVG-PRKURS = ZERO                                               
078300         IF NDC-CA                                                        
078400           MOVE 'CAD'   TO CURR-KDVALISO-ROW                              
078500         ELSE                                                             
078600           MOVE 'USD'   TO CURR-KDVALISO-ROW                              
078700         END-IF                                                           
078800         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
078900         IF CURR-KDSVAR = ' '                                             
079000           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
079100         ELSE                                                             
079200           MOVE ZERO    TO WS-PRAVCOST                                    
079300           MOVE '3'     TO AVG-KDSVAR                                     
079400         END-IF                                                           
079500       END-IF                                                             
079600       IF AVG-KDSVAR = SPACE                                              
079700         COMPUTE WS-PRAVCOST  ROUNDED =                                   
079800               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
079900               ((AVG-KVLEVART * AVG-PRARTNTO * WS-MARKUP) /               
080000                 AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVLEVART)             
080100           ON SIZE ERROR                                                  
080200***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
080300             MOVE -1             TO WS-PRAVCOST                           
080400         END-COMPUTE                                                      
080500         IF WS-PRAVCOST < ZERO                                            
080600           COMPUTE WS-PRAVCOST  ROUNDED =                                 
080700             AVG-PRARTNTO * WS-MARKUP / AVG-PRKURS                        
080800           ON SIZE ERROR                                                  
080900               MOVE ZERO         TO WS-PRAVCOST                           
081000               MOVE '4'          TO AVG-KDSVAR                            
081100           END-COMPUTE                                                    
081200         END-IF                                                           
081300       END-IF                                                             
081400       IF NDC-CA AND WP-PRKURS = 1.0                                      
081500         PERFORM S02-W335CURR                                             
081600       END-IF                                                             
081700       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
081800     END-IF                                                               
081900                                                                          
082000     .                                                                    
082100     EJECT                                                                
082200                                                                          
082300 F-VOR-SC  SECTION.                                                       
082400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
082500* VOR                                                           *         
082600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
082700     IF AVG-KDSVAR = SPACE                                                
082800      MOVE JA               TO PRODKOD-TEST                               
082900                                                                          
083000      IF PRODKOD-SAKNAS                                                   
083100        MOVE '1'              TO AVG-KDSVAR                               
083200      ELSE                                                                
083300       IF AVG-PRKURS = ZERO                                               
083400         MOVE DCS-KDVALISO TO CURR-KDVALISO-ROW                           
083500         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
083600         IF CURR-KDSVAR = ' '                                             
083700           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
083800         ELSE                                                             
083900           MOVE ZERO    TO WS-PRAVCOST                                    
084000           MOVE '3'     TO AVG-KDSVAR                                     
084100         END-IF                                                           
084200       END-IF                                                             
084300       IF AVG-KDSVAR = SPACE                                              
084400         COMPUTE WS-PRAVCOST  ROUNDED =                                   
084500               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
084600               ((AVG-KVLEVART * AVG-PRARTNTO * WS-MARKUP1) /              
084700                 AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVLEVART)             
085100           ON SIZE ERROR                                                  
085200***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
085300             MOVE -1             TO WS-PRAVCOST                           
085400         END-COMPUTE                                                      
085500         IF WS-PRAVCOST < ZERO                                            
085600           COMPUTE WS-PRAVCOST  ROUNDED =                                 
085700            (AVG-PRARTNTO * WS-MARKUP1) / AVG-PRKURS                      
085900           ON SIZE ERROR                                                  
086000               MOVE ZERO         TO WS-PRAVCOST                           
086100               MOVE '4'          TO AVG-KDSVAR                            
086200           END-COMPUTE                                                    
086300         END-IF                                                           
086400       END-IF                                                             
086510**** AS BRAZIL WILL NOT FOLLOW THE NORMAL AVERAGE COST CALCULATION        
086520**** ONLY WHEN THERE IS NO AVERAGE COST THE AVERAGE COST WILL             
086530**** BE UPDATED WITH A NEW VALUE OTHERWISE THE OLD VALUE WILL BE U        
086540       IF NDC-BR                                                          
086550         IF AVG-PRAVCOST-OLD = ZERO                                       
086560           MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                           
086570         ELSE                                                             
086580           MOVE AVG-PRAVCOST-OLD TO AVG-PRAVCOST-NEW                      
086590         END-IF                                                           
086591       ELSE                                                               
086592         MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                             
086593       END-IF                                                             
086600      END-IF                                                              
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 F-VOR-IN  SECTION.                                                       
087200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
087300* VOR                                                           *         
087400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
087500     MOVE JA               TO PRODKOD-TEST                                
087600                                                                          
087700     IF PRODKOD-SAKNAS                                                    
087800       MOVE '1'              TO AVG-KDSVAR                                
087900     ELSE                                                                 
088000       IF AVG-PRKURS = ZERO                                               
088100         MOVE 'INR'     TO CURR-KDVALISO-ROW                              
088200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
088300         IF CURR-KDSVAR = ' '                                             
088400           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
088500         ELSE                                                             
088600           MOVE ZERO    TO WS-PRAVCOST                                    
088700           MOVE '3'     TO AVG-KDSVAR                                     
088800         END-IF                                                           
088900       END-IF                                                             
089000       IF AVG-KDSVAR = SPACE                                              
089100         COMPUTE WS-PRAVCOST  ROUNDED =                                   
089200               ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) +                       
089300               ((AVG-KVLEVART * AVG-PRARTNTO * WS-MARKUP1) /              
089400                 AVG-PRKURS)) / (AVG-KVLS-OLD + AVG-KVLEVART)             
089500           ON SIZE ERROR                                                  
089600***** HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA           
089700             MOVE -1             TO WS-PRAVCOST                           
089800         END-COMPUTE                                                      
089900         IF WS-PRAVCOST < ZERO                                            
090000           COMPUTE WS-PRAVCOST  ROUNDED =                                 
090100             AVG-PRARTNTO * WS-MARKUP1 / AVG-PRKURS                       
090200           ON SIZE ERROR                                                  
090300               MOVE ZERO         TO WS-PRAVCOST                           
090400               MOVE '4'          TO AVG-KDSVAR                            
090500           END-COMPUTE                                                    
090600         END-IF                                                           
090700       END-IF                                                             
090800       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
090900     END-IF                                                               
091000                                                                          
091100     .                                                                    
091200     EJECT                                                                
091300                                                                          
091400 G-BACKA-AVG SECTION.                                                     
091500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
091600* BACKA AVG                                                     *         
091700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
091800     MOVE JA               TO PRODKOD-TEST                                
091900                                                                          
092000     IF PRODKOD-SAKNAS                                                    
092100       MOVE '1'              TO AVG-KDSVAR                                
092200     ELSE                                                                 
092300       IF AVG-PRKURS = ZERO                                               
092400         IF NDC-CA                                                        
092500           MOVE 'CAD'   TO CURR-KDVALISO-ROW                              
092600         ELSE                                                             
092700           MOVE 'USD'   TO CURR-KDVALISO-ROW                              
092800         END-IF                                                           
092900         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
093000         IF CURR-KDSVAR = ' '                                             
093100           MOVE CURR-PRKURS-NEW TO AVG-PRKURS                             
093200         ELSE                                                             
093300           MOVE ZERO    TO WS-PRAVCOST                                    
093400           MOVE '3'     TO AVG-KDSVAR                                     
093500         END-IF                                                           
093600         MOVE 1 TO AVG-REMARKUP                                           
093700         IF AVG-KDSVAR = SPACE                                            
093800           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
093900           COMPUTE WS-PRAVCOST ROUNDED =                                  
094000                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
094100                 ((AVG-KVANTMOT * AVG-PRARTNTO) +                         
094200                  (AVG-KVANTMOT * AVG-PRARTBEL) +                         
094300                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)))          
094400                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
094500             ON SIZE ERROR                                                
094600*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
094700               MOVE -1           TO WS-PRAVCOST                           
094800           END-COMPUTE                                                    
094900           IF WS-PRAVCOST < ZERO                                          
095000             COMPUTE WS-PRAVCOST ROUNDED =                                
095100               AVG-PRARTNTO * WS-MARKUP2 / AVG-PRKURS                     
095200             ON SIZE ERROR                                                
095300                 MOVE ZERO       TO WS-PRAVCOST                           
095400                 MOVE '4'        TO AVG-KDSVAR                            
095500             END-COMPUTE                                                  
095600           END-IF                                                         
095700         END-IF                                                           
095800       ELSE                                                               
095900         IF AVG-KDSVAR = SPACE                                            
096000           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
096100           COMPUTE WS-PRAVCOST ROUNDED =                                  
096200                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
096300                 ((AVG-KVANTMOT * AVG-PRARTNTO) +                         
096400                  (AVG-KVANTMOT * AVG-PRARTBEL * AVG-PRKURS) +            
096500                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)))          
096600                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
096700             ON SIZE ERROR                                                
096800*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
096900               MOVE -1           TO WS-PRAVCOST                           
097000               MOVE '4'          TO AVG-KDSVAR                            
097100           END-COMPUTE                                                    
097200           IF WS-PRAVCOST < ZERO                                          
097300             COMPUTE WS-PRAVCOST ROUNDED =                                
097400               AVG-PRARTBEL * WS-MARKUP2 / AVG-PRKURS                     
097500             ON SIZE ERROR                                                
097600                 MOVE ZERO       TO WS-PRAVCOST                           
097700                 MOVE '4'        TO AVG-KDSVAR                            
097800             END-COMPUTE                                                  
097900           END-IF                                                         
098000         END-IF                                                           
098100       END-IF                                                             
098200       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
098300     END-IF                                                               
098400     .                                                                    
098500     EJECT                                                                
098600                                                                          
098700 G-BACKA-AVG-SC SECTION.                                                  
098800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
098900* BACKA AVG                                                     *         
099000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
099100     IF AVG-KDSVAR = SPACE                                                
099200      MOVE JA               TO PRODKOD-TEST                               
099300                                                                          
099400      IF PRODKOD-SAKNAS                                                   
099500       MOVE '1'              TO AVG-KDSVAR                                
099600      ELSE                                                                
099700       IF AVG-PRKURS = ZERO                                               
099800         PERFORM S03-KURSER                                               
099900         IF AVG-KDSVAR = SPACE                                            
100000           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
100100           COMPUTE WS-PRAVCOST ROUNDED =                                  
100200                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
100300                (((AVG-KVANTMOT * AVG-PRARTNTO) +                         
100400                  (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP) +          
100500                  (AVG-KVANTMOT * AVG-PRARTBEL) +                         
100600                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)) *         
100700                   WS-PRKURS))                                            
100800                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
100900             ON SIZE ERROR                                                
101000*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
101100               MOVE -1           TO WS-PRAVCOST                           
101200           END-COMPUTE                                                    
101300           IF WS-PRAVCOST < ZERO                                          
101400             COMPUTE WS-PRAVCOST ROUNDED =                                
101500               (AVG-PRARTBEL * AVG-REMARKUP * WS-PRKURS) +                
101600               (AVG-PRARTBEL * WS-PRKURS)                                 
101700             ON SIZE ERROR                                                
101800                 MOVE ZERO       TO WS-PRAVCOST                           
101900                 MOVE '4'        TO AVG-KDSVAR                            
102000             END-COMPUTE                                                  
102100           END-IF                                                         
102200         END-IF                                                           
102300       ELSE                                                               
102400         IF AVG-KDSVAR = SPACE                                            
102500           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
102600           COMPUTE WS-PRAVCOST ROUNDED =                                  
102700                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
102800                (((AVG-KVANTMOT * AVG-PRARTNTO) +                         
102900                  (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP) +          
103000                  (AVG-KVANTMOT * AVG-PRARTBEL ) +                        
103100                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)) *         
103200                   AVG-PRKURS))                                           
103300                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
103400             ON SIZE ERROR                                                
103500*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
103600               MOVE -1           TO WS-PRAVCOST                           
103700           END-COMPUTE                                                    
103800           IF WS-PRAVCOST < ZERO                                          
103900             COMPUTE WS-PRAVCOST ROUNDED =                                
104000               (AVG-PRARTBEL * AVG-REMARKUP * AVG-PRKURS) +               
104100               (AVG-PRARTBEL * AVG-PRKURS)                                
104200             ON SIZE ERROR                                                
104300                 MOVE ZERO       TO WS-PRAVCOST                           
104400                 MOVE '4'        TO AVG-KDSVAR                            
104500             END-COMPUTE                                                  
104600           END-IF                                                         
104700         END-IF                                                           
104800       END-IF                                                             
104910**** AS BRAZIL WILL NOT FOLLOW THE NORMAL AVERAGE COST CALCULATION        
104920**** ONLY WHEN THERE IS NO AVERAGE COST THE AVERAGE COST WILL             
104930**** BE UPDATED WITH A NEW VALUE OTHERWISE THE OLD VALUE WILL BE U        
104940       IF NDC-BR                                                          
104950         IF AVG-PRAVCOST-OLD = ZERO                                       
104960           MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                           
104970         ELSE                                                             
104980           MOVE AVG-PRAVCOST-OLD TO AVG-PRAVCOST-NEW                      
104990         END-IF                                                           
104991       ELSE                                                               
104992         MOVE WS-PRAVCOST TO AVG-PRAVCOST-NEW                             
104993       END-IF                                                             
105000      END-IF                                                              
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105400                                                                          
105500 G-BACKA-AVG-IN SECTION.                                                  
105600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
105700* BACKA AVG                                                     *         
105800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
105900     MOVE JA               TO PRODKOD-TEST                                
106000                                                                          
106100     IF PRODKOD-SAKNAS                                                    
106200       MOVE '1'              TO AVG-KDSVAR                                
106300     ELSE                                                                 
106400       IF AVG-PRKURS = ZERO                                               
106500         PERFORM S03-KURSER                                               
106600         IF AVG-KDSVAR = SPACE                                            
106700           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
106800           COMPUTE WS-PRAVCOST ROUNDED =                                  
106900                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
107000                (((AVG-KVANTMOT * AVG-PRARTNTO) +                         
107100                  (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP) +          
107200                  (AVG-KVANTMOT * AVG-PRARTBEL) +                         
107300                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)) *         
107400                   WS-PRKURS))                                            
107500                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
107600             ON SIZE ERROR                                                
107700*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
107800               MOVE -1           TO WS-PRAVCOST                           
107900           END-COMPUTE                                                    
108000           IF WS-PRAVCOST < ZERO                                          
108100             COMPUTE WS-PRAVCOST ROUNDED =                                
108200               (AVG-PRARTBEL * AVG-REMARKUP * WS-PRKURS) +                
108300               (AVG-PRARTBEL * WS-PRKURS)                                 
108400             ON SIZE ERROR                                                
108500                 MOVE ZERO       TO WS-PRAVCOST                           
108600                 MOVE '4'        TO AVG-KDSVAR                            
108700             END-COMPUTE                                                  
108800           END-IF                                                         
108900         END-IF                                                           
109000       ELSE                                                               
109100         IF AVG-KDSVAR = SPACE                                            
109200           COMPUTE AVG-REMARKUP = AVG-REMARKUP - 1                        
109300           COMPUTE WS-PRAVCOST ROUNDED =                                  
109400                 ((AVG-PRAVCOST-OLD * AVG-KVLS-OLD) -                     
109500                (((AVG-KVANTMOT * AVG-PRARTNTO) +                         
109600                  (AVG-KVANTMOT * AVG-PRARTBEL * AVG-REMARKUP) +          
109700                  (AVG-KVANTMOT * AVG-PRARTBEL ) +                        
109800                  (AVG-KVANTMOT * AVG-PRARTNTO * AVG-REMARKUP)) *         
109900                   AVG-PRKURS))                                           
110000                   / (AVG-KVLS-OLD - AVG-KVANTMOT)                        
110100             ON SIZE ERROR                                                
110200*****   HÄR FYLLER KJ I -1 I PRAVCOST FÖR ATT HAMNA I NÄSTA FÅLLA         
110300               MOVE -1           TO WS-PRAVCOST                           
110400           END-COMPUTE                                                    
110500           IF WS-PRAVCOST < ZERO                                          
110600             COMPUTE WS-PRAVCOST ROUNDED =                                
110700               (AVG-PRARTBEL * AVG-REMARKUP * AVG-PRKURS) +               
110800               (AVG-PRARTBEL * AVG-PRKURS)                                
110900             ON SIZE ERROR                                                
111000                 MOVE ZERO       TO WS-PRAVCOST                           
111100                 MOVE '4'        TO AVG-KDSVAR                            
111200             END-COMPUTE                                                  
111300           END-IF                                                         
111400         END-IF                                                           
111500       END-IF                                                             
111600       MOVE WS-PRAVCOST      TO AVG-PRAVCOST-NEW                          
111700     END-IF                                                               
111800     .                                                                    
111900     EJECT                                                                
112000                                                                          
112100                                                                          
112200 I-SOEK-MARKUP  SECTION.                                                  
112300                                                                          
112400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
112500* SÖK UPP RÄTT MARKUP-FAKTOR I TABELLEN                         *         
112600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
112700                                                                          
112800     MOVE NEJ                  TO PRODKOD-TEST                            
112900     MOVE 1                    TO WS-IND                                  
113000                                                                          
113100     PERFORM UNTIL PRODKOD-FINNS OR  WS-IND > MARKUP-TAB-MAX              
113200       IF MARKUP-LPC (WS-IND) = AVG-KDPSLLOC                              
113300         MOVE JA               TO PRODKOD-TEST                            
113400       ELSE                                                               
113500         ADD 1                 TO WS-IND                                  
113600       END-IF                                                             
113700     END-PERFORM                                                          
113800                                                                          
113900     IF PRODKOD-FINNS                                                     
114000       IF NDC-US                                                          
114100         MOVE MARKUP-FAKTOR-USA (WS-IND)                                  
114200                               TO WS-MARKUP                               
114300                                  AVG-REMARKUP                            
114400       ELSE                                                               
114500         MOVE MARKUP-FAKTOR-CAN (WS-IND)                                  
114600                               TO WS-MARKUP                               
114700                                  AVG-REMARKUP                            
114800       END-IF                                                             
114900     END-IF                                                               
115000                                                                          
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115400 S01-KURSER  SECTION.                                                     
115500                                                                          
115600     IF AVG-PRKURS = ZERO                                                 
115700       IF AVG-KDVALISO = SPACE                                            
115800         MOVE ZERO    TO WS-PRAVCOST                                      
115900         MOVE '3'     TO AVG-KDSVAR                                       
116000       ELSE                                                               
116100         MOVE AVG-KDVALISO       TO CURR-KDVALISO-ROW                     
116200         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
116300         IF CURR-KDSVAR = ' '                                             
116400           MOVE CURR-PRKURS-NEW  TO AVG-PRKURS                            
116500         ELSE                                                             
116600           MOVE '3'              TO AVG-KDSVAR                            
116700         END-IF                                                           
116800       END-IF                                                             
116900     END-IF                                                               
117000                                                                          
117100     IF AVG-KDSVAR = SPACE                                                
117200       MOVE AVG-PRKURS      TO WS-PRKURS1                                 
117300       IF XDC-NON-VCC-OWNED                                               
117400         MOVE DCS-KDVALISO  TO CURR-KDVALISO-ROW                          
117500       ELSE                                                               
117600         MOVE AVG-KDVALISO  TO CURR-KDVALISO-ROW                          
117700       END-IF                                                             
117800       IF NDC-CA                                                          
117900         MOVE 'CAD'         TO CURR-KDVALISO-ROW                          
118000       END-IF                                                             
118100       IF NDC-US                                                          
118200         MOVE 'USD'         TO CURR-KDVALISO-ROW                          
118300       END-IF                                                             
118400       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
118500       IF CURR-KDSVAR = ' '                                               
118600         MOVE CURR-PRKURS-NEW TO WS-PRKURS2                               
118700         COMPUTE WS-PRKURS = WS-PRKURS1 / WS-PRKURS2                      
118800       ELSE                                                               
118900         MOVE '3'      TO AVG-KDSVAR                                      
119000         MOVE ZERO     TO WS-PRAVCOST                                     
119100       END-IF                                                             
119200       MOVE WS-PRKURS  TO AVG-PRKURS                                      
119300     END-IF                                                               
119400                                                                          
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800                                                                          
119900 S02-W335CURR  SECTION.                                                   
120000     MOVE 'USD'            TO CURR-KDVALISO-ROW                           
120100     MOVE 1.0              TO AVG-PRKURS                                  
120200                              CURR-PRKURS                                 
120300                              CURR-PRKURS-02                              
120400                                                                          
120500     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
120600     IF CURR-KDSVAR = ' '                                                 
120700        MOVE CURR-PRKURS-NEW   TO AVG-PRKURS                              
120800                                  CURR-PRKURS                             
120900     ELSE                                                                 
121000        MOVE ZERO          TO WS-PRAVCOST                                 
121100        MOVE '3'           TO AVG-KDSVAR                                  
121200     END-IF                                                               
121300     MOVE 'CAD'            TO CURR-KDVALISO-ROW                           
121400     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
121500     IF CURR-KDSVAR = ' '                                                 
121600        MOVE CURR-PRKURS-NEW   TO AVG-PRKURS                              
121700                                  CURR-PRKURS-02                          
121800     ELSE                                                                 
121900        MOVE ZERO          TO WS-PRAVCOST                                 
122000        MOVE '3'           TO AVG-KDSVAR                                  
122100     END-IF                                                               
122200     MOVE ZERO             TO CURR-SUORDV-IN                              
122300                              CURR-PRARTSTD-IN                            
122400                              CURR-PRARTSJK-IN                            
122500                              CURR-PRARTVNA-IN                            
122600     MOVE WS-PRAVCOST      TO CURR-PRARTSTD-IN                            
122700     MOVE +3               TO CURR-KDCALL                                 
122800     CALL W335CURR     USING CURR-W335CURR                                
122900**           KURS MELLAN CAD OCH USD                                      
123000     MOVE CURR-PRKURS-UT   TO AVG-PRKURS                                  
123100     MOVE CURR-PRARTSTD-UT   TO WS-PRAVCOST                               
123200     .                                                                    
123300     EJECT                                                                
123400                                                                          
123500 S03-KURSER  SECTION.                                                     
123600                                                                          
123700     IF AVG-PRKURS = ZERO                                                 
123800       IF AVG-KDVALISO = SPACE                                            
123900         MOVE ZERO               TO WS-PRAVCOST                           
124000         MOVE '3'                TO AVG-KDSVAR                            
124100       ELSE                                                               
124200         MOVE DCS-KDVALISO       TO CURR-KDVALISO-HUV                     
124300         MOVE AVG-KDVALISO       TO CURR-KDVALISO-ROW                     
124400         IF NDC-CA                                                        
124500           MOVE 'CAD'            TO CURR-KDVALISO-ROW                     
124600         END-IF                                                           
124700         IF NDC-US                                                        
124800           MOVE 'USD'            TO CURR-KDVALISO-ROW                     
124900         END-IF                                                           
125000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
125100         IF CURR-KDSVAR = ' '                                             
125200           MOVE CURR-PRKURS-NEW  TO WS-PRKURS                             
125300         ELSE                                                             
125400           MOVE '3'              TO AVG-KDSVAR                            
125500         END-IF                                                           
125600       END-IF                                                             
125700     END-IF                                                               
125800     .                                                                    
125900     EJECT                                                                
126000                                                                          
126100* --- IMS SEKTIONER ---                                                   
126200     SKIP3                                                                
126300                                                                          
126400 IMS-GU-WDB601    SECTION.                                                
126500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
126600          DELIMITED BY SIZE INTO SSA1                                     
126700     MOVE '  GE' TO GODK-STATUSKODER                                      
126800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
126900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
127000     PERFORM IMS-STATUSKONTROLL                                           
127100     .                                                                    
127200     SKIP3                                                                
127300 IMS-GNP-WDB617 SECTION.                                                  
127400     MOVE 'WDB617   ' TO SSA1                                             
127500     MOVE '  GE'        TO GODK-STATUSKODER                               
127600     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
127700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
127800     PERFORM IMS-STATUSKONTROLL                                           
127900     .                                                                    
128000     SKIP3                                                                
128100                                                                          
129510 IMS-GU-WDB622 SECTION.                                                   
129520                                                                          
129530     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
129540          DELIMITED BY SIZE INTO SSA1                                     
129550     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
129560          DELIMITED BY SIZE INTO SSA2                                     
129570     STRING 'WDB622  (IDFKNGRP =' W-IDFKNGRP-X ')'                        
129580          DELIMITED BY SIZE INTO SSA3                                     
129590     MOVE '  GE' TO GODK-STATUSKODER                                      
129591     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3          
129592     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
129593     PERFORM IMS-STATUSKONTROLL                                           
129594     .                                                                    
129595     SKIP3                                                                
129600 IMS-STATUSKONTROLL SECTION.                                              
129700     SET STATUS-IX TO 1                                                   
129800     SEARCH GODK-STATUS                                                   
129900       AT END                                                             
130000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
130100           DELIMITED BY SIZE INTO FELTEXT                                 
130200         DISPLAY FELTEXT                                                  
130300         CALL FELLOG                                                      
130400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
130500         CONTINUE                                                         
130600     END-SEARCH                                                           
130700     .                                                                    
