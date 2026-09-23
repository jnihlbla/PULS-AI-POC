000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4067100.                                                
000400 AUTHOR.         CAP GEMINI / BOH                                         
000500     DATE-WRITTEN.   JAN   86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR ETT FRÅGEPROGRAM SOM KAN GÖRA EN PROGRAM-TO-           
001100*    PROGRAM SWITCH TILL W4067200.                                        
001200*    DET LÄGGER UT SAMTLIGA DESTINATIONER/TRANSPORTNAMN SOM EJ            
001300*    ÄR ÅTERRAPPORTERADE.                                                 
001400*    MAN KAN VÄLJA UT EN RAD ELLER ANGE ETT ID, FÖR ATT ERHÅLLA           
001500*    YTTERLIGARE INFORMATION, SOM DÅ FÅS AV W4067200.                     
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T671                                              
002000*        MID:         W4I67101                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O67101                                            
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W4067100'                             
003100                                 PIC X(8).                                
003200 77  JA                          PIC X(1)    VALUE 'J'.                   
003300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003400 77  INDX                        PIC S9(4)   COMP SYNC.                   
003500 77  RAD-INDX                    PIC S9(4)   COMP SYNC.                   
003600 77  MIN-MOD-LAENGD              PIC S9(4)   VALUE +97  COMP SYNC.        
003700 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +795 COMP SYNC.        
003800 77  WS-BEROUTE                  PIC X(25).                               
003900 77  WS-IDTRANSP-NAMN            PIC X(15).                               
004000 77  WS-IDTRANS                  PIC X(4).                                
004100     88  WS-GODKAEND-BILD                    VALUE '4671' '4672'.         
004200     SKIP3                                                                
004300 01  PROGRAM-TO-PROGRAM-SWITCH   PIC X       VALUE 'N'.                   
004400     88  ALT-MSG-INSERT                      VALUE 'J'.                   
004500     SKIP2                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004900     EJECT                                                                
005000 01  NYCKLAR-TILL-DLI.                                                    
005100                                                                          
005200     03  W-4423-WDGXKEY-X.                                                
005300         05  FILLER              PIC X(4)    VALUE '4423'.                
005400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
005500                                                                          
005600     03  W-4424-WDGXKEY-X.                                                
005700         05  W-4424-BEROUTE.                                              
005800             07  W-INIT-BEROUTE     PIC X.                                
005900             07  W-LOWVALUE-BEROUTE PIC X(24).                            
006000         05  W-4424-IDTRANSP-NAMN   PIC X(15).                            
006100     EJECT                                                                
006200 01  FELMEDDELANDE.                                                       
006300                                                                          
006400     03  FEL1.                                                            
006500         05  FILLER              PIC X(40)   VALUE                        
006600             '836 INFO FÖRÄNDRAD. FÖRSTA SIDAN VISAS'.                    
006700         05  FILLER              PIC X(40)   VALUE                        
006800             '836 INFO CHANGED. FIRST PAGE IS SHOWN'.                     
006900     03  FEL-1 REDEFINES FEL1 OCCURS 2 PIC X(40).                         
007000                                                                          
007100     03  FEL2.                                                            
007200         05  FILLER              PIC X(40)   VALUE                        
007300             '837 INFO FÖRÄNDRAD. STARTA OM MED PF7'.                     
007400         05  FILLER              PIC X(40)   VALUE                        
007500             '837 INFO CHANGED. START AGAIN WITH PF7'.                    
007600     03  FEL-2 REDEFINES FEL2 OCCURS 2 PIC X(40).                         
007700                                                                          
007800     EJECT                                                                
007900 01  INFORMATIONSMEDDELANDE.                                              
008000                                                                          
008100     03  MEDDELANDE-1.                                                    
008200         05  FILLER              PIC X(61)   VALUE                        
008300             'TRYCK PF8 FÖR MER INFORMATION'.                             
008400         05  FILLER              PIC X(61)   VALUE                        
008500             'FOR MORE INFORMATION, USE PF8'.                             
008600     03  MED-1 REDEFINES MEDDELANDE-1 OCCURS 2 PIC X(61).                 
008700                                                                          
008800     03  MEDDELANDE-2.                                                    
008900         05  FILLER              PIC X(61)   VALUE                        
009000             'INFORMATION SAKNAS'.                                        
009100         05  FILLER              PIC X(61)   VALUE                        
009200             'NO INFORMATION AVAILABLE'.                                  
009300     03  MED-2 REDEFINES MEDDELANDE-2 OCCURS 2 PIC X(61).                 
009400     EJECT                                                                
009500 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
009600     SKIP3                                                                
009700 01    FILLER              PIC X(16)   VALUE 'MID W4I671 MID'.            
009800*01  MID -COPY W4I67101.                                                  
010000     EJECT                                                                
010100*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400*    03  MOD -COPY W4O67101  -RED MSG-AREA.                               
010600     EJECT                                                                
010700*01  MOD -COPY W4I67201  -PRE MOD-.                                       
010900     EJECT                                                                
011000*01  -COPY WMFSAREA                                                       
011200     EJECT                                                                
011300 01  IMS-WS.                                                              
011400     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011500     SKIP3                                                                
011600*                        **** STATUS-KOD FRÅN IMS                         
011700     03  STATUS-WS               PIC X(2).                                
011800         88  SEGMENT-FINNS                   VALUE '  '.                  
011900         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
012000     SKIP3                                                                
012100     03  GODK-STATUSKODER.                                                
012200         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*                            IMS FUNKTIONSKODER                           
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*                            DLI INPUT-OUTPUT AREA                        
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(140)  VALUE SPACE.                 
013400     SKIP2                                                                
013500*    03  WLXXDQ11 -COPY WDGX4424   -RED IO-AREA.                          
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900*01  -COPY W0009     -PRE MSG-                                            
014100     EJECT                                                                
014200*01  -COPY W0009     -PRE ALT-                                            
014400     EJECT                                                                
014500*01  -COPY W0008     -PRE XXDQ-                                           
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION USING MSG-PCB ALT-PCB XXDQ-PCB.                       
015000     SKIP2                                                                
015100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB XXDQ-PCB.                      
015200     SKIP2                                                                
015300     PERFORM IMS-GET-MSG                                                  
015400     SKIP1                                                                
015500     IF SEGMENT-FINNS                                                     
015600         PERFORM A-INIT-SPARA-INPUT                                       
015700     SKIP1                                                                
015800         PERFORM IMS-GU-4423                                              
015900     SKIP1                                                                
016000         IF MFS-IDPFK = '7'                                               
016100             PERFORM B-BEHANDLA-PF7                                       
016200         ELSE                                                             
016300             IF MFS-IDPFK = '8'                                           
016400                 PERFORM C-BEHANDLA-PF8                                   
016500             ELSE                                                         
016600                 PERFORM D-BEHANDLA-ENTER                                 
016700             END-IF                                                       
016800         END-IF                                                           
016900     SKIP1                                                                
017000         IF ALT-MSG-INSERT                                                
017100             PERFORM IMS-INSERT-ALT-MSG                                   
017200         ELSE                                                             
017300             IF NOT WS-GODKAEND-BILD                                      
017400                 PERFORM E-RENSA-NYCKLAR                                  
017500             END-IF                                                       
017600             PERFORM IMS-INSERT-MSG                                       
017700         END-IF                                                           
017800     END-IF                                                               
017900     SKIP1                                                                
018000     MOVE ZERO TO RETURN-CODE                                             
018100     GOBACK                                                               
018200     .                                                                    
018300     EJECT                                                                
018400 A-INIT-SPARA-INPUT SECTION.                                              
018500     SKIP2                                                                
018600     IF MSG-DUBBLA-TRANSKODER                                             
018700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I67101               
018800         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
018900         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
019000         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
019100         MOVE MSG-IDPFK TO MFS-IDPFK                                      
019200     ELSE                                                                 
019300         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I67101                
019400         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
019500         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
019600         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
019700     END-IF                                                               
019800     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
019900     SKIP1                                                                
020000     IF MID-BEROUTE-IN = ALL '+'                                          
020100         MOVE MID-BEROUTE-UT TO WS-BEROUTE                                
020200     ELSE                                                                 
020300         MOVE MID-BEROUTE-IN TO WS-BEROUTE                                
020400     END-IF                                                               
020500     SKIP1                                                                
020600     IF MID-IDTRANSP-NAMN-IN = ALL '+'                                    
020700         MOVE MID-IDTRANSP-NAMN-UT TO WS-IDTRANSP-NAMN                    
020800     ELSE                                                                 
020900         MOVE MID-IDTRANSP-NAMN-IN TO WS-IDTRANSP-NAMN                    
021000     END-IF                                                               
021100     SKIP1                                                                
021200     MOVE LOW-VALUE      TO MSG-AREA                                      
021300     MOVE 'W4O67101'     TO MFS-IDMOD                                     
021400     MOVE '4671'         TO MOD-IDTRANS                                   
021500     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
021600     SKIP1                                                                
021700     MOVE WS-BEROUTE       TO MOD-BEROUTE-UT                              
021800     MOVE WS-IDTRANSP-NAMN TO MOD-IDTRANSP-NAMN-UT                        
021900     SKIP1                                                                
022000     IF SWEDISH-TEXT                                                      
022100         MOVE +1 TO INDX                                                  
022200     ELSE                                                                 
022300         MOVE +2 TO INDX                                                  
022400     END-IF                                                               
022500     SKIP1                                                                
022600     MOVE MFS-RENSA-FAELT TO MOD-BEROUTE-IN                               
022700                             MOD-IDTRANSP-NAMN-IN                         
022800                             MOD-BEROUTE-SPA                              
022900                             MOD-IDTRANSP-NAMN-SPA                        
023000                             MOD-TEMFSFEL                                 
023100                             MOD-TEMFSINF                                 
023200     SKIP1                                                                
023300     MOVE +1 TO RAD-INDX                                                  
023400     SKIP1                                                                
023500     PERFORM UNTIL RAD-INDX NOT < +15                                     
023600         MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-INDX)                     
023700                                 MOD-BEROUTE (RAD-INDX)                   
023800                                 MOD-IDTRANSP-NAMN (RAD-INDX)             
023900         ADD +1 TO RAD-INDX                                               
024000     END-PERFORM                                                          
024100     SKIP1                                                                
024200     MOVE +1 TO RAD-INDX                                                  
024300     .                                                                    
024400     EJECT                                                                
024500 B-BEHANDLA-PF7 SECTION.                                                  
024600     SKIP2                                                                
024700     PERFORM S03-LAEGG-UT-SIDA-ETT                                        
024800     .                                                                    
024900     EJECT                                                                
025000 C-BEHANDLA-PF8 SECTION.                                                  
025100     SKIP2                                                                
025200     IF MID-BEROUTE-SPA = ALL '+'                                         
025300     AND MID-IDTRANSP-NAMN-SPA = ALL '+'                                  
025400     SKIP1                                                                
025500         IF MID-BEROUTE (RAD-INDX) = ALL '+'                              
025600         AND MID-IDTRANSP-NAMN (RAD-INDX) = ALL '+'                       
025700             PERFORM S03-LAEGG-UT-SIDA-ETT                                
025800         ELSE                                                             
025900             MOVE MFS-ROER-EJ-FAELT TO MOD-BEROUTE-SPA                    
026000             MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRANSP-NAMN-SPA              
026100             MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSINF                       
026200     SKIP1                                                                
026300             PERFORM UNTIL RAD-INDX NOT < +15                             
026400                 PERFORM S04-OFOERAENDRAD-BILD-UT                         
026500                 ADD +1 TO RAD-INDX                                       
026600             END-PERFORM                                                  
026700         END-IF                                                           
026800     ELSE                                                                 
026900         MOVE MID-BEROUTE-SPA       TO W-4424-BEROUTE                     
027000         MOVE MID-IDTRANSP-NAMN-SPA TO W-4424-IDTRANSP-NAMN               
027100     SKIP1                                                                
027200         PERFORM IMS-GNP-4424-KVAL-RAD-EL-SPA                             
027300         IF SEGMENT-FINNS                                                 
027400             PERFORM S01-REDIGERA-BILD                                    
027500         ELSE                                                             
027600             MOVE FEL-1(INDX) TO MOD-TEMFSFEL                             
027700             PERFORM S03-LAEGG-UT-SIDA-ETT                                
027800         END-IF                                                           
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 D-BEHANDLA-ENTER SECTION.                                                
028300     SKIP2                                                                
028400     IF MID-BEROUTE-IN = ALL '+'                                          
028500     AND MID-IDTRANSP-NAMN-IN = ALL '+'                                   
028600         PERFORM DA-INGA-NYCKLAR-IFYLLDA                                  
028700     ELSE                                                                 
028800         PERFORM DB-NYCKLAR-IFYLLDA                                       
028900     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 DA-INGA-NYCKLAR-IFYLLDA SECTION.                                         
029300     SKIP2                                                                
029400     IF MID-BEROUTE (RAD-INDX) = ALL '+'                                  
029500     AND MID-IDTRANSP-NAMN (RAD-INDX) = ALL '+'                           
029600     OR MFS-IDTRANS NOT = '4671'                                          
029700     SKIP1                                                                
029800         IF MFS-IDTRANS = '4672'                                          
029900             MOVE WS-BEROUTE TO W-4424-BEROUTE                            
030000             MOVE WS-IDTRANSP-NAMN TO W-4424-IDTRANSP-NAMN                
030100             PERFORM IMS-GNP-4424-KVAL-WS-ID                              
030200             IF SEGMENT-FINNS                                             
030300                 PERFORM S01-REDIGERA-BILD                                
030400             ELSE                                                         
030500                 PERFORM S03-LAEGG-UT-SIDA-ETT                            
030600             END-IF                                                       
030700         ELSE                                                             
030800             PERFORM S03-LAEGG-UT-SIDA-ETT                                
030900         END-IF                                                           
031000     SKIP1                                                                
031100     ELSE                                                                 
031200         PERFORM UNTIL RAD-INDX NOT < +15                                 
031300         OR ALT-MSG-INSERT                                                
031400                                                                          
031500             IF MID-KDCMD (RAD-INDX) = 'X'                                
031600                 MOVE MID-BEROUTE (RAD-INDX) TO                           
031700                                     W-4424-BEROUTE                       
031800                 MOVE MID-IDTRANSP-NAMN (RAD-INDX) TO                     
031900                                     W-4424-IDTRANSP-NAMN                 
032000                                                                          
032100                 PERFORM IMS-GNP-4424-KVAL-RAD-EL-SPA                     
032200                 IF SEGMENT-FINNS                                         
032300                     PERFORM S02-SPARKA-IGANG-W4T672                      
032400                     MOVE JA TO PROGRAM-TO-PROGRAM-SWITCH                 
032500                 ELSE                                                     
032600                     MOVE FEL-2 (INDX) TO MOD-TEMFSFEL                    
032700                 END-IF                                                   
032800             ELSE                                                         
032900                 IF RAD-INDX = +14                                        
033000                     MOVE MFS-ROER-EJ-FAELT TO MOD-BEROUTE-SPA            
033100                     MOVE MFS-ROER-EJ-FAELT TO                            
033200                                         MOD-IDTRANSP-NAMN-SPA            
033300                     MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSINF               
033400     SKIP1                                                                
033500                     MOVE +1 TO RAD-INDX                                  
033600     SKIP1                                                                
033700                     PERFORM UNTIL RAD-INDX NOT < +15                     
033800                         PERFORM S04-OFOERAENDRAD-BILD-UT                 
033900     SKIP1                                                                
034000                         ADD +1 TO RAD-INDX                               
034100     SKIP1                                                                
034200                     END-PERFORM                                          
034300                 END-IF                                                   
034400             END-IF                                                       
034500     SKIP1                                                                
034600             ADD +1 TO RAD-INDX                                           
034700     SKIP1                                                                
034800         END-PERFORM                                                      
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 DB-NYCKLAR-IFYLLDA SECTION.                                              
035300     SKIP2                                                                
035400     MOVE WS-BEROUTE       TO W-4424-BEROUTE                              
035500     MOVE WS-IDTRANSP-NAMN TO W-4424-IDTRANSP-NAMN                        
035600     SKIP1                                                                
035700     PERFORM IMS-GNP-4424-KVAL-WS-ID                                      
035800     SKIP1                                                                
035900     IF SEGMENT-FINNS                                                     
036000         PERFORM S02-SPARKA-IGANG-W4T672                                  
036100         MOVE JA TO PROGRAM-TO-PROGRAM-SWITCH                             
036200     ELSE                                                                 
036300         MOVE LOW-VALUE    TO W-LOWVALUE-BEROUTE                          
036400                              W-4424-IDTRANSP-NAMN                        
036500         PERFORM IMS-LAES-FRAN-INITIAL                                    
036600         IF SEGMENT-FINNS                                                 
036700             PERFORM S01-REDIGERA-BILD                                    
036800         ELSE                                                             
036900             PERFORM S03-LAEGG-UT-SIDA-ETT                                
037000         END-IF                                                           
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 E-RENSA-NYCKLAR SECTION.                                                 
037500     MOVE MFS-RENSA-FAELT            TO MOD-BEROUTE-UT                    
037600                                        MOD-IDTRANSP-NAMN-UT              
037700     .                                                                    
037800     EJECT                                                                
037900 S01-REDIGERA-BILD SECTION.                                               
038000     SKIP2                                                                
038100     PERFORM S05-FYLL-RAD-I-MOD                                           
038200     ADD +1 TO RAD-INDX                                                   
038300     SKIP1                                                                
038400     PERFORM UNTIL RAD-INDX NOT < +16                                     
038500     OR SEGMENT-SAKNAS                                                    
038600     SKIP1                                                                
038700         PERFORM IMS-GNP-4424                                             
038800     SKIP1                                                                
038900         IF SEGMENT-FINNS                                                 
039000             IF RAD-INDX = +15                                            
039100                 MOVE TCENT-BEROUTE       TO MOD-BEROUTE-SPA              
039200                 MOVE TCENT-IDTRANSP-NAMN TO MOD-IDTRANSP-NAMN-SPA        
039300                 MOVE MED-1(INDX)         TO MOD-TEMFSINF                 
039400             ELSE                                                         
039500                 PERFORM S05-FYLL-RAD-I-MOD                               
039600             END-IF                                                       
039700     SKIP1                                                                
039800         ADD +1 TO RAD-INDX                                               
039900     SKIP1                                                                
040000         END-IF                                                           
040100     END-PERFORM                                                          
040200     .                                                                    
040300     EJECT                                                                
040400 S02-SPARKA-IGANG-W4T672 SECTION.                                         
040500     SKIP2                                                                
040600     MOVE TCENT-BEROUTE       TO MOD-MID-BEROUTE-IN                       
040700     MOVE TCENT-IDTRANSP-NAMN TO MOD-MID-IDTRANSP-NAMN-IN                 
040800     MOVE MIN-MOD-LAENGD      TO MSG-KVLL                                 
040900     MOVE 'W4T672  '          TO MSG-KDTRANS-1                            
041000     MOVE '4671'              TO MSG-IDTRANS-1                            
041100     MOVE 1                   TO MSG-KDMFSFOR-1                           
041200     MOVE MOD-MID-W4I67201    TO MSG-INDATA-MINUS-1-TRANSKOD              
041300     .                                                                    
041400     EJECT                                                                
041500 S03-LAEGG-UT-SIDA-ETT SECTION.                                           
041600     SKIP2                                                                
041700     PERFORM IMS-GNP-FIRST-4424                                           
041800     SKIP1                                                                
041900     IF SEGMENT-FINNS                                                     
042000         PERFORM S01-REDIGERA-BILD                                        
042100     ELSE                                                                 
042200         MOVE MED-2(INDX) TO MOD-TEMFSINF                                 
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 S04-OFOERAENDRAD-BILD-UT SECTION.                                        
042700     SKIP2                                                                
042800     MOVE MFS-ROER-EJ-FAELT TO                                            
042900                   MOD-BEROUTE (RAD-INDX)                                 
043000                   MOD-IDTRANSP-NAMN (RAD-INDX)                           
043100     SKIP1                                                                
043200     IF MID-BEROUTE (RAD-INDX) NOT = ALL '+'                              
043300     AND MID-IDTRANSP-NAMN (RAD-INDX) NOT = ALL '+'                       
043400         MOVE MFS-OEPPNA-ALFA-FAELT TO                                    
043500                   MOD-KDCMD-ATTR (RAD-INDX)                              
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 S05-FYLL-RAD-I-MOD SECTION.                                              
044000     SKIP2                                                                
044100     MOVE TCENT-BEROUTE         TO MOD-BEROUTE (RAD-INDX)                 
044200     MOVE TCENT-IDTRANSP-NAMN   TO MOD-IDTRANSP-NAMN (RAD-INDX)           
044300     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDCMD-ATTR (RAD-INDX)              
044400     EJECT                                                                
044500* IMS SEKTIONER                                                           
044600     SKIP3                                                                
044700     .                                                                    
044800 IMS-GET-MSG SECTION.                                                     
044900     MOVE '  QC' TO GODK-STATUSKODER                                      
045000     CALL CBLTDLI USING GU                                                
045100                          MSG-PCB                                         
045200                          MSG-IO-AREA                                     
045300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     SKIP3                                                                
045600     .                                                                    
045700 IMS-INSERT-MSG SECTION.                                                  
045800     IF ENGLISH-TEXT                                                      
045900       MOVE 'N' TO MFS-KDHUVOMR                                           
046000     END-IF                                                               
046100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
046200     MOVE SPACE TO GODK-STATUSKODER                                       
046300     CALL CBLTDLI USING ISRT                                              
046400                          MSG-PCB                                         
046500                          MSG-IO-AREA                                     
046600                          MFS-IDMOD                                       
046700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046800     PERFORM IMS-STATUSKONTROLL                                           
046900     SKIP3                                                                
047000     .                                                                    
047100 IMS-INSERT-ALT-MSG SECTION.                                              
047200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
047300     MOVE SPACE TO GODK-STATUSKODER                                       
047400     CALL CBLTDLI USING ISRT                                              
047500                          ALT-PCB                                         
047600                          MSG-IO-AREA                                     
047700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
047800     PERFORM IMS-STATUSKONTROLL                                           
047900     .                                                                    
048000     EJECT                                                                
048100 IMS-GU-4423 SECTION.                                                     
048200     STRING 'WLXXDQ01(WDGXKEY  =' W-4423-WDGXKEY-X ')'                    
048300            DELIMITED BY SIZE INTO SSA1                                   
048400     MOVE '  ' TO GODK-STATUSKODER                                        
048500     CALL CBLTDLI USING GU                                                
048600                          XXDQ-PCB                                        
048700                          DLI-IO-AREA                                     
048800                          SSA1                                            
048900     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
049000     PERFORM IMS-STATUSKONTROLL                                           
049100     SKIP3                                                                
049200     .                                                                    
049300 IMS-GNP-FIRST-4424 SECTION.                                              
049400     MOVE 'WLXXDQ11*F' TO SSA1                                            
049500     MOVE '  GE' TO GODK-STATUSKODER                                      
049600     CALL CBLTDLI USING GNP                                               
049700                          XXDQ-PCB                                        
049800                          DLI-IO-AREA                                     
049900                          SSA1                                            
050000     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     SKIP3                                                                
050300     .                                                                    
050400 IMS-GNP-4424-KVAL-RAD-EL-SPA SECTION.                                    
050500     STRING 'WLXXDQ11*F(WDGXKEY  =' W-4424-WDGXKEY-X ')'                  
050600            DELIMITED BY SIZE INTO SSA1                                   
050700     MOVE '  GE' TO GODK-STATUSKODER                                      
050800     CALL CBLTDLI USING GNP                                               
050900                          XXDQ-PCB                                        
051000                          DLI-IO-AREA                                     
051100                          SSA1                                            
051200     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
051300     PERFORM IMS-STATUSKONTROLL                                           
051400     .                                                                    
051500     EJECT                                                                
051600 IMS-GNP-4424-KVAL-WS-ID SECTION.                                         
051700     STRING 'WLXXDQ11*F(WDGXKEY  =' W-4424-WDGXKEY-X ')'                  
051800            DELIMITED BY SIZE INTO SSA1                                   
051900     MOVE '  GE' TO GODK-STATUSKODER                                      
052000     CALL CBLTDLI USING GNP                                               
052100                          XXDQ-PCB                                        
052200                          DLI-IO-AREA                                     
052300                          SSA1                                            
052400     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     SKIP3                                                                
052700     .                                                                    
052800 IMS-GNP-4424 SECTION.                                                    
052900     MOVE 'WLXXDQ11 ' TO SSA1                                             
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GNP                                               
053200                          XXDQ-PCB                                        
053300                          DLI-IO-AREA                                     
053400                          SSA1                                            
053500     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     SKIP3                                                                
053800     .                                                                    
053900 IMS-LAES-FRAN-INITIAL SECTION.                                           
054000     STRING 'WLXXDQ11*F(WDGXKEY =>' W-4424-WDGXKEY-X ')'                  
054100            DELIMITED BY SIZE INTO SSA1                                   
054200     MOVE '  GE' TO GODK-STATUSKODER                                      
054300     CALL CBLTDLI USING GNP                                               
054400                          XXDQ-PCB                                        
054500                          DLI-IO-AREA                                     
054600                          SSA1                                            
054700     MOVE XXDQ-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     EJECT                                                                
055100 IMS-STATUSKONTROLL SECTION.                                              
055200     SET STATUS-IX TO 1                                                   
055300     SEARCH GODK-STATUS AT END CALL FELLOG                                
055400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
055500     END-SEARCH                                                           
055600     .                                                                    
