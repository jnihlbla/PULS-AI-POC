000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL01TIDZ.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   JULY  05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        GENERELLT SUBPROGRAM FÖR ATT OMVANDLA                            
000900*        MASKINDATUM/MASKINTID TILL LOKAL DITO.                           
001000*                                                                         
001100*    INDATA.                                                              
001200*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001300*          MSGI-WL01TIDZ                                                  
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MSGI-WL01TIDZ                                                    
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400*    -- CHECKED BY WY2000                                                 
002500                                                                          
002600 77  IDPGM                   PIC X(8)    VALUE 'WL01TIDZ'.                
002700 77  W-COMPILED              PIC X(16)   VALUE SPACE.                     
002800 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
002900 77  JA                      PIC X       VALUE 'J'.                       
003000 77  NEJ                     PIC X       VALUE 'N'.                       
003100 77  INDX                    PIC S9(9)   VALUE ZERO COMP SYNC.            
003200 77  WS-DIFF                 PIC S9(3)   VALUE ZERO COMP-3.               
003300 77  WS-DATE                 PIC 9(6)    VALUE ZERO.                      
003400 77  WS-TIME                 PIC 9(8)    VALUE ZERO.                      
003500 77  WS-IDTIDZON             PIC 9(2)    VALUE ZERO.                      
003600 77  WS-IDLTERM-USER         PIC X(8)    VALUE SPACE.                     
003700 77  WS-IDUSER-IDDC          PIC X(5)    VALUE SPACE.                     
003800 77  WS-IDDC                 PIC X(2)    VALUE SPACE.                     
003900 77  WS-IDTRANS              PIC X(4)    VALUE SPACE.                     
004000 77  WS-SPAR-USER            PIC X(200)  VALUE SPACE.                     
004100 77  WS-TILOKDAT             PIC X(6)    VALUE ZERO.                      
004200 77  WS-TILOKTID             PIC X(8)    VALUE ZERO.                      
       77  TZRUL-HIT-SW                PIC X       VALUE 'N'.                   
           88  TZRUL-HIT-NO                        VALUE 'N'.                   
           88  TZRUL-HIT-YES                       VALUE 'J'.                   
004300                                                                          
004600                                                                          
004700 01  WS-IDUSER-LTERM.                                                     
004800   03  FILLER                PIC X(9)    VALUE 'IDUSER ='.                
004900   03  WS-IDUSER.                                                         
005000     05  WS-IDUSER-POS1-5.                                                
005100      07  WS-IDUSER-POS1-2.                                               
005200       09  WS-IDUSER-POS1    PIC X(1)    VALUE SPACE.                     
005300       09  WS-IDUSER-POS2    PIC X(1)    VALUE SPACE.                     
005400      07  WS-IDUSER-POS3-4.                                               
005500       09  WS-IDUSER-POS3    PIC X(1)    VALUE SPACE.                     
005600       09  WS-IDUSER-POS4    PIC X(1)    VALUE SPACE.                     
005700      07  WS-IDUSER-POS5     PIC X(1)    VALUE SPACE.                     
005800     05  FILLER              PIC X(3)    VALUE SPACE.                     
005900                                                                          
006000                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006300                                                                          
006400     EJECT                                                                
006500 01  TZ-FULL-DATE            PIC 9(8)    VALUE ZERO.                      
006600 01  FILLER REDEFINES TZ-FULL-DATE.                                       
006700   03  TZ-TILOKDAT-CENT      PIC 9(2).                                    
006800   03  TZ-TILOKDAT           PIC X(6).                                    
006900   03  FILLER REDEFINES TZ-TILOKDAT.                                      
007000     05  TZ-AA               PIC 9(2).                                    
007100     05  TZ-MM               PIC 9(2).                                    
007200     05  TZ-DD               PIC 9(2).                                    
007300                                                                          
007400 01  TZ-INT-DATE             PIC S9(9)   BINARY.                          
007500                                                                          
007600 01  TZ-TILOKTID             PIC X(4)    VALUE ZERO.                      
007700 01  FILLER REDEFINES TZ-TILOKTID.                                        
007800   03  TZ-TIME-HOUR          PIC 9(2).                                    
007900   03  TZ-TIME-MIN           PIC 9(2).                                    
008000                                                                          
008100 01  TZ-HOUR                 PIC S9(3)   COMP-3.                          
008200 01  TZ-MIN                  PIC S9(3)   COMP-3.                          
008300 01  TZ-DIFF                 PIC S9(3)   COMP-3.                          
008400                                                                          
008500 77  TZ-SKOTT-AR             PIC X(2)    VALUE SPACE.                     
008600   88  SKOTT-AR                              VALUE '00'                   
008700                '04' '08' '12' '16' '20' '24' '28' '32'                   
008800                '36' '40' '44' '48' '52' '56' '60' '64'                   
008900                '68' '72' '76' '80' '84' '88' '92' '96'.                  
009000                                                                          
009100     EJECT                                                                
009200 01  TZ-RULE-TABELL.                                                      
009300*                   EN MASKIN OCH TABELLSTOPP MÅSTE FINNAS                
009400*       TIDSZON 02 = AUSTRALIA                                            
009500*       TIDSZON 07 = INDIEN                                               
009600*       TIDSZON 10 = FINLAND FOR INSTANCE (NOT USED TODAY)                
      *                    SOUTH AFRICA                                         
009700*       TIDSZON 11 = EUROPE EXCEPT FOR THE UK                             
009800*       TIDSZON 12 = THE UK                                               
009900*       TIDSZON 17 = USA (DC 41, 42) AND CANADA                           
010000*       TIDSZON 18 = USA (DC 45, 47)                                      
010100*       TIDSZON 20 = USA (DC 43, DC44)                                    
010200*                                                                         
010300   03  TIDZON-MASKIN-2024.                                                
010400     05 FILLER               PIC X(16) VALUE '    240331241027'.          
010500     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
010600   03  TIDZON-MASKIN-2025.                                                
010700     05 FILLER               PIC X(16) VALUE '    250330251026'.          
010800     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
010900   03  TIDZON-MASKIN-2026.                                                
011000     05 FILLER               PIC X(16) VALUE '    260329261025'.          
011100     05 FILLER               PIC S9(3) VALUE +1  COMP-3.                  
011200                                                                          
011300*       TIDSZON 02 = AUSTRALIA                                            
011400   03  TIDZON-02-2024.                                                    
011500     05 FILLER               PIC X(16) VALUE '02  231001240407'.          
011600     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
011700   03  TIDZON-02-2025.                                                    
011800     05 FILLER               PIC X(16) VALUE '02  241006250406'.          
011900     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
012000   03  TIDZON-02-2026.                                                    
012100     05 FILLER               PIC X(16) VALUE '02  251005260405'.          
012200     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
012300   03  TIDZON-02-2027.                                                    
012400     05 FILLER               PIC X(16) VALUE '02  261004270404'.          
012500     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
012600                                                                          
012700*       TIDSZON 10 = FINLAND AND SOUTH AFRICA                             
012800   03  TIDZON-10-DC85.                                                    
012900     05 FILLER               PIC X(16) VALUE '1085010101991231'.          
013000     05 FILLER               PIC S9(3) VALUE 0   COMP-3.                  
013100                                                                          
013200   03  TIDZON-10-2024.                                                    
013300     05 FILLER               PIC X(16) VALUE '10  240331241027'.          
013400     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
013500   03  TIDZON-10-2025.                                                    
013600     05 FILLER               PIC X(16) VALUE '10  250330251026'.          
013700     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
013800   03  TIDZON-10-2026.                                                    
013900     05 FILLER               PIC X(16) VALUE '10  260329261025'.          
014000     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
014100*       TIDSZON 11 = EUROPE EXCEPT FOR THE UK                             
014200   03  TIDZON-11-2024.                                                    
014300     05 FILLER               PIC X(16) VALUE '11  240331241027'.          
014400     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
014500   03  TIDZON-11-2025.                                                    
014600     05 FILLER               PIC X(16) VALUE '11  250330251026'.          
014700     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
014800   03  TIDZON-11-2026.                                                    
014900     05 FILLER               PIC X(16) VALUE '11  260329261025'.          
015000     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
015100                                                                          
015200*       TIDSZON 12 = THE UK                                               
015300   03  TIDZON-12-2024.                                                    
015400     05 FILLER               PIC X(16) VALUE '12  240331241027'.          
015500     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
015600   03  TIDZON-12-2025.                                                    
015700     05 FILLER               PIC X(16) VALUE '12  250330251026'.          
015800     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
015900   03  TIDZON-12-2026.                                                    
016000     05 FILLER               PIC X(16) VALUE '12  260329261025'.          
016100     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
016200                                                                          
016300*       TIDSZON 17 = USA (DC41, DC46, DC51)                               
016400   03  TIDZON-17-2024.                                                    
016500     05 FILLER               PIC X(16) VALUE '17  240310241103'.          
016600     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
016700   03  TIDZON-17-2025.                                                    
016800     05 FILLER               PIC X(16) VALUE '17  250309251102'.          
016900     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
017000   03  TIDZON-17-2026.                                                    
017100     05 FILLER               PIC X(16) VALUE '17  260308261101'.          
017200     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
017300                                                                          
017400*       TIDSZON 18 = CHICAGO(DC 45)                                       
017500   03  TIDZON-18-2023.                                                    
017600     05 FILLER               PIC X(16) VALUE '18  230312231105'.          
017700     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
017800   03  TIDZON-18-2024.                                                    
017900     05 FILLER               PIC X(16) VALUE '18  240310241103'.          
018000     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
018100   03  TIDZON-18-2025.                                                    
018200     05 FILLER               PIC X(16) VALUE '18  250309251102'.          
018300     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
018400   03  TIDZON-18-2026.                                                    
018500     05 FILLER               PIC X(16) VALUE '18  260308261101'.          
018600     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
018700                                                                          
018800*       TIDSZON 20 = USA (DC 43, DC44, DC47)                              
018900   03  TIDZON-20-2024.                                                    
019000     05 FILLER               PIC X(16) VALUE '20  240310241103'.          
019100     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
019200   03  TIDZON-20-2025.                                                    
019300     05 FILLER               PIC X(16) VALUE '20  250309251102'.          
019400     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
019500   03  TIDZON-20-2026.                                                    
019600     05 FILLER               PIC X(16) VALUE '20  260308261101'.          
019700     05 FILLER               PIC S9(3) VALUE -1  COMP-3.                  
019800                                                                          
019900   03  TIDZON-TABELL-STOPP.                                               
020000     05 FILLER               PIC X(16) VALUE '99ZZ000000000000'.          
020100     05 FILLER               PIC S9(3) VALUE +0  COMP-3.                  
020200                                                                          
020300*  -- LITE EXTRA SPACE SÅ BEHÖVER ANTAL OCCURS INTE VARA EXAKT            
020400   03  FILLER                PIC X(400) VALUE SPACE.                      
020500                                                                          
020600 01  FILLER REDEFINES TZ-RULE-TABELL.                                     
020700*  -- OCCURS TILLTAGET I ÖVERKANT - CA 36 ANVÄNDS                         
020800   03 TZRUL OCCURS  50.                                                   
020900     05  TZRUL-IDTIDZON      PIC 9(2).                                    
021000     05  TZRUL-IDDC          PIC X(2).                                    
021100     05  TZRUL-DAT-FOM       PIC 9(6).                                    
021200     05  TZRUL-DAT-TOM       PIC 9(6).                                    
021300     05  TZRUL-DIFF          PIC S9(3)            COMP-3.                 
021400                                                                          
021500     EJECT                                                                
021600 LINKAGE SECTION.                                                         
021700*01  -COPY WL01TIDZ                                                       
021800     EJECT                                                                
021900*01  -COPY W0008 -PRE USEA-                                               
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200                                                                          
022300 PROCEDURE DIVISION USING  MSGI-WL01TIDZ.                                 
022400 STYR SECTION.                                                            
022500                                                                          
022600     PERFORM A-INIT                                                       
022700     IF MSGI-KDCALL = '011'                                               
022800*       KONV FRÅN MASKINTID TILL LOCAL-TIME                               
022900     OR MSGI-KDCALL = '012'                                               
023000*       KONV FRÅN LOCAL-TIME TILL MASKINTID                               
023100     OR MSGI-KDCALL = '013'                                               
023200*       KONV FRÅN UTC TILL LOCAL-TIME                                     
023300     OR MSGI-KDCALL = '014'                                               
023400*       KONV FRÅN LOCAL-TIME TILL UTC                                     
023500        MOVE MSGI-TILOKDAT       TO WS-TILOKDAT                           
023600        MOVE MSGI-TILOKTID (1:4) TO WS-TILOKTID                           
023700        MOVE MSGI-IDTIDZON       TO WS-IDTIDZON                           
023800        MOVE MSGI-IDDC           TO WS-IDDC                               
023900        PERFORM F-KOLLA-TIDZON                                            
024000     ELSE                                                                 
024100        MOVE 'FEL KDCALL-TYP EJ = 11, 12, 13, 14 ' TO FELTEXT             
024200        MOVE 'F' TO MSGI-KDSVAR                                           
024300     END-IF                                                               
024400                                                                          
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 A-INIT SECTION.                                                          
025000                                                                          
025100     MOVE WHEN-COMPILED TO W-COMPILED                                     
025200     .                                                                    
025300     EJECT                                                                
025400 F-KOLLA-TIDZON SECTION.                                                  
025500     IF TZ-AA > 50                                                        
025600       MOVE 19 TO TZ-TILOKDAT-CENT                                        
025700     ELSE                                                                 
025800       MOVE 20 TO TZ-TILOKDAT-CENT                                        
025900     END-IF                                                               
026000     MOVE WS-TILOKDAT TO TZ-TILOKDAT                                      
026100     MOVE WS-TILOKTID TO TZ-TILOKTID                                      
026200                                                                          
026300     MOVE SPACE TO MSGI-KDSVAR                                            
026400                                                                          
026500     PERFORM FA-CHECK-INPUT                                               
026600                                                                          
026700     IF MSGI-KDSVAR = SPACE                                               
026800       PERFORM FB-CALC-TZ-DIFF                                            
026900                                                                          
027000*    -- CONVERT THE DATE TO AN INTEGER VALUE (COUNTING FROM 1600)         
027100       COMPUTE TZ-INT-DATE =                                              
027200                           FUNCTION INTEGER-OF-DATE (TZ-FULL-DATE)        
027300                                                                          
027400*    -- EXTRACT THE HOUR-PART OF THE INPUT TIME                           
027500       MOVE TZ-TIME-HOUR     TO TZ-HOUR                                   
027600       MOVE TZ-TIME-MIN      TO TZ-MIN                                    
027700                                                                          
027800*    --TO ADJUST TIME FOR INDIA                                           
027900       IF MSGI-IDTIDZON = 07                                              
028000         IF MSGI-KDCALL = '012' OR '014'                                  
028100           SUBTRACT 30 FROM TZ-MIN                                        
028200           IF TZ-MIN < 0                                                  
028300             SUBTRACT 1 FROM TZ-HOUR                                      
028400             ADD 60 TO TZ-MIN                                             
028500           END-IF                                                         
028600         ELSE                                                             
028700           ADD 30 TO TZ-MIN                                               
028800           IF TZ-MIN > 59                                                 
028900             ADD 1 TO TZ-HOUR                                             
029000             SUBTRACT 60 FROM TZ-MIN                                      
029100           END-IF                                                         
029200         END-IF                                                           
029300       END-IF                                                             
029400                                                                          
029500*    -- AND COMPUTE THE NEW TIME AND POSSIBLY ALSO THE NEW DATE           
029600*        CONVERTS DATE AND TIME BETWEEN TWO TIME ZONES                    
029700                                                                          
029800       ADD TZ-DIFF TO TZ-HOUR                                             
029900       IF TZ-HOUR  < 0                                                    
030000         ADD 24 TO TZ-HOUR                                                
030100         SUBTRACT 1 FROM TZ-INT-DATE                                      
030200       END-IF                                                             
030300       IF TZ-HOUR  > 23                                                   
030400         SUBTRACT 24 FROM TZ-HOUR                                         
030500         ADD 1 TO TZ-INT-DATE                                             
030600       END-IF                                                             
030700                                                                          
030800*    -- CONVERT BACK TO DISPLAY FORMAT AND RETURN THE                     
030900*    -- VALUES IN THE OUTPUT FIELDS                                       
031000       COMPUTE TZ-FULL-DATE =                                             
031100                            FUNCTION DATE-OF-INTEGER (TZ-INT-DATE)        
031200       MOVE TZ-HOUR     TO TZ-TIME-HOUR                                   
031300       MOVE TZ-MIN      TO TZ-TIME-MIN                                    
031400                                                                          
031500       MOVE TZ-TILOKDAT TO MSGI-TILOKDAT                                  
031600       MOVE TZ-TILOKTID TO MSGI-TILOKTID                                  
031700     END-IF                                                               
031800     .                                                                    
031900                                                                          
032000 FA-CHECK-INPUT SECTION.                                                  
032100                                                                          
032200     MOVE TZ-TILOKDAT TO TZ-SKOTT-AR                                      
032300     IF TZ-TILOKDAT NOT NUMERIC                                           
032400         OR TZ-MM > 12 OR = 0                                             
032500         OR TZ-DD > 31  OR = 0                                            
032600         OR (TZ-MM = 4 OR 6 OR 9 OR 11) AND TZ-DD > 30                    
032700         OR (TZ-MM = 2 AND SKOTT-AR AND TZ-DD > 29)                       
032800         OR (TZ-MM = 2 AND NOT SKOTT-AR AND TZ-DD > 28)                   
032900       MOVE ' INVALID DATE' TO FELTEXT                                    
033000       MOVE 'F' TO MSGI-KDSVAR                                            
033100     END-IF                                                               
033200                                                                          
033300     IF TZ-TILOKTID NOT NUMERIC                                           
033400         OR TZ-TIME-HOUR > 23                                             
033500       MOVE ' INVALID TIME'  TO FELTEXT                                   
033600       MOVE 'F'              TO MSGI-KDSVAR                               
033700     END-IF                                                               
033800                                                                          
033900     IF MSGI-IDTIDZON NUMERIC                                             
034000     AND MSGI-IDTIDZON < 24                                               
034100        MOVE MSGI-IDTIDZON       TO WS-IDTIDZON                           
034200     ELSE                                                                 
034300       MOVE ' INVALID TIDZON'    TO FELTEXT                               
034400       MOVE 'F'                  TO MSGI-KDSVAR                           
034500     END-IF                                                               
034600     .                                                                    
034700                                                                          
034800 FB-CALC-TZ-DIFF SECTION.                                                 
034900                                                                          
035000     MOVE ZERO TO WS-DIFF                                                 
035100                                                                          
035200     IF MSGI-KDCALL = '011' OR '012'                                      
035300       PERFORM FBA-USING-COMPUTER-TIME *> SWEDEN (CET/CEST)               
035400     ELSE                                                                 
035500       PERFORM FBB-USING-UTC                                              
035600     END-IF                                                               
035700     .                                                                    
035800                                                                          
035900 FBA-USING-COMPUTER-TIME SECTION.                                         
036000                                                                          
036100*    -- CHANGE FOR DAYLIGHT SAVING TIME IN COMPUTER                       
036200*    -- NOTE: COMPUTER TIME CHANGES ON SUNDAY NIGHT, NOT                  
036300*    -- MORNING. THEREFOR THE TEST IS > AND <=                            
036400     MOVE +1 TO INDX                                                      
036500     PERFORM UNTIL TZRUL-IDTIDZON (INDX) NOT = SPACE                      
036600       IF TZ-TILOKDAT > TZRUL-DAT-FOM (INDX)                              
036700           AND TZ-TILOKDAT <= TZRUL-DAT-TOM (INDX)                        
036800         ADD TZRUL-DIFF (INDX) TO WS-DIFF                                 
036900       END-IF                                                             
037000       ADD +1 TO INDX                                                     
037100     END-PERFORM                                                          
037200                                                                          
037300*    -- CHANGE FOR DAYLIGHT SAVING TIME IN WAREHOUSE                      
037400*    -- NOTE: SUMMER/WINTER TIME CHANGES IN THE MORNING,                  
037500*    -- THEREFOR THE TEST IS >= AND <                                     
037600*    -- ALSO NOTE THAT SUMMER/WINTER TIME IS ASSUMED TO                   
037700*    -- START AT MIDNIGHT, NOT 02:00 OR 03:00                             
038700     MOVE NEJ TO TZRUL-HIT-SW                                             
038800     PERFORM UNTIL TZRUL-IDTIDZON (INDX) = '99' OR                        
038900                        TZRUL-HIT-YES                                     
039000      IF WS-IDTIDZON = TZRUL-IDTIDZON (INDX)                              
039100       IF (TZRUL-IDDC (INDX) = SPACE OR                                   
039200           TZRUL-IDDC (INDX) = WS-IDDC)                                   
039300        IF TZ-TILOKDAT >= TZRUL-DAT-FOM (INDX)                            
039400                 AND TZ-TILOKDAT < TZRUL-DAT-TOM (INDX)                   
039500           ADD TZRUL-DIFF (INDX) TO WS-DIFF                               
039600           MOVE JA TO TZRUL-HIT-SW                                        
039700        END-IF                                                            
039800       END-IF                                                             
039900      END-IF                                                              
040000      ADD +1 TO INDX                                                      
040100     END-PERFORM                                                          
040200                                                                          
040300     ADD WS-DIFF TO WS-IDTIDZON                                           
040400                                                                          
040500     IF MSGI-KDCALL = '012'                                               
040600       COMPUTE TZ-DIFF = WS-IDTIDZON - 11                                 
040700     ELSE                                                                 
040800       COMPUTE TZ-DIFF = 11 - WS-IDTIDZON                                 
040900     END-IF                                                               
041000     .                                                                    
041100                                                                          
041200 FBB-USING-UTC SECTION.                                                   
041300                                                                          
041400*    -- IF WE ARE WORKING WITH UTC REQUESTS, ADJUST THE BASE DIFF         
041500*    -- FROM COMPUTER TIME (WHICH IS CET) TO UTC.                         
041600     SUBTRACT 1 FROM WS-DIFF                                              
041700                                                                          
041800     MOVE +1 TO INDX                                                      
041900                                                                          
042000*    -- CHANGE FOR DAYLIGHT SAVING TIME IN WAREHOUSE                      
042100*    -- NOTE: SUMMER/WINTER TIME CHANGES IN THE MORNING,                  
042200*    -- THEREFOR THE TEST IS >= AND <                                     
042300*    -- ALSO NOTE THAT SUMMER/WINTER TIME IS ASSUMED TO                   
042400*    -- START AT MIDNIGHT, NOT 02:00 OR 03:00                             
043400                                                                          
043500     MOVE NEJ TO TZRUL-HIT-SW                                             
043600     PERFORM UNTIL TZRUL-IDTIDZON (INDX) = '99' OR                        
043700                        TZRUL-HIT-YES                                     
043800      IF WS-IDTIDZON = TZRUL-IDTIDZON (INDX)                              
043900       IF (TZRUL-IDDC (INDX) = SPACE OR                                   
044000           TZRUL-IDDC (INDX) = WS-IDDC)                                   
044100        IF TZ-TILOKDAT >= TZRUL-DAT-FOM (INDX)                            
044200                 AND TZ-TILOKDAT < TZRUL-DAT-TOM (INDX)                   
044300           ADD TZRUL-DIFF (INDX) TO WS-DIFF                               
044400           MOVE JA TO TZRUL-HIT-SW                                        
044500        END-IF                                                            
044600       END-IF                                                             
044700      END-IF                                                              
044800      ADD +1 TO INDX                                                      
044900     END-PERFORM                                                          
045000     ADD WS-DIFF TO WS-IDTIDZON                                           
045100                                                                          
045200     IF MSGI-KDCALL = '014'                                               
045300       COMPUTE TZ-DIFF = WS-IDTIDZON - 11                                 
045400     ELSE                                                                 
045500       COMPUTE TZ-DIFF = 11 - WS-IDTIDZON                                 
045600     END-IF                                                               
045700     .                                                                    
