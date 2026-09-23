000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4011500.                                                
000400 AUTHOR.         LENA LINDBLOM.                                           
000500 DATE-WRITTEN.   MARS  90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING,NYUPPLÄGGNING,BORTTAG OCH FRÅGEPROGRAM               
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T115                                              
001400*        MID:         W4I11501                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O11501                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002301                                                                          
002310*    -- CHECKED BY WY2000                                                 
002400 77  IDPGM                   PIC X(8)    VALUE 'W4011500'.                
002500 77  JA                      PIC X       VALUE 'J'.                       
002600 77  NEJ                     PIC X       VALUE 'N'.                       
002800 77  BENAMN-IX               PIC S9(9)   VALUE +0   COMP SYNC.            
002900 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
003000 77  MAX-RADER               PIC S9(9)   VALUE +10  COMP SYNC.            
003100 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +916 COMP SYNC.            
003200 77  IDKVAFEL-WS             PIC X(2)    VALUE SPACE.                     
003300 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
003400                                                                          
003500 77  INDATA-SW               PIC X       VALUE 'J'.                       
003600   88  INDATA-OK                         VALUE 'J'.                       
003700   88  INDATA-FEL                        VALUE 'N'.                       
003800                                                                          
003900 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
004000   88  NYCKLAR-OK                        VALUE 'J'.                       
004100   88  NYCKLAR-FEL                       VALUE 'N'.                       
004200                                                                          
004300 77  FOERAENDRING-SW         PIC X       VALUE 'A'.                       
004400   88  AENDRING                          VALUE 'A'.                       
004500   88  NYUPPLAEGG                        VALUE 'N'.                       
004600   88  BORTTAG                           VALUE 'B'.                       
004700                                                                          
004800 77  ALLT-SW                 PIC X       VALUE 'J'.                       
004900   88  ALLT-OK                           VALUE 'J'.                       
005000                                                                          
005100 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
005200   88  EGEN-MID                          VALUE '4115'.                    
005300   88  GODK-MID                          VALUE '4116'.                    
005400     EJECT                                                                
005500 01  GENERELLA-SUBPROGRAM.                                                
005600   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
005700   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
005800   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005900   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006000     EJECT                                                                
006100*   -COPY WDECAREA                                                        
006300     EJECT                                                                
006400*   -COPY WMEDAREA                                                        
006600     EJECT                                                                
006700******************************************************************        
006800*                                                                         
006900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
007000*                                                                         
007100 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
007200     SKIP3                                                                
007300*01  MID -COPY W4I11501                                                   
007500     EJECT                                                                
007600*01  -COPY WMSGAREA                                                       
007800     EJECT                                                                
007900*  03  MOD -COPY W4O11501 -RED MSG-AREA.                                  
008100     EJECT                                                                
008200*01  -COPY WMFSAREA                                                       
008400     EJECT                                                                
008500******************************************************************        
008600*                                                                         
008700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900 01  IMS-WS.                                                              
009000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
009100     SKIP3                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300   03  W-IDSKYLT-X.                                                       
009400     05  W-IDSKYLT           PIC  X(2)   VALUE SPACE.                     
009500   03  W-WDGX-4821-KEY-X.                                                 
009600     05  W-IDHTYP-4821       PIC  X(04)  VALUE '4821'.                    
009700     05  FILLER              PIC  X(26)  VALUE LOW-VALUE.                 
009800   03  W-WDGX-4822-KEY-X.                                                 
009900     05  W-IDKVAFEL          PIC  X(02)  VALUE SPACE.                     
010000     05  FILLER              PIC  X(03)  VALUE LOW-VALUE.                 
010100     SKIP3                                                                
010200*                        **** STATUS-KOD FRÅN IMS                         
010300   03  STATUS-WS             PIC XX.                                      
010400     88  SEGMENT-FINNS                   VALUE '  '.                      
010500     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
010600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
010700     SKIP3                                                                
010800   03  GODK-STATUSKODER.                                                  
010900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01    SSA1                  PIC X(64).                                   
011200 01    SSA2                  PIC X(64).                                   
011300     EJECT                                                                
011400*                            IMS FUNKTIONSKODER                           
011500*01    -COPY W0003                                                        
011700     EJECT                                                                
011800*                            DLI INPUT-OUTPUT AREA                        
011900 01  DLI-IO-AREA.                                                         
012000   03  IO-AREA               PIC X(400)  VALUE SPACE.                     
012100     SKIP3                                                                
012200*  03  WLXXJY01  -COPY WDGX01  -PRE XXJY-  -RED IO-AREA.                  
012400     EJECT                                                                
012500*  03  WLXXJY11  -COPY WDGX4822  -PRE XXJY-  -RED IO-AREA.                
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900*01  -COPY W0009     -PRE MSG-                                            
013100     EJECT                                                                
013200*01  -COPY W0008     -PRE XXJY-                                           
013400     05  FILLER              PIC X(50).                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION USING MSG-PCB XXJY-PCB.                               
013700     ENTRY 'DLITCBL' USING MSG-PCB XXJY-PCB.                              
013800                                                                          
013900     PERFORM IMS-GET-MSG                                                  
014000     IF SEGMENT-FINNS                                                     
014100       PERFORM A-INIT                                                     
014200       PERFORM B-KOLLA-NYCKLAR                                            
014300       IF NYCKLAR-OK                                                      
014400         IF MFS-UPDATE                                                    
014500           PERFORM G-KOLLA-INPUT                                          
014600           IF INDATA-OK                                                   
014700             PERFORM H-UPPDATERA                                          
014800           END-IF                                                         
014900         ELSE                                                             
015000           IF MFS-FIRST                                                   
015100             PERFORM C-FOERSTA-SIDA                                       
015200           ELSE                                                           
015300             IF MFS-NEXT                                                  
015400               PERFORM D-NAESTA-SIDA                                      
015500             ELSE                                                         
015600               PERFORM E-SAMMA-SIDA                                       
015700             END-IF                                                       
015800           END-IF                                                         
015900           IF ALLT-OK                                                     
016000             PERFORM S01-LAES-VISA-FELKOD                                 
016100           END-IF                                                         
016200         END-IF                                                           
016300       END-IF                                                             
016400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016500       PERFORM IMS-INSERT-MSG                                             
016600     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     IF MSG-DUBBLA-TRANSKODER                                             
017500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I11501                 
017600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017800     ELSE                                                                 
017900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I11501                  
018000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018200     END-IF                                                               
018300                                                                          
018400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
018500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
018600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018700                                                                          
018800     MOVE LOW-VALUE TO MSG-AREA                                           
018900     MOVE 'W4O115N1' TO MFS-IDMOD                                         
019000     MOVE '4115' TO MOD-IDTRANS                                           
019100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019200                                                                          
019300     IF NOT EGEN-MID                                                      
019400       MOVE SPACE TO MFS-KDTRTYP                                          
019500       MOVE '7' TO MFS-IDPFK                                              
019600     END-IF                                                               
019700                                                                          
019800     IF ENGLISH-TEXT                                                      
020000       MOVE 'GB ' TO MED-IDSKYLT                                          
020100     ELSE                                                                 
020310       MOVE 'S  ' TO MED-IDSKYLT                                          
020400     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 B-KOLLA-NYCKLAR SECTION.                                                 
020800                                                                          
020900     MOVE JA TO NYCKLAR-SW                                                
021000     MOVE MFS-RENSA-FAELT TO MOD-IDKVAFELI                                
021100                             MOD-IDSKYLTI                                 
021200     IF NOT EGEN-MID                                                      
021300       MOVE ZERO TO MID-IDKVAFELI                                         
021400       MOVE SPACE TO MID-IDSKYLTI                                         
021500     END-IF                                                               
021600     IF ((MID-IDKVAFELI NOT = ALL '+') OR                                 
021700        (MID-IDSKYLTI NOT = ALL '+')) OR                                  
021800        (MFS-IDPFK = '8' AND MID-IDKVAFEL-NX = ZERO)                      
021900       MOVE '7'      TO MFS-IDPFK                                         
022000       MOVE SPACE    TO MFS-KDTRTYP                                       
022100     END-IF                                                               
022200                                                                          
022300     IF MID-IDKVAFELI = ALL '+'                                           
022400       MOVE MID-IDKVAFELU TO IDKVAFEL-WS                                  
022500       INSPECT IDKVAFEL-WS REPLACING LEADING SPACE BY ZERO                
022600     ELSE                                                                 
022700       MOVE MID-IDKVAFELI   TO IDKVAFEL-WS                                
022800     END-IF                                                               
022900                                                                          
023000     IF MID-IDSKYLTI = ALL '+'                                            
023100       IF MID-IDSKYLTU = SPACE                                            
023200         IF ENGLISH-TEXT                                                  
023300           MOVE 'NL ' TO IDSKYLT-WS                                       
023400           MOVE +2    TO BENAMN-IX                                        
023500         ELSE                                                             
023600           MOVE 'S  ' TO IDSKYLT-WS                                       
023700           MOVE +1    TO BENAMN-IX                                        
023800         END-IF                                                           
023900       ELSE                                                               
024000         MOVE MID-IDSKYLTU TO IDSKYLT-WS                                  
024100         IF IDSKYLT-WS = 'B  '                                            
024200           MOVE +2 TO BENAMN-IX                                           
024300         ELSE                                                             
024400            IF IDSKYLT-WS = 'GB '                                         
024500              MOVE +3 TO BENAMN-IX                                        
024600            ELSE                                                          
024700              MOVE +1 TO BENAMN-IX                                        
024800            END-IF                                                        
024900         END-IF                                                           
025000       END-IF                                                             
025100     ELSE                                                                 
025200       IF MID-IDSKYLTI NOT = 'S  ' AND 'NL ' AND 'GB ' AND 'B'            
025210                        AND  'F  ' AND 'E  ' AND 'I  '                    
025300         MOVE '401' TO MED-IDMFSFEL                                       
025400         MOVE NEJ TO NYCKLAR-SW                                           
025500       ELSE                                                               
025600         IF MID-IDSKYLTI = 'NL' OR 'B'                                    
025700           MOVE +2 TO BENAMN-IX                                           
025800         ELSE                                                             
025900           IF MID-IDSKYLTI = 'GB ' OR 'F  ' OR 'E  ' OR 'I  '             
026000              MOVE +3 TO BENAMN-IX                                        
026100           ELSE                                                           
026200              MOVE +1 TO BENAMN-IX                                        
026300           END-IF                                                         
026400         END-IF                                                           
026500       END-IF                                                             
026600       MOVE MID-IDSKYLTI    TO IDSKYLT-WS                                 
026700     END-IF                                                               
026800                                                                          
026900     IF IDKVAFEL-WS NUMERIC AND IDKVAFEL-WS > ZERO                        
027000       MOVE IDKVAFEL-WS TO W-IDKVAFEL                                     
027100     ELSE                                                                 
027200       MOVE '401' TO MED-IDMFSFEL                                         
027300       MOVE NEJ TO NYCKLAR-SW                                             
027400     END-IF                                                               
027500                                                                          
027600                                                                          
027700     IF GODK-MID OR NYCKLAR-OK                                            
027800         MOVE IDKVAFEL-WS TO MOD-IDKVAFELU                                
027900         INSPECT MOD-IDKVAFELU REPLACING LEADING ZERO BY SPACE            
028000         MOVE IDSKYLT-WS TO MOD-IDSKYLTU                                  
028010         INSPECT MOD-IDSKYLTU REPLACING LEADING 'NL'  BY  'B '            
028020                                                'F '  BY  'GB'            
028030                                                'E '  BY  'GB'            
028040                                                'I '  BY  'GB'            
028100     ELSE                                                                 
028200       MOVE MFS-RENSA-FAELT TO MOD-IDKVAFELU                              
028300                               MOD-IDSKYLTU                               
028400     END-IF                                                               
028500                                                                          
028600     IF NYCKLAR-FEL                                                       
028700       CALL WMEDKONV USING MED-WMEDAREA                                   
028800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028900       PERFORM MFS-RENSA-FAELT-IN                                         
029000       PERFORM MFS-RENSA-FAELT-UT                                         
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 C-FOERSTA-SIDA SECTION.                                                  
029500                                                                          
029600     PERFORM MFS-RENSA-FAELT-IN                                           
029700     PERFORM MFS-RENSA-FAELT-UT                                           
029800     MOVE JA TO ALLT-SW                                                   
029900     .                                                                    
030000     EJECT                                                                
030100 D-NAESTA-SIDA SECTION.                                                   
030200                                                                          
030300     MOVE MID-IDKVAFEL-NX TO IDKVAFEL-WS                                  
030400     INSPECT IDKVAFEL-WS REPLACING LEADING SPACE BY ZERO                  
030500     MOVE IDKVAFEL-WS TO W-IDKVAFEL                                       
030600     MOVE MID-IDSKYLT-NX TO W-IDSKYLT                                     
030700     MOVE JA TO ALLT-SW                                                   
030800     .                                                                    
030900     EJECT                                                                
031000 E-SAMMA-SIDA SECTION.                                                    
031100                                                                          
031200     IF MID-INPUT = ALL '+'                                               
031300       MOVE MID-IDKVAFEL-EN TO IDKVAFEL-WS                                
031400       INSPECT IDKVAFEL-WS REPLACING LEADING SPACE BY ZERO                
031500       MOVE IDKVAFEL-WS TO W-IDKVAFEL                                     
031600       MOVE MID-IDSKYLT-EN TO W-IDSKYLT                                   
031700       MOVE JA TO ALLT-SW                                                 
031800     ELSE                                                                 
031900       MOVE NEJ TO ALLT-SW                                                
032000       MOVE '003' TO MED-IDMFSFEL                                         
032100       CALL WMEDKONV USING MED-WMEDAREA                                   
032200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032300       PERFORM MFS-ROR-EJ-FAELT-IN                                        
032400       PERFORM MFS-ROR-EJ-FAELT-UT                                        
032500       PERFORM MFS-LAS-IN-IGEN                                            
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 G-KOLLA-INPUT SECTION.                                                   
033000                                                                          
033100     MOVE JA  TO INDATA-SW                                                
033200     IF MID-INPUT = ALL '+'                                               
033300       MOVE '011' TO MED-IDMFSFEL                                         
033400       CALL WMEDKONV USING MED-WMEDAREA                                   
033500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033600       PERFORM MFS-ROR-EJ-FAELT-IN                                        
033700       PERFORM MFS-ROR-EJ-FAELT-UT                                        
033800       MOVE NEJ TO INDATA-SW                                              
033900     ELSE                                                                 
034000       PERFORM MFS-LAS-IN-IGEN                                            
034100     END-IF                                                               
034200     IF INDATA-OK                                                         
034300       IF MID-KDCMD-IN = ALL '+' OR SPACE                                 
034400         IF MID-IDSKYLT-IN = 'S  ' OR 'GB ' OR 'B'                        
034500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-IN-ATTR               
034600           IF MID-IDSKYLT-IN = 'S  '                                      
034700             MOVE +1 TO BENAMN-IX                                         
034800           ELSE                                                           
034900             IF MID-IDSKYLT-IN = 'B'                                      
035000                MOVE +2 TO BENAMN-IX                                      
035100             ELSE                                                         
035200                MOVE +3 TO BENAMN-IX                                      
035300             END-IF                                                       
035400           END-IF                                                         
035500         ELSE                                                             
035600           MOVE '001' TO MED-IDMFSFEL                                     
035700           MOVE NEJ TO INDATA-SW                                          
035800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-IN-ATTR                 
035900         END-IF                                                           
036000         IF MID-IDKVAFEL-IN = ALL '+'                                     
036100           MOVE '001' TO MED-IDMFSFEL                                     
036200           MOVE NEJ TO INDATA-SW                                          
036300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAFEL-IN-ATTR                 
036400         ELSE                                                             
036500           IF MID-IDKVAFEL-IN NUMERIC                                     
036600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKVAFEL-IN-ATTR             
036700             MOVE MID-IDKVAFEL-IN TO W-IDKVAFEL                           
036800             PERFORM IMS-GHU-WLXXJY11                                     
036900             IF SEGMENT-FINNS                                             
037000               PERFORM GA-AENDRING                                        
037100             ELSE                                                         
037200               PERFORM GB-NYUPPLAEGG                                      
037300             END-IF                                                       
037400           ELSE                                                           
037500             MOVE '001' TO MED-IDMFSFEL                                   
037600             MOVE NEJ TO INDATA-SW                                        
037700             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAFEL-IN-ATTR               
037800           END-IF                                                         
037900         END-IF                                                           
038000       ELSE                                                               
038100         IF MID-KDCMD-IN NOT = 'B' AND 'D'                                
038200           MOVE '001' TO MED-IDMFSFEL                                     
038300           MOVE NEJ TO INDATA-SW                                          
038400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                   
038500         ELSE                                                             
038600           PERFORM GC-BORTTAG                                             
038700         END-IF                                                           
038800       END-IF                                                             
038900     END-IF                                                               
039000*                                                                         
039100     IF INDATA-FEL                                                        
039200       CALL WMEDKONV USING MED-WMEDAREA                                   
039300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
039500       PERFORM MFS-ROR-EJ-FAELT-UT                                        
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 GA-AENDRING SECTION.                                                     
040000                                                                          
040100     MOVE 'A' TO FOERAENDRING-SW                                          
040200     IF XXJY-4822-IDSKYLT (BENAMN-IX) = SPACE                             
040300                               OR LOW-VALUE                               
040400       PERFORM GAA-AENDRING-TILLAEGG                                      
040500     ELSE                                                                 
040600       PERFORM GAB-AENDRING-BEFINTLIG                                     
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 GAA-AENDRING-TILLAEGG SECTION.                                           
041100                                                                          
041200     IF MID-BEKVAFEL-IN = ALL '+'                                         
041300       MOVE '001' TO MED-IDMFSFEL                                         
041400       MOVE NEJ TO INDATA-SW                                              
041500       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKVAFEL-IN-ATTR                    
041600     ELSE                                                                 
041700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKVAFEL-IN-ATTR                  
041800       IF MID-KDKVAFG-IN = ALL '+'                                        
041900          IF  MID-KDKVAFG-IN NUMERIC                                      
042000          AND MID-KDKVAFG-IN = 1 OR 2 OR 3 OR 4                           
042100            CONTINUE                                                      
042200          ELSE                                                            
042300            MOVE '001' TO MED-IDMFSFEL                                    
042400            MOVE NEJ TO INDATA-SW                                         
042500            MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVAFG-IN-ATTR                 
042600          END-IF                                                          
042700       ELSE                                                               
042800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKVAFG-IN-ATTR                 
042900         IF MID-BEKVAFGR-IN = ALL '+'                                     
043000           MOVE '001' TO MED-IDMFSFEL                                     
043100           MOVE NEJ TO INDATA-SW                                          
043200           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKVAFGR-IN-ATTR                
043300         ELSE                                                             
043400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKVAFGR-IN-ATTR              
043500                                        MOD-KVKVAFPO-IN-ATTR              
043600           IF MID-KVKVAFPO-IN NOT = ALL '+'                               
043700             IF MID-KVKVAFPO-IN NOT NUMERIC                               
043800               MOVE '001' TO MED-IDMFSFEL                                 
043900               MOVE NEJ TO INDATA-SW                                      
044000               MOVE MFS-NUM-FAELT-FEL TO MOD-KVKVAFPO-IN-ATTR             
044100             END-IF                                                       
044200           END-IF                                                         
044300         END-IF                                                           
044400       END-IF                                                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 GAB-AENDRING-BEFINTLIG SECTION.                                          
044900                                                                          
045000     IF  MID-KDKVAFG-IN  = ALL '+'                                        
045100     AND MID-BEKVAFEL-IN = ALL '+'                                        
045200     AND MID-BEKVAFGR-IN = ALL '+'                                        
045300     AND MID-KVKVAFPO-IN = ALL '+'                                        
045400        MOVE NEJ TO INDATA-SW                                             
045500        MOVE MFS-NUM-FAELT-FEL  TO MOD-KVKVAFPO-IN-ATTR                   
045600                                   MOD-KDKVAFG-IN-ATTR                    
045700        MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKVAFGR-IN-ATTR                   
045800                                   MOD-BEKVAFEL-IN-ATTR                   
045900     ELSE                                                                 
046000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKVAFGR-IN-ATTR                  
046100                                    MOD-BEKVAFEL-IN-ATTR                  
046200       IF MID-KVKVAFPO-IN NOT = ALL '+'                                   
046300         IF MID-KVKVAFPO-IN NUMERIC                                       
046400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKVAFPO-IN-ATTR               
046500         ELSE                                                             
046600           MOVE NEJ TO INDATA-SW                                          
046700           MOVE MFS-NUM-FAELT-FEL TO MOD-KVKVAFPO-IN-ATTR                 
046800         END-IF                                                           
046900       ELSE                                                               
047000         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKVAFPO-IN-ATTR                 
047100       END-IF                                                             
047200       IF MID-KDKVAFG-IN NOT = ALL '+'                                    
047300         IF  MID-KDKVAFG-IN NUMERIC                                       
047400         AND MID-KDKVAFG-IN = 1 OR 2 OR 3 OR 4                            
047500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVAFG-IN-ATTR                
047600         ELSE                                                             
047700           MOVE NEJ TO INDATA-SW                                          
047800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVAFG-IN-ATTR                  
047900         END-IF                                                           
048000       ELSE                                                               
048100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVAFG-IN-ATTR                  
048200       END-IF                                                             
048300     END-IF                                                               
048400     IF INDATA-FEL                                                        
048500        MOVE '001' TO MED-IDMFSFEL                                        
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 GB-NYUPPLAEGG SECTION.                                                   
049000                                                                          
049100     MOVE 'N' TO FOERAENDRING-SW                                          
049200     IF MID-BEKVAFEL-IN = ALL '+'                                         
049300       MOVE NEJ TO INDATA-SW                                              
049400       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKVAFEL-IN-ATTR                    
049500     ELSE                                                                 
049600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKVAFEL-IN-ATTR                  
049700     END-IF                                                               
049800     IF  MID-KDKVAFG-IN NUMERIC                                           
049900     AND MID-KDKVAFG-IN = 1 OR 2 OR 3 OR 4                                
050000       MOVE MFS-NUM-FAELT-RAETT TO MOD-KDKVAFG-IN-ATTR                    
050100     ELSE                                                                 
050200       MOVE NEJ TO INDATA-SW                                              
050300       MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVAFG-IN-ATTR                      
050400     END-IF                                                               
050500     IF MID-BEKVAFGR-IN = ALL '+'                                         
050600       MOVE '001' TO MED-IDMFSFEL                                         
050700       MOVE NEJ TO INDATA-SW                                              
050800       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEKVAFGR-IN-ATTR                    
050900     ELSE                                                                 
051000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKVAFGR-IN-ATTR                  
051100     END-IF                                                               
051200     IF MID-KVKVAFPO-IN = ALL '+'                                         
051300       MOVE '001' TO MED-IDMFSFEL                                         
051400       MOVE NEJ TO INDATA-SW                                              
051500       MOVE MFS-NUM-FAELT-FEL TO MOD-KVKVAFPO-IN-ATTR                     
051600     ELSE                                                                 
051700       MOVE MFS-NUM-FAELT-RAETT TO MOD-KVKVAFPO-IN-ATTR                   
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 GC-BORTTAG SECTION.                                                      
052200                                                                          
052300     MOVE 'B' TO FOERAENDRING-SW                                          
052400                                                                          
052500     IF MID-IDKVAFEL-IN = ALL '+'                                         
052600       MOVE '001' TO MED-IDMFSFEL                                         
052700       MOVE NEJ TO INDATA-SW                                              
052800       MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAFEL-IN-ATTR                     
052900     ELSE                                                                 
053000       IF MID-IDKVAFEL-IN NUMERIC                                         
053100         PERFORM IMS-GHU-WLXXJY11                                         
053200         IF SEGMENT-SAKNAS                                                
053300           MOVE '010' TO MED-IDMFSFEL                                     
053400           MOVE NEJ TO INDATA-SW                                          
053500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAFEL-IN-ATTR                 
053600         END-IF                                                           
053700       END-IF                                                             
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 H-UPPDATERA SECTION.                                                     
054200                                                                          
054300     IF NYUPPLAEGG                                                        
054400        PERFORM HA-LAGG-UPP-SEGMENT                                       
054500        PERFORM IMS-ISRT-WLXXJY11                                         
054600     ELSE                                                                 
054700        IF AENDRING                                                       
054800          PERFORM HB-UPPDAT-SEGMENT                                       
054900          PERFORM IMS-REPL-WLXXJY11                                       
055000        ELSE                                                              
055100          IF BORTTAG                                                      
055200            MOVE +1 TO BENAMN-IX                                          
055300            PERFORM IMS-DLET-WLXXJY11                                     
055400          END-IF                                                          
055500        END-IF                                                            
055600     END-IF                                                               
055700                                                                          
055800     MOVE '101' TO MED-IDMFSINF                                           
055900     CALL WMEDKONV USING MED-WMEDAREA                                     
056000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
056100     PERFORM MFS-FORM-ATTR                                                
056200                                                                          
056300     PERFORM S01-LAES-VISA-FELKOD                                         
056400     PERFORM MFS-RENSA-FAELT-IN                                           
056500     .                                                                    
056600     EJECT                                                                
056700 HA-LAGG-UPP-SEGMENT SECTION.                                             
056800                                                                          
056900     MOVE MID-IDKVAFEL-IN TO XXJY-4822-IDKVAFEL                           
057000     MOVE LOW-VALUE TO XXJY-4822-LOW-VALUE                                
057100     MOVE MID-KVKVAFPO-IN TO XXJY-4822-KVKVAFPO                           
057200     MOVE MID-KDKVAFG-IN TO XXJY-4822-KDKVAFG                             
057300     MOVE MID-IDSKYLT-IN TO XXJY-4822-IDSKYLT (BENAMN-IX)                 
057400     MOVE MID-BEKVAFEL-IN TO XXJY-4822-BEKVAFEL (BENAMN-IX)               
057500     MOVE MID-BEKVAFGR-IN TO XXJY-4822-BEKVAFGR (BENAMN-IX)               
057600     .                                                                    
057700     EJECT                                                                
057800 HB-UPPDAT-SEGMENT SECTION.                                               
057900                                                                          
058000     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
058100       MOVE MID-IDSKYLT-IN TO XXJY-4822-IDSKYLT (BENAMN-IX)               
058200     END-IF                                                               
058300     IF MID-KDKVAFG-IN NOT = ALL '+'                                      
058400       MOVE MID-KDKVAFG-IN TO XXJY-4822-KDKVAFG                           
058500       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
058600                              MOD-KDKVAFG-UT-ATTR (1)                     
058700     END-IF                                                               
058800     IF MID-BEKVAFEL-IN NOT = ALL '+'                                     
058900       MOVE MID-BEKVAFEL-IN TO XXJY-4822-BEKVAFEL (BENAMN-IX)             
059000       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
059100                              MOD-BEKVAFEL-UT-ATTR (1)                    
059200     END-IF                                                               
059300     IF MID-BEKVAFGR-IN NOT = ALL '+'                                     
059400       MOVE MID-BEKVAFGR-IN TO XXJY-4822-BEKVAFGR (BENAMN-IX)             
059500       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
059600                              MOD-BEKVAFGR-UT-ATTR (1)                    
059700     END-IF                                                               
059800     IF MID-KVKVAFPO-IN NOT = ALL '+'                                     
059900       MOVE MID-KVKVAFPO-IN TO XXJY-4822-KVKVAFPO                         
060000       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
060100                              MOD-KVKVAFPO-UT-ATTR (1)                    
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 S01-LAES-VISA-FELKOD SECTION.                                            
060600                                                                          
060700     PERFORM IMS-GU-WLXXJY11                                              
060800     MOVE +1 TO INDX                                                      
060900     IF SEGMENT-FINNS                                                     
061000       MOVE XXJY-4822-IDKVAFEL TO MOD-IDKVAFEL-EN                         
061100       MOVE IDSKYLT-WS         TO MOD-IDSKYLT-EN                          
061200     ELSE                                                                 
061300       PERFORM IMS-GN-WLXXJY11                                            
061400       IF SEGMENT-FINNS                                                   
061500         MOVE XXJY-4822-IDKVAFEL TO MOD-IDKVAFEL-EN                       
061600         MOVE IDSKYLT-WS         TO MOD-IDSKYLT-EN                        
061700       ELSE                                                               
061800         MOVE ZERO  TO MOD-IDKVAFEL-EN                                    
061900         MOVE SPACE TO MOD-IDSKYLT-EN                                     
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     PERFORM UNTIL INDX > MAX-RADER                                       
062400       IF SEGMENT-FINNS                                                   
062500         MOVE XXJY-4822-IDKVAFEL TO MOD-IDKVAFEL-UT (INDX)                
062600         MOVE XXJY-4822-KDKVAFG  TO MOD-KDKVAFG-UT  (INDX)                
062700         MOVE XXJY-4822-BEKVAFEL (BENAMN-IX) TO                           
062800                                    MOD-BEKVAFEL-UT (INDX)                
062900         MOVE XXJY-4822-BEKVAFGR (BENAMN-IX) TO                           
063000                                    MOD-BEKVAFGR-UT (INDX)                
063100         MOVE XXJY-4822-KVKVAFPO TO MOD-KVKVAFPO-UT (INDX)                
063200         PERFORM IMS-GN-WLXXJY11                                          
063300       ELSE                                                               
063400         MOVE MFS-RENSA-FAELT TO MOD-IDKVAFEL-UT (INDX)                   
063500                                 MOD-KDKVAFG-UT  (INDX)                   
063600                                 MOD-BEKVAFEL-UT (INDX)                   
063700                                 MOD-BEKVAFGR-UT (INDX)                   
063800                                 MOD-KVKVAFPO-UT (INDX)                   
063900       END-IF                                                             
064000       ADD +1 TO INDX                                                     
064100     END-PERFORM                                                          
064200                                                                          
064300     IF SEGMENT-FINNS                                                     
064400       MOVE XXJY-4822-IDKVAFEL TO MOD-IDKVAFEL-NX                         
064500       MOVE IDSKYLT-WS         TO MOD-IDSKYLT-NX                          
064600       MOVE '105' TO MED-IDMFSINF                                         
064700       CALL WMEDKONV USING MED-WMEDAREA                                   
064800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064900     ELSE                                                                 
065000       MOVE ZERO  TO MOD-IDKVAFEL-NX                                      
065100       MOVE SPACE TO MOD-IDSKYLT-NX                                       
065200     END-IF                                                               
065300     PERFORM MFS-RENSA-FAELT-IN                                           
065400     .                                                                    
065500     EJECT                                                                
065600                                                                          
065700 MFS-RENSA-FAELT-IN SECTION.                                              
065800                                                                          
065900     MOVE MFS-RENSA-FAELT   TO MOD-IDKVAFEL-IN                            
066000                               MOD-KDKVAFG-IN                             
066100                               MOD-BEKVAFEL-IN                            
066200                               MOD-BEKVAFGR-IN                            
066300                               MOD-KVKVAFPO-IN                            
066400                               MOD-IDSKYLT-IN                             
066500                               MOD-KDCMD-IN                               
066600     .                                                                    
066700     EJECT                                                                
066800 MFS-RENSA-FAELT-UT SECTION.                                              
066900                                                                          
067000     MOVE MFS-RENSA-FAELT TO MOD-IDKVAFEL-NX                              
067100                             MOD-IDSKYLT-NX                               
067200                             MOD-IDKVAFEL-EN                              
067300                             MOD-IDSKYLT-EN                               
067400     PERFORM MFS-RENSA-FAELT-UT-RAD                                       
067500     .                                                                    
067600     SKIP2                                                                
067700 MFS-RENSA-FAELT-UT-RAD SECTION.                                          
067800                                                                          
067900     MOVE +1 TO INDX                                                      
068000     PERFORM UNTIL INDX > MAX-RADER                                       
068100       MOVE MFS-RENSA-FAELT TO MOD-IDKVAFEL-UT (INDX)                     
068200                               MOD-KDKVAFG-UT  (INDX)                     
068300                               MOD-BEKVAFEL-UT (INDX)                     
068400                               MOD-BEKVAFGR-UT (INDX)                     
068500                               MOD-KVKVAFPO-UT (INDX)                     
068600       ADD +1 TO INDX                                                     
068700     END-PERFORM                                                          
068800     .                                                                    
068900     SKIP2                                                                
069000 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
069100     MOVE MFS-ROER-EJ-FAELT TO                                            
069200                               MOD-IDKVAFEL-NX                            
069300                               MOD-IDSKYLT-NX                             
069400                               MOD-IDKVAFEL-EN                            
069500                               MOD-IDSKYLT-EN                             
069600     MOVE +1 TO INDX                                                      
069700     PERFORM UNTIL INDX > MAX-RADER                                       
069800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAFEL-UT (INDX)                   
069900                                 MOD-KDKVAFG-UT  (INDX)                   
070000                                 MOD-BEKVAFEL-UT (INDX)                   
070100                                 MOD-BEKVAFGR-UT (INDX)                   
070200                                 MOD-KVKVAFPO-UT (INDX)                   
070300       ADD +1 TO INDX                                                     
070400     END-PERFORM                                                          
070500     .                                                                    
070600     SKIP2                                                                
070700 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
070800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAFEL-IN                            
070900                               MOD-KDKVAFG-IN                             
071000                               MOD-BEKVAFEL-IN                            
071100                               MOD-BEKVAFGR-IN                            
071200                               MOD-KVKVAFPO-IN                            
071300                               MOD-IDSKYLT-IN                             
071400                               MOD-KDCMD-IN                               
071500     .                                                                    
071600     EJECT                                                                
071700 MFS-FORM-ATTR SECTION.                                                   
071800                                                                          
071900     MOVE MFS-FORMATETS-ATTR TO MOD-IDKVAFEL-IN-ATTR                      
072000                                MOD-KDKVAFG-IN-ATTR                       
072100                                MOD-BEKVAFEL-IN-ATTR                      
072200                                MOD-BEKVAFGR-IN-ATTR                      
072300                                MOD-KVKVAFPO-IN-ATTR                      
072400                                MOD-IDSKYLT-IN-ATTR                       
072500                                MOD-KDCMD-IN-ATTR                         
072600     .                                                                    
072700     SKIP2                                                                
072800 MFS-LAS-IN-IGEN SECTION.                                                 
072900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKVAFEL-IN-ATTR                   
073000                                   MOD-KDKVAFG-IN-ATTR                    
073100                                   MOD-BEKVAFEL-IN-ATTR                   
073200                                   MOD-BEKVAFGR-IN-ATTR                   
073300                                   MOD-KVKVAFPO-IN-ATTR                   
073400                                   MOD-IDSKYLT-IN-ATTR                    
073500                                   MOD-KDCMD-IN-ATTR                      
073600     .                                                                    
073700     EJECT                                                                
073800* IMS SEKTIONER                                                           
073900     SKIP3                                                                
074000 IMS-GET-MSG SECTION.                                                     
074100                                                                          
074200     MOVE '  QC' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP3                                                                
074800 IMS-INSERT-MSG SECTION.                                                  
074900                                                                          
075000     IF NOT ENGLISH-TEXT                                                  
075100       MOVE '0' TO MFS-KDHUVOMR                                           
075200     END-IF                                                               
075300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075400     MOVE SPACE TO GODK-STATUSKODER                                       
075500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     EJECT                                                                
076000 IMS-GHU-WLXXJY11 SECTION.                                                
076100                                                                          
076200     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
076300          DELIMITED BY SIZE INTO SSA1                                     
076400     STRING 'WLXXJY11(WDGXKEY  =' W-WDGX-4822-KEY-X ')'                   
076500          DELIMITED BY SIZE INTO SSA2                                     
076600     MOVE '  GE' TO GODK-STATUSKODER                                      
076700     CALL CBLTDLI USING GHU XXJY-PCB DLI-IO-AREA SSA1 SSA2                
076800     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
076900     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
077100                                                                          
077200 IMS-GN-WLXXJY11 SECTION.                                                 
077300                                                                          
077400     STRING 'WLXXJY11(WDGXKEY =>' W-WDGX-4822-KEY-X ')'                   
077500          DELIMITED BY SIZE INTO SSA1                                     
077600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
077700     CALL CBLTDLI USING GN XXJY-PCB DLI-IO-AREA SSA1                      
077800     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100                                                                          
078200 IMS-GU-WLXXJY11 SECTION.                                                 
078300                                                                          
078400     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
078500          DELIMITED BY SIZE INTO SSA1                                     
078600     STRING 'WLXXJY11(WDGXKEY  =' W-WDGX-4822-KEY-X ')'                   
078700          DELIMITED BY SIZE INTO SSA2                                     
078800     MOVE '  GE' TO GODK-STATUSKODER                                      
078900     CALL CBLTDLI USING GU XXJY-PCB DLI-IO-AREA SSA1 SSA2                 
079000     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
079100     PERFORM IMS-STATUSKONTROLL                                           
079200     .                                                                    
079300     EJECT                                                                
079400 IMS-ISRT-WLXXJY11 SECTION.                                               
079500                                                                          
079600     STRING 'WLXXJY01(WDGXKEY  =' W-WDGX-4821-KEY-X ')'                   
079700          DELIMITED BY SIZE INTO SSA1                                     
079800     MOVE 'WLXXJY11 ' TO SSA2                                             
079900     MOVE '  II' TO GODK-STATUSKODER                                      
080000     CALL CBLTDLI USING ISRT XXJY-PCB DLI-IO-AREA SSA1 SSA2               
080100     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     .                                                                    
080400                                                                          
080500 IMS-REPL-WLXXJY11 SECTION.                                               
080600                                                                          
080700     MOVE '  ' TO GODK-STATUSKODER                                        
080800     CALL CBLTDLI USING REPL XXJY-PCB DLI-IO-AREA                         
080900     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUSKONTROLL                                           
081100     .                                                                    
081200                                                                          
081300 IMS-DLET-WLXXJY11 SECTION.                                               
081400                                                                          
081500     MOVE '  ' TO GODK-STATUSKODER                                        
081600     CALL CBLTDLI USING DLET XXJY-PCB DLI-IO-AREA                         
081700     MOVE XXJY-STATUS-CODE TO STATUS-WS                                   
081800     PERFORM IMS-STATUSKONTROLL                                           
081900     .                                                                    
082000     EJECT                                                                
082100 IMS-STATUSKONTROLL SECTION.                                              
082200                                                                          
082300     SET STATUS-IX TO 1                                                   
082400     SEARCH GODK-STATUS                                                   
082500       AT END CALL FELLOG                                                 
082600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
082700     END-SEARCH                                                           
082800     .                                                                    
