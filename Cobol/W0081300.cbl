000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0081300.                                                
000400 AUTHOR.         STEFANO GIOBBI.                                          
000500 DATE-WRITTEN.   MARS. 90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAM FÖR ON-LINEHANTERING AV NYUPPLÄGG, ÄNDRING,              
001200*        BORTTAG SAMT FRÅGOR.                                             
001300*                                                                         
001400*        PROGRAMMET GER EN FÖRKLARANDE TEXT TILL EN ANGIVEN               
001500*        ORDERBEKRÄFTELSEKOD. TEXTEN KAN VARA PÅ OLIKA SPRÅK,             
001600*        VILKA BESTÄMS AV IDSKYLT.                                        
001700*        PROGRAMMET KAN LÄGGA UPP NYA OCH TA BORT GAMLA ORDER-            
001800*        BEKRÄFTELSEKODER, SAMT ÄNDRA BEFINTLIGA TEXTER SOM ÄR            
001900*        KOPPLADE TILL VISS ORDERBEKRÄFTELSEKOD.                          
002000*                                                                         
002100*        DET FINNS ÄVEN ETT ENGELSKT FORMAT.                              
002200*        ENGELSKANS NO (NEJ) STAVAS NOO FÖR NO ÄR ETT RESER-              
002300*        VERAT COBOLORD.                                                  
002400*                                                                         
002500*        BLÄDDRING FÖREKOMMER EJ.                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W0T813                                              
002900*        MID:         W0I81301                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W0O81301                                            
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100*                                                                         
004200 77  IDPGM                   PIC X(8)    VALUE 'W0081300'.                
004300 77  JA                      PIC X       VALUE 'J'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004500 77  YES                     PIC X       VALUE 'Y'.                       
004600 77  NOO                     PIC X       VALUE 'N'.                       
004700 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
004800 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +837 COMP SYNC.            
004900 77  KDORDBEK-WS             PIC X(2)    VALUE SPACE.                     
005000 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
005010 77  HOPP                    PIC X(1)    VALUE 'N'.                       
005100                                                                          
005200 77  INDATA-SW               PIC X       VALUE 'J'.                       
005300   88  INDATA-OK                         VALUE 'J'.                       
005400   88  INDATA-FEL                        VALUE 'N'.                       
005500                                                                          
005600 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
005700   88  NYCKLAR-OK                        VALUE 'J'.                       
005800   88  NYCKLAR-FEL                       VALUE 'N'.                       
005900                                                                          
006000 77  ALLT-SW                 PIC X       VALUE 'J'.                       
006100   88  ALLT-OK                           VALUE 'J'.                       
006200                                                                          
006300 77  NY-NYCKEL-SW            PIC X       VALUE 'J'.                       
006400   88  NY-NYCKEL                         VALUE 'J'.                       
006500     EJECT                                                                
006600 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006700   88  EGEN-MID                          VALUE '0813'.                    
006800   88  GODK-MID                          VALUE '0813'.                    
006810   88  OE-SVARSBILD                      VALUE '4213' '4223'              
006820                                               '4233' '4243'.             
006830   88  TILL-4213                         VALUE '4213'.                    
006840   88  TILL-4223                         VALUE '4223'.                    
006850   88  TILL-4233                         VALUE '4233'.                    
006860   88  TILL-4243                         VALUE '4243'.                    
006900     SKIP2                                                                
007000 01  GENERELLA-SUBPROGRAM.                                                
007100*                                                                         
007200   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
007300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007400   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007401     EJECT                                                                
007600 01  KONSTANT-AREOR.                                                      
007700*                                                                         
007800   03  K-OBEK-KOD-FINNS-EJ-025                                            
007900                             PIC X(3)    VALUE '025'.                     
008000   03  K-NYCKEL-FEL-401      PIC X(3)    VALUE '401'.                     
008100   03  K-UPPDAT-UTF-404      PIC X(3)    VALUE '404'.                     
008200   03  K-PF11-FOER-UPPDAT-407                                             
008300                             PIC X(3)    VALUE '407'.                     
008400   03  K-UPPL-FAELT-FEL-409  PIC X(3)    VALUE '409'.                     
008500   03  K-UPPDAT-EJ-UTF-414   PIC X(3)    VALUE '414'.                     
008600*                                                                         
008700   03  K-SVENSKA-S           PIC X(3)    VALUE 'S  '.                     
008800   03  K-ENGELSKA-GB         PIC X(3)    VALUE 'GB '.                     
008900   03  K-ENGELSKA-USA        PIC X(3)    VALUE 'USA'.                     
009000   03  K-SPANSKA-E           PIC X(3)    VALUE 'E  '.                     
009100   03  K-FRANSKA-F           PIC X(3)    VALUE 'F  '.                     
009200   03  K-ITALIENSKA-I        PIC X(3)    VALUE 'I  '.                     
009300   03  K-PORTUGISISKA-P      PIC X(3)    VALUE 'P  '.                     
009400   03  K-FLAMLAENDSKA-NL     PIC X(3)    VALUE 'NL '.                     
009500   03  K-FINSKA-SF           PIC X(3)    VALUE 'SF '.                     
009600   03  K-TYSKA-D             PIC X(3)    VALUE 'D  '.                     
009700*                                                                         
009800   03  K-BASEN-SLUT-GB       PIC X(2)    VALUE 'GB'.                      
009900     SKIP2                                                                
010000 01  HELP-AREOR.                                                          
010100*                                                                         
010200   03  HELP-TEORDBEK         PIC X(70)   VALUE SPACE.                     
010300   03  HELP-ATTRIBUT         PIC X(2)    VALUE SPACE.                     
010400     EJECT                                                                
010500*   -COPY WMEDAREA                                                        
010600     EJECT                                                                
010700******************************************************************        
010800*                                                                *        
010900*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
011000*                                                                *        
011100******************************************************************        
011200 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
011300     SKIP3                                                                
011400*01  MID -COPY W0I81301                                                   
011500     EJECT                                                                
011600*01  -COPY WMSGAREA                                                       
011700     EJECT                                                                
011800*  03  MOD -COPY W0O81301 -RED MSG-AREA.                                  
011900     EJECT                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200******************************************************************        
012300*                                                                *        
012400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                       *        
012500*                                                                *        
012600******************************************************************        
012700*                                                                         
012800 01  IMS-WS.                                                              
012900   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
013000     SKIP3                                                                
013100 01  NYCKLAR-TILL-DLI.                                                    
013200*                                                                         
013300   03  W-WDGX01KEY-X.                                                     
013400     05  W-IDHTYP            PIC  X(4)   VALUE '4521'.                    
013500     05  FILLER              PIC  X(26)  VALUE LOW-VALUE.                 
013600*                                                                         
013700   03  W-WDGX11KEY-X.                                                     
013800     05  W-KDORDBEK          PIC  9(2)   VALUE ZERO.                      
013900     05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                     
014000*                                                                         
014100   03  W-WDGX11KEY-MIN-X.                                                 
014200     05  W-KDORDBEK-MIN      PIC  9(2)   VALUE ZERO.                      
014300     05  FILLER              PIC  X(3)   VALUE LOW-VALUE.                 
014400*                                                                         
014500   03  W-WDGX11KEY-MAX-X.                                                 
014600     05  W-KDORDBEK-MAX      PIC  9(2)   VALUE ZERO.                      
014700     05  FILLER              PIC  X(3)   VALUE HIGH-VALUE.                
014800     EJECT                                                                
014900******************************************************************        
015000*                                                                *        
015100*        STATUSKODER FRÅN IMS                                    *        
015200*                                                                *        
015300******************************************************************        
015400*                                                                         
015500   03  STATUS-WS             PIC X(2).                                    
015600*                                                                         
015700     88  SEGMENT-FINNS                   VALUE '  '.                      
015800     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
015900     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
016000     SKIP3                                                                
016100   03  GODK-STATUSKODER.                                                  
016200     05  GODK-STATUS OCCURS  5                                            
016300                     INDEXED BY STATUS-IX PIC X(2).                       
016400     SKIP3                                                                
016500******************************************************************        
016600*                                                                *        
016700*        SSA:ER                                                  *        
016800*                                                                *        
016900******************************************************************        
017000*                                                                         
017100 01    SSA1                  PIC X(128).                                  
017200 01    SSA2                  PIC X(128).                                  
017300     EJECT                                                                
017400******************************************************************        
017500*                                                                *        
017600*        IMS FUNKTIONSKODER                                      *        
017700*                                                                *        
017800******************************************************************        
017900*                                                                         
018000*01    -COPY W0003                                                        
018100     EJECT                                                                
018200******************************************************************        
018300*                                                                *        
018400*        DLI INPUT-OUTPUT AREA                                   *        
018500*                                                                *        
018600******************************************************************        
018700*                                                                         
018800 01  DLI-IO-AREA.                                                         
018900   03  IO-AREA               PIC X(1000) VALUE SPACE.                     
019000     SKIP3                                                                
019100*  03  WLXXKJ01  -COPY WDGX01              -RED IO-AREA.                  
019200     EJECT                                                                
019300*  03  WLXXKJ11  -COPY WDGX4522            -RED IO-AREA.                  
019400     EJECT                                                                
019500*                                                                         
019593 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW 4213'.           
019594     SKIP3                                                                
019595 01  4213-MSG-IO-AREA.                                                    
019596     03  4213-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
019597     03  4213-Z1               PIC X.                                     
019598     03  4213-Z2               PIC X.                                     
019599     03  4213-TRANSKOD         PIC X(8)   VALUE 'W4T213  '.               
019600     03  4213-IDTRANS          PIC X(4)   VALUE '0813'.                   
019601     03  4213-SPRAK            PIC X.                                     
019602*    03 -COPY W4I21301  -PRE 4213-                                        
019603*                                                                         
019604 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW 4223'.           
019605     SKIP3                                                                
019606 01  4223-MSG-IO-AREA.                                                    
019607     03  4223-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
019608     03  4223-Z1               PIC X.                                     
019609     03  4223-Z2               PIC X.                                     
019610     03  4223-TRANSKOD         PIC X(8)   VALUE 'W4T223  '.               
019611     03  4223-IDTRANS          PIC X(4)   VALUE '0813'.                   
019612     03  4223-SPRAK            PIC X.                                     
019613*    03 -COPY W4I22301  -PRE 4223-                                        
019614*                                                                         
019615 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW 4233'.           
019616     SKIP3                                                                
019617 01  4233-MSG-IO-AREA.                                                    
019618     03  4233-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
019619     03  4233-Z1               PIC X.                                     
019620     03  4233-Z2               PIC X.                                     
019621     03  4233-TRANSKOD         PIC X(8)   VALUE 'W4T233  '.               
019622     03  4233-IDTRANS          PIC X(4)   VALUE '0813'.                   
019623     03  4233-SPRAK            PIC X.                                     
019624*    03 -COPY W4I23301  -PRE 4233-                                        
019625*                                                                         
019626 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW 4243'.           
019627     SKIP3                                                                
019628 01  4243-MSG-IO-AREA.                                                    
019629     03  4243-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
019630     03  4243-Z1               PIC X.                                     
019631     03  4243-Z2               PIC X.                                     
019632     03  4243-TRANSKOD         PIC X(8)   VALUE 'W4T243  '.               
019633     03  4243-IDTRANS          PIC X(4)   VALUE '0813'.                   
019634     03  4243-SPRAK            PIC X.                                     
019635*    03 -COPY W4I24301  -PRE 4243-                                        
019640 LINKAGE SECTION.                                                         
019700*                                                                         
019800     SKIP2                                                                
019900*01  -COPY W0009     -PRE MSG-                                            
020000     EJECT                                                                
020030*01  -COPY W0009     -PRE 4213-                                           
020040     EJECT                                                                
020050*01  -COPY W0009     -PRE 4223-                                           
020051     EJECT                                                                
020052*01  -COPY W0009     -PRE 4233-                                           
020060     EJECT                                                                
020070*01  -COPY W0009     -PRE 4243-                                           
020080     EJECT                                                                
020100*01  -COPY W0008     -PRE XXKJ-                                           
020200     05  FILLER              PIC X(35).                                   
020300     EJECT                                                                
020400 PROCEDURE DIVISION USING MSG-PCB 4213-PCB 4223-PCB 4233-PCB              
020500                          4243-PCB XXKJ-PCB.                              
020600     ENTRY 'DLITCBL' USING MSG-PCB 4213-PCB 4223-PCB 4233-PCB             
020700                           4243-PCB XXKJ-PCB.                             
020800                                                                          
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100        PERFORM A-INIT                                                    
021110        IF MFS-IDPFK = '3'                                                
021120           PERFORM D-AVSLUTA                                              
021130        ELSE                                                              
021200                                                                          
021300         PERFORM B-KOLLA-NYCKLAR                                          
021400         IF NYCKLAR-OK                                                    
021520            IF MFS-UPDATE                                                 
021600              PERFORM G-KOLLA-INPUT                                       
021700              IF INDATA-OK                                                
021800                PERFORM H-UPPDATERA                                       
021900              END-IF                                                      
022000            ELSE                                                          
022100              IF MFS-ENTER OR                                             
022200                 MFS-FIRST OR                                             
022300                 MFS-NEXT                                                 
022400                PERFORM C-ENTER-TRYCKT                                    
022500              END-IF                                                      
022600            END-IF                                                        
022610         END-IF                                                           
022700       END-IF                                                             
022710       IF HOPP = NEJ                                                      
022810          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
022900          PERFORM IMS-INSERT-MSG                                          
022910       END-IF                                                             
023000     END-IF                                                               
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700                                                                          
023800     IF MSG-DUBBLA-TRANSKODER                                             
023900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I81301                 
024000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
024100                                               W-IDTRANS                  
024200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
024300     ELSE                                                                 
024400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I81301                 
024500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
024600                                               W-IDTRANS                  
024700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024800     END-IF                                                               
024900                                                                          
025000     IF EGEN-MID                                                          
025100       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
025200       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
025300     ELSE                                                                 
025400       MOVE SPACE                         TO MFS-KDTRTYP                  
025500       MOVE '7'                           TO MFS-IDPFK                    
025600     END-IF                                                               
025700                                                                          
025800     MOVE LOW-VALUE                       TO MSG-AREA                     
025900     MOVE 'W0O81301'                      TO MFS-IDMOD                    
026000     MOVE '0'                             TO MOD-IDTRANS1                 
026001     MOVE '8'                             TO MOD-IDTRANS2                 
026002     MOVE '1'                             TO MOD-IDTRANS3                 
026003     MOVE '3'                             TO MOD-IDTRANS4                 
026100     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
026200                                             MOD-TEMFSINF                 
026210     IF OE-SVARSBILD                                                      
026211*    VI KOMMER DIREKT FRÅN EN SVARSBILD                                   
026220        MOVE W-IDTRANS                    TO MOD-IDSYSTEM                 
026221        MOVE MFS-CLOSE-FIELD              TO MOD-IDTRANS1-ATTR            
026222        MOVE MFS-CLOSE-FIELD              TO MOD-IDTRANS2-ATTR            
026223        MOVE MFS-CLOSE-FIELD              TO MOD-IDTRANS3-ATTR            
026224        MOVE MFS-CLOSE-FIELD              TO MOD-IDTRANS4-ATTR            
026230     ELSE                                                                 
026231        IF MID-IDSYSTEM = SPACE OR ALL '+'                                
026232*    VI HAR KOMMMIT DIREKT TILL BILDEN                                    
026233           MOVE SPACE                     TO MOD-IDSYSTEM                 
026234           MOVE MFS-FORMATETS-ATTR        TO MOD-IDTRANS1-ATTR            
026235           MOVE MFS-FORMATETS-ATTR        TO MOD-IDTRANS2-ATTR            
026236           MOVE MFS-FORMATETS-ATTR        TO MOD-IDTRANS3-ATTR            
026237           MOVE MFS-FORMATETS-ATTR        TO MOD-IDTRANS4-ATTR            
026245        ELSE                                                              
026246*    VI HAR KOMMMIT URSPRUNGLIGEN KOMMIT FRÅN EN SVARSBILD                
026253           MOVE MID-IDSYSTEM              TO MOD-IDSYSTEM                 
026254           MOVE MFS-CLOSE-FIELD           TO MOD-IDTRANS1-ATTR            
026255           MOVE MFS-CLOSE-FIELD           TO MOD-IDTRANS2-ATTR            
026256           MOVE MFS-CLOSE-FIELD           TO MOD-IDTRANS3-ATTR            
026257           MOVE MFS-CLOSE-FIELD           TO MOD-IDTRANS4-ATTR            
026259        END-IF                                                            
026260     END-IF                                                               
026300                                                                          
026400     IF ENGLISH-TEXT                                                      
026500       MOVE +2                            TO SPRAK-IX                     
026600       MOVE 'GB '                         TO MED-IDSKYLT                  
026700     ELSE                                                                 
026800       MOVE +1                            TO SPRAK-IX                     
026900       MOVE 'S  '                         TO MED-IDSKYLT                  
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 B-KOLLA-NYCKLAR SECTION.                                                 
027400                                                                          
027500     MOVE      JA               TO        NYCKLAR-SW                      
027600     MOVE      MFS-RENSA-FAELT  TO        MOD-KDORDBEK-IN                 
027700                                                                          
027800     IF MID-KDORDBEK-IN = ALL '+'                                         
027900       MOVE    NEJ              TO        NY-NYCKEL-SW                    
028000       MOVE    MID-KDORDBEK-UT  TO        KDORDBEK-WS                     
028100       INSPECT KDORDBEK-WS      REPLACING LEADING SPACE BY ZERO           
028200     ELSE                                                                 
028300       MOVE    JA               TO        NY-NYCKEL-SW                    
028400       MOVE    MID-KDORDBEK-IN  TO        KDORDBEK-WS                     
028500       MOVE    '7'              TO        MFS-IDPFK                       
028600       MOVE    SPACE            TO        MFS-KDTRTYP                     
028700     END-IF                                                               
028800                                                                          
028900     MOVE      KDORDBEK-WS      TO        W-KDORDBEK                      
029000                                          MOD-KDORDBEK-UT                 
029100                                                                          
029200     IF KDORDBEK-WS NUMERIC AND KDORDBEK-WS > ZERO                        
029300       MOVE    KDORDBEK-WS      TO        W-KDORDBEK                      
029400       INSPECT MOD-KDORDBEK-UT  REPLACING LEADING ZERO BY SPACE           
029500     ELSE                                                                 
029600       MOVE NEJ                 TO        NYCKLAR-SW                      
029700     END-IF                                                               
029800                                                                          
029900     IF NOT GODK-MID                                                      
030000       MOVE NEJ                 TO        NYCKLAR-SW                      
030100       MOVE MFS-RENSA-FAELT     TO        MOD-KDORDBEK-UT                 
030200     ELSE                                                                 
030300       IF NYCKLAR-FEL                                                     
030400         MOVE    K-NYCKEL-FEL-401 TO       MED-IDMFSFEL                   
030500         CALL    WMEDKONV         USING    MED-WMEDAREA                   
030600         MOVE    MED-MFSFEL       TO       MOD-TEMFSFEL                   
030700         PERFORM MFS-RENSA-FAELT-UT                                       
030800       END-IF                                                             
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 C-ENTER-TRYCKT SECTION.                                                  
031300                                                                          
031400     IF NY-NYCKEL                                                         
031500       PERFORM S04-LAS-IN-SEGM-FR-ORDBEK-REG                              
031600     ELSE                                                                 
031700       IF MID-RADER   = ALL '+' AND                                       
031800          MID-FLJANEJ = ALL '+'                                           
031900         PERFORM S04-LAS-IN-SEGM-FR-ORDBEK-REG                            
032000       ELSE                                                               
032100         PERFORM CA-TRYCK-PF11-FOER-UPPDAT                                
032200       END-IF                                                             
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 CA-TRYCK-PF11-FOER-UPPDAT SECTION.                                       
032700                                                                          
032800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
032900                                                                          
033000     IF MID-FLJANEJ      NOT = ALL '+'                                    
033100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLJANEJ-ATTR                     
033200     END-IF                                                               
033300                                                                          
033400     IF MID-TEORDBEK-S   NOT = ALL '+'                                    
033500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-S-ATTR                  
033600     END-IF                                                               
033700                                                                          
033800     IF MID-TEORDBEK-GB  NOT = ALL '+'                                    
033900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-GB-ATTR                 
034000     END-IF                                                               
034100                                                                          
034200     IF MID-TEORDBEK-USA NOT = ALL '+'                                    
034300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-USA-ATTR                
034400     END-IF                                                               
034500                                                                          
034600     IF MID-TEORDBEK-E   NOT = ALL '+'                                    
034700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-E-ATTR                  
034800     END-IF                                                               
034900                                                                          
035000     IF MID-TEORDBEK-F   NOT = ALL '+'                                    
035100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-F-ATTR                  
035200     END-IF                                                               
035300                                                                          
035400     IF MID-TEORDBEK-I   NOT = ALL '+'                                    
035500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-I-ATTR                  
035600     END-IF                                                               
035700                                                                          
035800     IF MID-TEORDBEK-P   NOT = ALL '+'                                    
035900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-P-ATTR                  
036000     END-IF                                                               
036100                                                                          
036200     IF MID-TEORDBEK-NL  NOT = ALL '+'                                    
036300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-NL-ATTR                 
036400     END-IF                                                               
036500                                                                          
036600     IF MID-TEORDBEK-SF  NOT = ALL '+'                                    
036700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-SF-ATTR                 
036800     END-IF                                                               
036900                                                                          
037000     IF MID-TEORDBEK-D   NOT = ALL '+'                                    
037100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEORDBEK-D-ATTR                  
037200     END-IF                                                               
037300                                                                          
037400     MOVE    K-PF11-FOER-UPPDAT-407 TO    MED-IDMFSFEL                    
037500     CALL    WMEDKONV               USING MED-WMEDAREA                    
037600     MOVE    MED-MFSFEL             TO    MOD-TEMFSFEL                    
037700     .                                                                    
037800     EJECT                                                                
037900 D-AVSLUTA     SECTION.                                                   
038000                                                                          
038018     IF MID-IDSYSTEM = '4213'                                             
038019        COMPUTE 4213-LL = LENGTH OF 4213-MID-W4I21301 + 17                
038020        MOVE MFS-KDMFSFOR        TO 4213-SPRAK                            
038021                                                                          
038030        MOVE ALL '+'             TO 4213-MID-W4I21301                     
038031                                                                          
038032        PERFORM IMS-INSERT-4213-MSG                                       
038033        MOVE JA TO HOPP                                                   
038034     ELSE                                                                 
038035        IF MID-IDSYSTEM = '4223'                                          
038036           COMPUTE 4223-LL = LENGTH OF 4223-MID-W4I22301 + 17             
038037           MOVE MFS-KDMFSFOR       TO 4223-SPRAK                          
038038                                                                          
038039           MOVE ALL '+'            TO 4223-MID-W4I22301                   
038043                                                                          
038044           PERFORM IMS-INSERT-4223-MSG                                    
038045           MOVE JA TO HOPP                                                
038046        ELSE                                                              
038047           IF MID-IDSYSTEM = '4233'                                       
038048              COMPUTE 4233-LL = LENGTH OF 4233-MID-W4I23301 + 17          
038049              MOVE MFS-KDMFSFOR       TO 4233-SPRAK                       
038050                                                                          
038051              MOVE ALL '+'            TO 4233-MID-W4I23301                
038052                                                                          
038053              PERFORM IMS-INSERT-4233-MSG                                 
038054              MOVE JA TO HOPP                                             
038055           ELSE                                                           
038056              IF MID-IDSYSTEM = '4243'                                    
038057                COMPUTE 4243-LL = LENGTH OF 4243-MID-W4I24301 + 17        
038058                MOVE MFS-KDMFSFOR       TO 4243-SPRAK                     
038059                                                                          
038060                MOVE ALL '+'            TO 4243-MID-W4I24301              
038061                                                                          
038062                PERFORM IMS-INSERT-4243-MSG                               
038063                MOVE JA TO HOPP                                           
038064              ELSE                                                        
038065                 MOVE 'W0O50401'           TO MFS-IDMOD                   
038066                 MOVE SPACE                TO MSG-AREA                    
038067              END-IF                                                      
038068           END-IF                                                         
038069        END-IF                                                            
038070     END-IF                                                               
038071     .                                                                    
038072     EJECT                                                                
038073 G-KOLLA-INPUT SECTION.                                                   
038080                                                                          
038100     MOVE JA                   TO INDATA-SW                               
038200     MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-ATTR                        
038300                                                                          
038400     IF MID-FLJANEJ = '+'                                                 
038500       CONTINUE                                                           
038600     ELSE                                                                 
038700                                                                          
038800       IF SWEDISH-TEXT                                                    
038900         IF MID-FLJANEJ = JA  OR                                          
039000                          NEJ                                             
039100           CONTINUE                                                       
039200         ELSE                                                             
039300           MOVE NEJ TO INDATA-SW                                          
039400         END-IF                                                           
039500       ELSE                                                               
039600                                                                          
039700         IF ENGLISH-TEXT                                                  
039800           IF MID-FLJANEJ = YES OR                                        
039900                            NOO                                           
040000             CONTINUE                                                     
040100           ELSE                                                           
040200             MOVE NEJ TO INDATA-SW                                        
040300           END-IF                                                         
040400                                                                          
040500         END-IF                                                           
040600       END-IF                                                             
040700     END-IF                                                               
040800                                                                          
040900     IF INDATA-FEL                                                        
041000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
041100       MOVE    MFS-ALFA-FAELT-FEL   TO    MOD-FLJANEJ-ATTR                
041200       MOVE    K-UPPL-FAELT-FEL-409 TO    MED-IDMFSFEL                    
041300       CALL    WMEDKONV             USING MED-WMEDAREA                    
041400       MOVE    MED-MFSFEL           TO    MOD-TEMFSFEL                    
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 H-UPPDATERA SECTION.                                                     
041900                                                                          
042000     IF MID-FLJANEJ = JA  OR                                              
042100                      YES                                                 
042200       PERFORM HA-DELETE-OBEK-KOD                                         
042300                                                                          
042400     ELSE                                                                 
042500       IF MID-RADER = ALL '+'                                             
042600         PERFORM HB-INGEN-INPUT                                           
042700       ELSE                                                               
042800         PERFORM HC-REPL-ISRT-DLET-TEORDBEK                               
042900       END-IF                                                             
043000                                                                          
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 HA-DELETE-OBEK-KOD SECTION.                                              
043500                                                                          
043600     PERFORM IMS-GU-XXKJ01                                                
043700     MOVE    W-KDORDBEK TO W-KDORDBEK-MIN                                 
043800                           W-KDORDBEK-MAX                                 
043900     PERFORM IMS-GHNP-XXKJ11                                              
044000                                                                          
044100     IF SEGMENT-SAKNAS                                                    
044200       PERFORM S03-OBEK-KOD-FINNS-EJ                                      
044300     ELSE                                                                 
044400       PERFORM UNTIL SEGMENT-SAKNAS                                       
044500                                                                          
044600         PERFORM IMS-DLET-XXKJ11                                          
044700         PERFORM IMS-GHNP-XXKJ11                                          
044800                                                                          
044900       END-PERFORM                                                        
045000                                                                          
045100       PERFORM MFS-RENSA-FAELT-UT                                         
045200       PERFORM S02-UPPDAT-OK                                              
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 HB-INGEN-INPUT SECTION.                                                  
045700                                                                          
045800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
045900     MOVE    MFS-RENSA-FAELT     TO    MOD-FLJANEJ                        
046000     MOVE    K-UPPDAT-EJ-UTF-414 TO    MED-IDMFSFEL                       
046100     CALL    WMEDKONV            USING MED-WMEDAREA                       
046200     MOVE    MED-MFSFEL          TO    MOD-TEMFSFEL                       
046300     .                                                                    
046400     EJECT                                                                
046500 HC-REPL-ISRT-DLET-TEORDBEK SECTION.                                      
046600                                                                          
046700     PERFORM MFS-ROER-EJ-FAELT-UT                                         
046800     MOVE    MFS-RENSA-FAELT      TO MOD-FLJANEJ                          
046900     MOVE    W-KDORDBEK           TO 4522-KDORDBEK                        
047000                                                                          
047100     IF MID-TEORDBEK-S   NOT = ALL '+'                                    
047200       MOVE    K-SVENSKA-S        TO 4522-IDSKYLT                         
047300                                        W-IDSKYLT                         
047400       MOVE    MID-TEORDBEK-S     TO HELP-TEORDBEK                        
047500       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
047600       PERFORM HCA-UPPDAT-OBEK-REG                                        
047700       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-S-ATTR                  
047800     END-IF                                                               
047900                                                                          
048000     IF MID-TEORDBEK-GB  NOT = ALL '+'                                    
048100       MOVE    K-ENGELSKA-GB      TO 4522-IDSKYLT                         
048200                                        W-IDSKYLT                         
048300       MOVE    MID-TEORDBEK-GB    TO HELP-TEORDBEK                        
048400       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
048500       PERFORM HCA-UPPDAT-OBEK-REG                                        
048600       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-GB-ATTR                 
048700     END-IF                                                               
048800                                                                          
048900     IF MID-TEORDBEK-USA NOT = ALL '+'                                    
049000       MOVE    K-ENGELSKA-USA     TO 4522-IDSKYLT                         
049100                                        W-IDSKYLT                         
049200       MOVE    MID-TEORDBEK-USA   TO HELP-TEORDBEK                        
049300       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
049400       PERFORM HCA-UPPDAT-OBEK-REG                                        
049500       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-USA-ATTR                
049600     END-IF                                                               
049700                                                                          
049800     IF MID-TEORDBEK-E   NOT = ALL '+'                                    
049900       MOVE    K-SPANSKA-E        TO 4522-IDSKYLT                         
050000                                        W-IDSKYLT                         
050100       MOVE    MID-TEORDBEK-E     TO HELP-TEORDBEK                        
050200       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
050300       PERFORM HCA-UPPDAT-OBEK-REG                                        
050400       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-E-ATTR                  
050500     END-IF                                                               
050600                                                                          
050700     IF MID-TEORDBEK-F   NOT = ALL '+'                                    
050800       MOVE    K-FRANSKA-F        TO 4522-IDSKYLT                         
050900                                        W-IDSKYLT                         
051000       MOVE    MID-TEORDBEK-F     TO HELP-TEORDBEK                        
051100       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
051200       PERFORM HCA-UPPDAT-OBEK-REG                                        
051300       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-F-ATTR                  
051400     END-IF                                                               
051500                                                                          
051600     IF MID-TEORDBEK-I   NOT = ALL '+'                                    
051700       MOVE    K-ITALIENSKA-I     TO 4522-IDSKYLT                         
051800                                        W-IDSKYLT                         
051900       MOVE    MID-TEORDBEK-I     TO HELP-TEORDBEK                        
052000       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
052100       PERFORM HCA-UPPDAT-OBEK-REG                                        
052200       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-I-ATTR                  
052300     END-IF                                                               
052400                                                                          
052500     IF MID-TEORDBEK-P   NOT = ALL '+'                                    
052600       MOVE    K-PORTUGISISKA-P   TO 4522-IDSKYLT                         
052700                                        W-IDSKYLT                         
052800       MOVE    MID-TEORDBEK-P     TO HELP-TEORDBEK                        
052900       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
053000       PERFORM HCA-UPPDAT-OBEK-REG                                        
053100       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-P-ATTR                  
053200     END-IF                                                               
053300                                                                          
053400     IF MID-TEORDBEK-NL  NOT = ALL '+'                                    
053500       MOVE    K-FLAMLAENDSKA-NL  TO 4522-IDSKYLT                         
053600                                        W-IDSKYLT                         
053700       MOVE    MID-TEORDBEK-NL    TO HELP-TEORDBEK                        
053800       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
053900       PERFORM HCA-UPPDAT-OBEK-REG                                        
054000       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-NL-ATTR                 
054100     END-IF                                                               
054200                                                                          
054300     IF MID-TEORDBEK-SF  NOT = ALL '+'                                    
054400       MOVE    K-FINSKA-SF        TO 4522-IDSKYLT                         
054500                                        W-IDSKYLT                         
054600       MOVE    MID-TEORDBEK-SF    TO HELP-TEORDBEK                        
054700       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
054800       PERFORM HCA-UPPDAT-OBEK-REG                                        
054900       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-SF-ATTR                 
055000     END-IF                                                               
055100                                                                          
055200     IF MID-TEORDBEK-D   NOT = ALL '+'                                    
055300       MOVE    K-TYSKA-D          TO 4522-IDSKYLT                         
055400                                        W-IDSKYLT                         
055500       MOVE    MID-TEORDBEK-D     TO HELP-TEORDBEK                        
055600       MOVE    MFS-FORMATETS-ATTR TO HELP-ATTRIBUT                        
055700       PERFORM HCA-UPPDAT-OBEK-REG                                        
055800       MOVE    HELP-ATTRIBUT      TO MOD-TEORDBEK-D-ATTR                  
055900     END-IF                                                               
056000                                                                          
056100     PERFORM S02-UPPDAT-OK                                                
056200     .                                                                    
056300     EJECT                                                                
056400 HCA-UPPDAT-OBEK-REG SECTION.                                             
056500                                                                          
056600     PERFORM IMS-GHU-XXKJ11                                               
056700                                                                          
056800     IF SEGMENT-FINNS                                                     
056900       IF HELP-TEORDBEK = SPACE                                           
057000         PERFORM IMS-DLET-XXKJ11                                          
057100       ELSE                                                               
057200         MOVE    HELP-TEORDBEK         TO 4522-TEORDBEK                   
057300         PERFORM IMS-REPL-XXKJ11                                          
057400         MOVE    MFS-ADD-LYS-UPP-FAELT TO HELP-ATTRIBUT                   
057500       END-IF                                                             
057600     ELSE                                                                 
057700       IF HELP-TEORDBEK NOT = SPACE                                       
057800         MOVE    HELP-TEORDBEK         TO 4522-TEORDBEK                   
057900         PERFORM IMS-ISRT-XXKJ11                                          
058000         MOVE    MFS-ADD-LYS-UPP-FAELT TO HELP-ATTRIBUT                   
058100       END-IF                                                             
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 S02-UPPDAT-OK SECTION.                                                   
058600                                                                          
058700     MOVE K-UPPDAT-UTF-404 TO      MED-IDMFSINF                           
058800     CALL WMEDKONV           USING MED-WMEDAREA                           
058900     MOVE MED-MFSINF         TO    MOD-TEMFSINF                           
059000     .                                                                    
059100     EJECT                                                                
059200 S03-OBEK-KOD-FINNS-EJ SECTION.                                           
059300                                                                          
059400     MOVE    K-OBEK-KOD-FINNS-EJ-025 TO    MED-IDMFSFEL                   
059500     CALL    WMEDKONV                USING MED-WMEDAREA                   
059600     MOVE    MED-MFSFEL              TO    MOD-TEMFSFEL                   
059700     PERFORM MFS-RENSA-FAELT-UT                                           
059800     .                                                                    
059900     EJECT                                                                
060000 S04-LAS-IN-SEGM-FR-ORDBEK-REG SECTION.                                   
060100                                                                          
060200     PERFORM MFS-RENSA-FAELT-UT                                           
060300                                                                          
060400     PERFORM IMS-GU-XXKJ01                                                
060500                                                                          
060600     MOVE    W-KDORDBEK TO W-KDORDBEK-MIN                                 
060700                           W-KDORDBEK-MAX                                 
060800     PERFORM IMS-GNP-XXKJ11                                               
060900     IF SEGMENT-SAKNAS                                                    
061000       PERFORM S03-OBEK-KOD-FINNS-EJ                                      
061100     ELSE                                                                 
061200       PERFORM UNTIL SEGMENT-SAKNAS                                       
061300         EVALUATE 4522-IDSKYLT                                            
061400                                                                          
061500           WHEN K-SVENSKA-S                                               
061600             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-S                         
061700                                                                          
061800           WHEN K-ENGELSKA-GB                                             
061900             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-GB                        
062000                                                                          
062100           WHEN K-ENGELSKA-USA                                            
062200             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-USA                       
062300                                                                          
062400           WHEN K-SPANSKA-E                                               
062500             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-E                         
062600                                                                          
062700           WHEN K-FRANSKA-F                                               
062800             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-F                         
062900                                                                          
063000           WHEN K-ITALIENSKA-I                                            
063100             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-I                         
063200                                                                          
063300           WHEN K-PORTUGISISKA-P                                          
063400             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-P                         
063500                                                                          
063600           WHEN K-FLAMLAENDSKA-NL                                         
063700             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-NL                        
063800                                                                          
063900           WHEN K-FINSKA-SF                                               
064000             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-SF                        
064100                                                                          
064200           WHEN K-TYSKA-D                                                 
064300             MOVE 4522-TEORDBEK TO MOD-TEORDBEK-D                         
064400         END-EVALUATE                                                     
064500                                                                          
064600         PERFORM IMS-GNP-XXKJ11                                           
064700       END-PERFORM                                                        
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 MFS-RENSA-FAELT-UT SECTION.                                              
065200                                                                          
065300     MOVE MFS-RENSA-FAELT TO                                              
065400                             MOD-FLJANEJ                                  
065500                             MOD-TEORDBEK-S                               
065600                             MOD-TEORDBEK-GB                              
065700                             MOD-TEORDBEK-USA                             
065800                             MOD-TEORDBEK-E                               
065900                             MOD-TEORDBEK-F                               
066000                             MOD-TEORDBEK-I                               
066100                             MOD-TEORDBEK-P                               
066200                             MOD-TEORDBEK-NL                              
066300                             MOD-TEORDBEK-SF                              
066400                             MOD-TEORDBEK-D                               
066500     .                                                                    
066600     SKIP2                                                                
066700 MFS-ROER-EJ-FAELT-UT SECTION.                                            
066800                                                                          
066900     MOVE MFS-ROER-EJ-FAELT TO                                            
067000                             MOD-FLJANEJ                                  
067100                             MOD-TEORDBEK-S                               
067200                             MOD-TEORDBEK-GB                              
067300                             MOD-TEORDBEK-USA                             
067400                             MOD-TEORDBEK-E                               
067500                             MOD-TEORDBEK-F                               
067600                             MOD-TEORDBEK-I                               
067700                             MOD-TEORDBEK-P                               
067800                             MOD-TEORDBEK-NL                              
067900                             MOD-TEORDBEK-SF                              
068000                             MOD-TEORDBEK-D                               
068100     .                                                                    
068200     EJECT                                                                
068300*    IMS SEKTIONER IMS SEKTIONER IMS SEKTIONER IMS SEKTIONER     *        
068400     SKIP3                                                                
068491 IMS-INSERT-4213-MSG SECTION.                                             
068492                                                                          
068493     MOVE LOW-VALUE TO 4213-Z1 4213-Z2                                    
068494     MOVE SPACE TO GODK-STATUSKODER                                       
068495     CALL CBLTDLI USING ISRT 4213-PCB 4213-MSG-IO-AREA                    
068496     MOVE 4213-STATUS-CODE TO STATUS-WS                                   
068497     PERFORM IMS-STATUSKONTROLL                                           
068498     .                                                                    
068499     EJECT                                                                
068500 IMS-INSERT-4223-MSG SECTION.                                             
068501                                                                          
068502     MOVE LOW-VALUE TO 4223-Z1 4223-Z2                                    
068503     MOVE SPACE TO GODK-STATUSKODER                                       
068504     CALL CBLTDLI USING ISRT 4223-PCB 4223-MSG-IO-AREA                    
068505     MOVE 4223-STATUS-CODE TO STATUS-WS                                   
068506     PERFORM IMS-STATUSKONTROLL                                           
068507     .                                                                    
068508     EJECT                                                                
068509 IMS-INSERT-4233-MSG SECTION.                                             
068510                                                                          
068511     MOVE LOW-VALUE TO 4233-Z1 4233-Z2                                    
068512     MOVE SPACE TO GODK-STATUSKODER                                       
068513     CALL CBLTDLI USING ISRT 4233-PCB 4233-MSG-IO-AREA                    
068514     MOVE 4233-STATUS-CODE TO STATUS-WS                                   
068515     PERFORM IMS-STATUSKONTROLL                                           
068516     .                                                                    
068517     EJECT                                                                
068518 IMS-INSERT-4243-MSG SECTION.                                             
068519                                                                          
068520     MOVE LOW-VALUE TO 4243-Z1 4243-Z2                                    
068521     MOVE SPACE TO GODK-STATUSKODER                                       
068522     CALL CBLTDLI USING ISRT 4243-PCB 4243-MSG-IO-AREA                    
068523     MOVE 4243-STATUS-CODE TO STATUS-WS                                   
068524     PERFORM IMS-STATUSKONTROLL                                           
068525     .                                                                    
068526     EJECT                                                                
068530 IMS-GU-XXKJ01 SECTION.                                                   
068600                                                                          
068700     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
068800            DELIMITED BY SIZE INTO SSA1                                   
068900     MOVE    '  '               TO GODK-STATUSKODER                       
069000     CALL    CBLTDLI USING GU XXKJ-PCB DLI-IO-AREA SSA1                   
069100     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400     SKIP3                                                                
069500 IMS-GNP-XXKJ11 SECTION.                                                  
069600                                                                          
069700     STRING  'WLXXKJ11(WDGXKEY >=' W-WDGX11KEY-MIN-X                      
069800                     '&WDGXKEY <=' W-WDGX11KEY-MAX-X ')'                  
069900            DELIMITED BY SIZE INTO SSA1                                   
070000     MOVE    '  GE'             TO GODK-STATUSKODER                       
070100     CALL    CBLTDLI USING GNP XXKJ-PCB DLI-IO-AREA SSA1                  
070200     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-GHU-XXKJ11 SECTION.                                                  
070700                                                                          
070800     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
070900            DELIMITED BY SIZE INTO SSA1                                   
071000     STRING  'WLXXKJ11(WDGXKEY  =' W-WDGX11KEY-X ')'                      
071100            DELIMITED BY SIZE INTO SSA2                                   
071200     MOVE    '  GE'             TO GODK-STATUSKODER                       
071300     CALL    CBLTDLI USING GHU XXKJ-PCB DLI-IO-AREA SSA1 SSA2             
071400     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700                                                                          
071800 IMS-GHNP-XXKJ11 SECTION.                                                 
071900                                                                          
072000     STRING  'WLXXKJ11(WDGXKEY >=' W-WDGX11KEY-MIN-X                      
072100                     '&WDGXKEY <=' W-WDGX11KEY-MAX-X ')'                  
072200            DELIMITED BY SIZE INTO SSA1                                   
072300     MOVE    '  GE'             TO GODK-STATUSKODER                       
072400     CALL    CBLTDLI USING GHNP XXKJ-PCB DLI-IO-AREA SSA1                 
072500     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     EJECT                                                                
072900 IMS-ISRT-XXKJ11 SECTION.                                                 
073000                                                                          
073100     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
073200            DELIMITED BY SIZE INTO SSA1                                   
073300     MOVE    'WLXXKJ11 '        TO SSA2                                   
073400     MOVE    '  II' TO GODK-STATUSKODER                                   
073500     CALL    CBLTDLI USING ISRT XXKJ-PCB DLI-IO-AREA SSA1 SSA2            
073600     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900                                                                          
074000 IMS-REPL-XXKJ11 SECTION.                                                 
074100                                                                          
074200     MOVE    '  '               TO GODK-STATUSKODER                       
074300     CALL    CBLTDLI USING REPL XXKJ-PCB DLI-IO-AREA                      
074400     MOVE    XXKJ-STATUS-CODE  TO STATUS-WS                               
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700                                                                          
074800 IMS-DLET-XXKJ11 SECTION.                                                 
074900                                                                          
075000     MOVE    '  '               TO GODK-STATUSKODER                       
075100     CALL    CBLTDLI USING DLET XXKJ-PCB DLI-IO-AREA                      
075200     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
075300     PERFORM IMS-STATUSKONTROLL                                           
075400     .                                                                    
075500     EJECT                                                                
075600 IMS-GET-MSG SECTION.                                                     
075700                                                                          
075800     MOVE    '  QC'          TO GODK-STATUSKODER                          
075900     CALL    CBLTDLI USING GU MSG-PCB MSG-IO-AREA                         
076000     MOVE    MSG-STATUS-CODE TO STATUS-WS                                 
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     .                                                                    
076300                                                                          
076400 IMS-INSERT-MSG SECTION.                                                  
076500                                                                          
076600     IF ENGLISH-TEXT                                                      
076700       MOVE 'N' TO MFS-KDHUVOMR                                           
076800     END-IF                                                               
076900     MOVE    LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                         
077000     MOVE    SPACE           TO GODK-STATUSKODER                          
077100     CALL    CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD             
077200     MOVE    MSG-STATUS-CODE TO STATUS-WS                                 
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     EJECT                                                                
077600 IMS-STATUSKONTROLL SECTION.                                              
077700                                                                          
077800     SET    STATUS-IX TO +1                                               
077900     SEARCH GODK-STATUS                                                   
078000       AT END                                                             
078100         CALL FELLOG                                                      
078200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078300         CONTINUE                                                         
078400     END-SEARCH                                                           
078500     .                                                                    
