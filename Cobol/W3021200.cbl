000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3021200.                                                
000400 AUTHOR.         S. K.                                                    
000500     DATE-WRITTEN.  SEPT   89.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        ARTIKELSTATESTIK                                                 
001100*                                                                         
001200*        SÖKNING OCH UPPDATERING AV                                       
001300*        DELBUDGET                                                        
001400*                                                                         
001500*        COBOL II                                                         
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W3T212                                              
002000*        MID:         W3I21201                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W3O21201                                            
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900*                                                                         
003000****************************************************************          
003100*         WORKING-STORAGE SECTION                                         
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200****************************************************************          
003300*                                                                         
003400 WORKING-STORAGE SECTION.                                                 
003500*                                                                         
003600 01  FILLER                  PIC X(16)        VALUE '77-OR'.              
003700*                                                                         
003800 77  PROGRAM-NAMN            PIC X(8)         VALUE 'W3021300'.           
003900 77  JA                      PIC X            VALUE 'J'.                  
004000 77  NEJ                     PIC X            VALUE 'N'.                  
004100 77  RAD-IX                  PIC S9(3)        VALUE +1.                   
004200 77  MAX-RAD-IX              PIC S9(3)        VALUE 21.                   
004300 77  WS-3-COMP-UPPACKNING    PIC S9(2)        VALUE ZERO.                 
004400 77  WS-5-COMP-UPPACKNING    PIC S9(4)        VALUE ZERO.                 
004500 77  WS-13-COMP-UPPACKNING   PIC S9(14)       VALUE ZERO.                 
004600 77  MAX-MOD-LAENGD          PIC S9(4)        VALUE +657 COMP-3.          
004700 77  WS-IDTRANS              PIC X(04).                                   
004800     88 EGEN-BILD                             VALUE '3212'.               
004900   EJECT                                                                  
005000*                                                                         
005100******************************************************************        
005200*          SUBPROGRAM                                                     
005300******************************************************************        
005400*                                                                         
005500 01  FILLER                  PIC X(16)   VALUE 'SUBPGM'.                  
005600*                                                                         
005700 01  SUBPGM.                                                              
005800   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
005900   03  WKPSKONV              PIC X(8)    VALUE 'WKPSKONV'.                
006000   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006100   03  FELLOG                PIC X(8)    VALUE 'FELLOG '.                 
006200   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006300   EJECT                                                                  
006400*                                                                         
006500******************************************************************        
006600*          NYCKLAR TILL DLI                                               
006700******************************************************************        
006800*                                                                         
006900 01    FILLER                  PIC X(16) VALUE 'DLI-NYCKLAR'.             
007000*                                                                         
007100 01    NYCKLAR-TILL-DLI.                                                  
007200*                                DELBUDGET ROT                            
007300   03    W-3135-X.                                                        
007400     05    W-3135              PIC X(4)  VALUE '3135'.                    
007500     05    FILLER              PIC X(26) VALUE LOW-VALUE.                 
007600     SKIP3                                                                
007700*                                DELBUDGET SEGMENT                        
007800   03    W-DELBUDGET-X.                                                   
007900     05    W-KDMARK-BUDG       PIC S9(3) VALUE ZERO  COMP-3.              
008000     05    W-KDPRODSL          PIC S9(3) VALUE ZERO  COMP-3.              
008100     05    W-IDFKNGRP          PIC S9(5) VALUE ZERO  COMP-3.              
008200     SKIP3                                                                
008300                                                                          
008400*                                IDSKURVA ROT                             
008500   03    W-3133-X.                                                        
008600     05    W-3133              PIC X(4)  VALUE '3133'.                    
008700     05    FILLER              PIC X(26) VALUE LOW-VALUE.                 
008800     SKIP3                                                                
008900*                                IDSKURVA SEGMENT                         
009000   03    W-IDSKURVA-X.                                                    
009100     05    W-IDSKURVA          PIC S9(3) VALUE ZERO  COMP-3.              
009200     05    FILLER              PIC X(13) VALUE LOW-VALUE.                 
009300    EJECT                                                                 
009400                                                                          
009500******************************************************************        
009600*            SWITCHAR                                                     
009700******************************************************************        
009800*                                                                         
009900 01   FILLER                   PIC X(16)   VALUE 'SWITCHAR'.              
010000*                                                                         
010100 01   SWITCHAR.                                                           
010200    03 FELFLAGGA               PIC X(01)   VALUE 'N'.                     
010300    03 ISRT-FLAGGA             PIC X(01)   VALUE 'N'.                     
010400    03 REPL-FLAGGA             PIC X(01)   VALUE 'N'.                     
010500    03 DLET-FLAGGA             PIC X(01)   VALUE 'N'.                     
010600    03 FEL-1-FLAGGA            PIC X(01)   VALUE 'N'.                     
010700    03 MED-4-FLAGGA            PIC X(01)   VALUE 'N'.                     
010800    03 MED-5-FLAGGA            PIC X(01)   VALUE 'N'.                     
010900    03 MED-6-FLAGGA            PIC X(01)   VALUE 'N'.                     
011000                                                                          
011100   EJECT                                                                  
011200*                                                                         
011300******************************************************************        
011400*            ARBETSFÄLTT                                                  
011500******************************************************************        
011600*                                                                         
011700 01   FILLER                 PIC X(16)   VALUE 'ARBETSFÄLT'.              
011800                                                                          
011900 01  ARBETSFAELT.                                                         
012000                                                                          
012100    03 WS-KDMARK-BUDG-NY     PIC S9(2)  VALUE ZERO.                       
012200                                                                          
012300    03 WS-KDPRODSL-NY        PIC S9(2)  VALUE ZERO.                       
012400                                                                          
012500    03 WS-IDFKNGRP-NY        PIC S9(4)  VALUE ZERO.                       
012600                                                                          
012700    03 WS-SUTOTFSG-NY        PIC S9(11)V9(2) VALUE ZERO.                  
012800                                                                          
012900    03 WS-IDSKURVA-NY        PIC S9(2) VALUE ZERO.                        
013000                                                                          
013100    03 WS-KDMARK-BUDG        PIC S9(2)  VALUE ZERO.                       
013200                                                                          
013300    03 WS-KDPRODSL           PIC S9(2)  VALUE ZERO.                       
013400                                                                          
013500    03 WS-IDFKNGRP           PIC S9(4)  VALUE ZERO.                       
013600                                                                          
013700    03 WS-SUTOTFSG-BUDG      PIC S9(11)V9(2) VALUE ZERO.                  
013800                                                                          
013900    03 WS-IDSKURVA           PIC S9(2) VALUE ZERO.                        
014000                                                                          
014100    03 WS-KDMARK-BUDG-RIGHT  PIC X(3) VALUE SPACE  JUST RIGHT.            
014200                                                                          
014300    03 DAGENS-DATUM          PIC X(6) VALUE ZERO.                         
014400                                                                          
014500    03 RED-SUTOTFSG-BUDG-ZERO.                                            
014600                                                                          
014700       05 FILLER             PIC X(7) VALUE SPACE.                        
014800                                                                          
014900       05 FILLER             PIC X(1) VALUE ZERO.                         
015000                                                                          
015100       05 FILLER             PIC X(6) VALUE SPACE.                        
015200                                                                          
015300  EJECT                                                                   
015400*                                                                         
015500******************************************************************        
015600*            DIV SPARFÄLT                                                 
015700******************************************************************        
015800*                                                                         
015900 01   FILLER                   PIC X(16)   VALUE 'DIV SPARFÄLT'.          
016000*                                                                         
016100 01   SPARFAELT.                                                          
016200    03 SPAR-DIVERSE.                                                      
016300       05 SPAR-TEXT-IND           PIC 9(01)   VALUE ZERO.                 
016400                                                                          
016500  EJECT                                                                   
016600*                                                                         
016700******************************************************************        
016800*                COPYTEXTER                                               
016900******************************************************************        
017000*                                                                         
017100                                                                          
017200 01   FILLER                PIC X(16)   VALUE 'WDECAREA'.                 
017300*01  -COPY WDECAREA.                                                      
017400*++INCLUDE WDECAREAC0                                                     
017500     EJECT                                                                
017600                                                                          
017700 01   FILLER                PIC X(16)   VALUE 'WKPSAREA'.                 
017800*01  -COPY WKPSAREA.                                                      
017900*++INCLUDE WKPSAREAC0                                                     
018000     EJECT                                                                
018100                                                                          
018200 01   FILLER                PIC X(16)   VALUE 'WMEDAREA'.                 
018300*01  -COPY WMEDAREA.                                                      
018400*++INCLUDE WMEDAREAC0                                                     
018500     EJECT                                                                
018600*                                                                         
018700******************************************************************        
018800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
018900******************************************************************        
019000*                MID COPYTEXT                                             
019100******************************************************************        
019200 01    FILLER                 PIC X(16)   VALUE 'MIDCOPYTEXT'.            
019300     SKIP3                                                                
019400*01    MID -COPY W3I21201.                                                
019500*++INCLUDE W3I21201C0                                                     
019600     EJECT                                                                
019700*                                                                         
019800******************************************************************        
019900*                M S G  I/O  AREA                                         
020000******************************************************************        
020100                                                                          
020200 01    FILLER                 PIC X(16)   VALUE 'MSG-IO-AREA'.            
020300*01    -COPY WMSGAREA                                                     
020400*++INCLUDE WMSGAREAC0                                                     
020500     EJECT                                                                
020600                                                                          
020700******************************************************************        
020800*                M O D   COPYTEXT                                         
020900******************************************************************        
021000                                                                          
021100*  03    MOD -COPY W3O21201  -RED MSG-AREA.                               
021200*++INCLUDE W3O21201C0                                                     
021300     EJECT                                                                
021400                                                                          
021500******************************************************************        
021600*               M  F S   AREA                                             
021700******************************************************************        
021800*01    -COPY WMFSAREA                                                     
021900*++INCLUDE WMFSAREAC0                                                     
022000     EJECT                                                                
022100******************************************************************        
022200                                                                          
022300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022400******************************************************************        
022500*                                                                         
022600 01    IMS-WS.                                                            
022700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
022800     SKIP3                                                                
022900*                        **** STATUS-KOD FRÅN IMS                         
023000   03    STATUS-WS               PIC XX.                                  
023100     88    SEGMENT-FINNS                     VALUE '  '.                  
023200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
023300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
023400     SKIP3                                                                
023500   03    GODK-STATUSKODER.                                                
023600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
023700     SKIP3                                                                
023800 01    SSA1                      PIC X(64).                               
023900 01    SSA2                      PIC X(64).                               
024000     EJECT                                                                
024100*                            IMS FUNKTIONSKODER                           
024200*01    -COPY W0003                                                        
024300 ++INCLUDE W0003CCCC0                                                     
024400     EJECT                                                                
024500                                                                          
024600******************************************************************        
024700*                D L I   AREOR                                            
024800******************************************************************        
024900                                                                          
025000*                            DLI INPUT-OUTPUT AREA                        
025100 01    DLI-IO-AREA.                                                       
025200   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
025300*  WLXXCI01 INGEN COPYTEXT - TOM ROT MÅSTE FINNAS                         
025400*  03  WLXXCI11   -COPY WDGX3136         -RED IO-AREA.                    
025500*++INCLUDE WDGX3136C0                                                     
025600     EJECT                                                                
025700 01    DLI-IO-AREA-2.                                                     
025800   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
025900*  WLXXCH01 INGEN COPYTEXT - TOM ROT MÅSTE FINNAS                         
026000*  03  WLXXCH11   -COPY WDGX3134         -RED IO-AREA.                    
026100*++INCLUDE WDGX3134C0                                                     
026200     EJECT                                                                
026300                                                                          
026400******************************************************************        
026500*               LINKAGE SECTION                                           
026600******************************************************************        
026700                                                                          
026800 LINKAGE SECTION.                                                         
026900*01    -COPY W0009     -PRE MSG-                                          
027000 ++INCLUDE W0009CCCC0                                                     
027100     EJECT                                                                
027200*01    -COPY W0008     -PRE XXCI-                                         
027300 ++INCLUDE W0008CCCC0                                                     
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600*01    -COPY W0008     -PRE XXCH-                                         
027700 ++INCLUDE W0008CCCC0                                                     
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000                                                                          
028100******************************************************************        
028200*                 PROCEDURE DIVISION                                      
028300******************************************************************        
028400                                                                          
028500 PROCEDURE DIVISION USING MSG-PCB XXCI-PCB XXCH-PCB.                      
028600                                                                          
028700     ENTRY 'DLITCBL' USING MSG-PCB XXCI-PCB XXCH-PCB.                     
028800                                                                          
028900 STYR SECTION.                                                            
029000     PERFORM IMS-GET-MSG                                                  
029100     IF SEGMENT-FINNS                                                     
029200        PERFORM A-INIT-SPARA-INPUT                                        
029300        PERFORM B-LAES-ROT-I-DATABAS                                      
029400        IF MFS-UPDATE                                                     
029500           PERFORM C-KONTROLLERA-INDATA                                   
029600           IF FELFLAGGA = NEJ                                             
029700              PERFORM D-UPPDATERA                                         
029800              PERFORM E-VISA-RIKTIG-BILD                                  
029900           ELSE                                                           
030000              PERFORM F-VISA-FELBILD-UPPDATERING                          
030100           END-IF                                                         
030200        ELSE                                                              
030300           IF MFS-IDPFK = ' '                                             
030400           PERFORM G-INRAD-KONTROLL                                       
030500           PERFORM H-DOLD-ENTER-HANTERING                                 
030600           ELSE                                                           
030700              IF MFS-IDPFK = '7'                                          
030800                 PERFORM I-PFK7-HANTERING                                 
030900              ELSE                                                        
031000                 IF MFS-IDPFK = '8'                                       
031100                    PERFORM J-DOLD-PFK8-HANTERING                         
031200                 END-IF                                                   
031300              END-IF                                                      
031400           END-IF                                                         
031500           IF FELFLAGGA = NEJ                                             
031600              PERFORM E-VISA-RIKTIG-BILD                                  
031700           ELSE                                                           
031800              PERFORM K-VISA-FELBILD-SOEKNING                             
031900           END-IF                                                         
032000        END-IF                                                            
032100        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
032200        PERFORM IMS-INSERT-MSG                                            
032300     END-IF                                                               
032400     MOVE ZERO TO RETURN-CODE                                             
032500     GOBACK.                                                              
032600    EJECT                                                                 
032700                                                                          
032800 A-INIT-SPARA-INPUT SECTION.                                              
032900     IF MSG-DUBBLA-TRANSKODER                                             
033000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I21201                
033100        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
033200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
033300        MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                          
033400        MOVE MSG-IDPFK TO MFS-IDPFK                                       
033500     ELSE                                                                 
033600        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I21201                 
033700        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
033800        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
033900        MOVE ' ' TO MFS-KDTRTYP                                           
034000        MOVE ' ' TO MFS-IDPFK                                             
034100     END-IF                                                               
034200                                                                          
034300                                                                          
034400     MOVE LOW-VALUE TO MSG-AREA                                           
034500     MOVE 'W3O212N1' TO MFS-IDMOD                                         
034600     MOVE '3212' TO MOD-IDTRANS                                           
034700                                                                          
034800     IF MFS-IDTRANS = '3212'                                              
034900        CONTINUE                                                          
035000     ELSE                                                                 
035100        MOVE ALL '+'                TO MID-INRAD-KDMARK-BUDG              
035200                                       MID-INRAD-KDPRODSL                 
035300                                       MID-INRAD-IDFKNGRP                 
035400        MOVE '++++++++000.00'       TO MID-INRAD-SUTOTFSG-BUDG            
035500        MOVE ALL '+'                TO MID-INRAD-IDSKURVA                 
035600        MOVE ALL '+'                TO MID-INRAD-KDCMD                    
035700                                                                          
035800        MOVE SPACE                  TO MFS-KDTRTYP                        
035900        MOVE '7'                    TO MFS-IDPFK                          
036000     END-IF                                                               
036100                                                                          
036200                                                                          
036300     MOVE MFS-RENSA-FAELT           TO MOD-INRAD-KDMARK-BUDG              
036400                                       MOD-INRAD-KDPRODSL                 
036500                                       MOD-INRAD-IDFKNGRP                 
036600                                       MOD-INRAD-SUTOTFSG-BUDG            
036700                                       MOD-INRAD-IDSKURVA                 
036800                                       MOD-INRAD-KDCMD                    
036900     MOVE MFS-RENSA-FAELT           TO MOD-TEMFSINF                       
037000     MOVE MFS-RENSA-FAELT           TO MOD-TEMFSFEL                       
037100                                                                          
037200     MOVE MID-KDMARK-BUDG-ENTER     TO MOD-KDMARK-BUDG-ENTER              
037300     MOVE MID-KDPRODSL-ENTER        TO MOD-KDPRODSL-ENTER                 
037400     MOVE MID-IDFKNGRP-ENTER        TO MOD-IDFKNGRP-ENTER                 
037500     MOVE MID-KDMARK-BUDG-PFK8      TO MOD-KDMARK-BUDG-PFK8               
037600     MOVE MID-KDPRODSL-PFK8         TO MOD-KDPRODSL-PFK8                  
037700     MOVE MID-IDFKNGRP-PFK8         TO MOD-IDFKNGRP-PFK8                  
037800                                                                          
037900     ACCEPT DAGENS-DATUM            FROM DATE                             
038000     IF SWEDISH-TEXT                                                      
038100        MOVE 1                      TO SPAR-TEXT-IND                      
038200        MOVE 'S  '                  TO MED-IDSKYLT                        
038300     ELSE                                                                 
038400        MOVE 2                      TO SPAR-TEXT-IND                      
038500        MOVE 'GB '                  TO MED-IDSKYLT                        
038600     END-IF.                                                              
038700    EJECT                                                                 
038800                                                                          
038900                                                                          
039000 B-LAES-ROT-I-DATABAS SECTION.                                            
039100     PERFORM IMS-GU-WLXXCI01-DELBUDG-ROT.                                 
039200    EJECT                                                                 
039300                                                                          
039400                                                                          
039500 C-KONTROLLERA-INDATA SECTION.                                            
039600                                                                          
039700                                                                          
039800     PERFORM CA-TOM-INRAD-PF11-KONTROLL                                   
039900     IF FELFLAGGA = NEJ                                                   
040000        PERFORM CB-FORMELL-INDATAKONTROLL                                 
040100        PERFORM CC-UPPDATERINGSTYP-RELKOLL                                
040200     ELSE                                                                 
040300        MOVE MFS-NUM-FAELT-FEL  TO MOD-INRAD-KDMARK-BUDG-ATTR             
040400                                   MOD-INRAD-KDPRODSL-ATTR                
040500                                   MOD-INRAD-IDFKNGRP-ATTR                
040600                                   MOD-INRAD-SUTOTFSG-BUDG-ATTR           
040700                                   MOD-INRAD-IDSKURVA-ATTR                
040800                                   MOD-INRAD-KDCMD-ATTR                   
040900     END-IF.                                                              
041000    EJECT                                                                 
041100                                                                          
041200 CA-TOM-INRAD-PF11-KONTROLL SECTION.                                      
041300                                                                          
041400*          SEKTIONEN GÅR IGENOM INDATARADEN FÖR ATT TA REDA PÅ            
041500*          OM SAMTLIGA INDATAFÄLT ÄR OIFYLLDA.                            
041600*          ÄR INDATARADEN TOM VID PF11-TRYCKNING BLIR FELFLAGGA           
041700*          = JA. DÄREFTER VISAS SPARAD BILD MED MEDDELANDE:               
041800*          "EJ PF11 OCH TOM INDATARAD".                                   
041900                                                                          
042000                                                                          
042100     IF MID-INRAD-KDMARK-BUDG = ALL  '+'  AND                             
042200            MID-INRAD-KDPRODSL = ALL '+'  AND                             
042300            MID-INRAD-IDFKNGRP = ALL '+'  AND                             
042400            MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00' AND                
042500            MID-INRAD-IDSKURVA = ALL '+'  AND                             
042600            MID-INRAD-KDCMD = ALL '+'                                     
042700        MOVE JA                     TO FELFLAGGA                          
042800        MOVE JA                     TO FEL-1-FLAGGA                       
042900     ELSE                                                                 
043000        CONTINUE                                                          
043100     END-IF.                                                              
043200   EJECT                                                                  
043300                                                                          
043400 CB-FORMELL-INDATAKONTROLL SECTION.                                       
043500                                                                          
043600*          SEKTIONEN KONTROLLERAR INDATARADEN, LÄGGER "RENSA              
043700*          FÄLT" TILL EJ IFYLLDA FÄLT OCH "RÖR EJ FÄLT" TILL              
043800*          IFYLLDA.                                                       
043900*          OM "LÄSNYCKELFÄLTEN" KDMARK-BUDG, KDPRODSL OCH                 
044000*          IDFKNGRP ÄR OIFYLLDA ELLER ONUMERISKA SLÅS                     
044100*          FELFLAGGA PÅ (JA).                                             
044200*          FÖR SUTOTFSG ACCEPTERAS I DENNA SEKTION ALLA SVAR              
044300*          MEDAN DET FÖR IDSKURVA KRÄVS NUMERISKA SVAR.                   
044400*          GER FELFLAGGA PÅ OM DET INMATADE ÄR ONUMERISKT.                
044500*                                                                         
044600*          VID PÅSLAGEN FELFLAGGA LÄGGS SPARAD BILD UT MED                
044700*          FELTEXT "UPPLYSTA FÄLT FEL" OCH FELAKTIGA FÄLT                 
044800*          UPPLYSTA.                                                      
044900                                                                          
045000                                                                          
045100*          KDMARK-BUDG                                                    
045200                                                                          
045300     IF MID-INRAD-KDMARK-BUDG = ALL '+'                                   
045400         MOVE JA                    TO FELFLAGGA                          
045500         MOVE MFS-RENSA-FAELT       TO MOD-INRAD-KDMARK-BUDG              
045600         MOVE MFS-NUM-FAELT-FEL                                           
045700                     TO MOD-INRAD-KDMARK-BUDG-ATTR                        
045800     ELSE                                                                 
045900        IF MID-INRAD-KDMARK-BUDG NUMERIC                                  
046000           MOVE MFS-NUM-FAELT-RAETT                                       
046100                     TO MOD-INRAD-KDMARK-BUDG-ATTR                        
046200        ELSE                                                              
046300           MOVE JA                  TO FELFLAGGA                          
046400           MOVE MFS-NUM-FAELT-FEL                                         
046500                     TO MOD-INRAD-KDMARK-BUDG-ATTR                        
046600        END-IF                                                            
046700        MOVE MFS-ROER-EJ-FAELT                                            
046800                     TO MOD-INRAD-KDMARK-BUDG                             
046900     END-IF                                                               
047000                                                                          
047100                                                                          
047200*         KDPRODSL                                                        
047300                                                                          
047400     IF MID-INRAD-KDPRODSL = ALL '+'                                      
047500         MOVE JA                    TO FELFLAGGA                          
047600         MOVE MFS-RENSA-FAELT       TO MOD-INRAD-KDPRODSL                 
047700         MOVE MFS-NUM-FAELT-FEL     TO MOD-INRAD-KDPRODSL-ATTR            
047800     ELSE                                                                 
047900        IF MID-INRAD-KDPRODSL NUMERIC                                     
048000           MOVE MFS-NUM-FAELT-RAETT                                       
048100                     TO MOD-INRAD-KDPRODSL-ATTR                           
048200        ELSE                                                              
048300           MOVE JA                  TO FELFLAGGA                          
048400           MOVE MFS-NUM-FAELT-FEL                                         
048500                     TO MOD-INRAD-KDPRODSL-ATTR                           
048600        END-IF                                                            
048700        MOVE MFS-ROER-EJ-FAELT                                            
048800                     TO MOD-INRAD-KDPRODSL                                
048900     END-IF                                                               
049000                                                                          
049100*          IDFKNGRP                                                       
049200                                                                          
049300     IF MID-INRAD-IDFKNGRP = ALL '+'                                      
049400         MOVE JA                    TO FELFLAGGA                          
049500         MOVE MFS-RENSA-FAELT       TO MOD-INRAD-IDFKNGRP                 
049600         MOVE MFS-NUM-FAELT-FEL     TO MOD-INRAD-IDFKNGRP-ATTR            
049700     ELSE                                                                 
049800        IF MID-INRAD-IDFKNGRP NUMERIC                                     
049900           MOVE MFS-NUM-FAELT-RAETT                                       
050000                     TO MOD-INRAD-IDFKNGRP-ATTR                           
050100        ELSE                                                              
050200           MOVE JA                  TO FELFLAGGA                          
050300           MOVE MFS-NUM-FAELT-FEL                                         
050400                     TO MOD-INRAD-IDFKNGRP-ATTR                           
050500        END-IF                                                            
050600        MOVE MFS-ROER-EJ-FAELT                                            
050700                     TO MOD-INRAD-IDFKNGRP                                
050800     END-IF                                                               
050900                                                                          
051000*          KONTROLL AV SUTOTFSG-BUDG                                      
051100                                                                          
051200                                                                          
051300                                                                          
051400     IF MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00'                        
051500        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-SUTOTFSG-BUDG            
051600        MOVE MFS-NUM-FAELT-RAETT                                          
051700                     TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                      
051800     ELSE                                                                 
051900        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-SUTOTFSG-BUDG            
052000        MOVE MFS-NUM-FAELT-RAETT                                          
052100                     TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                      
052200     END-IF                                                               
052300                                                                          
052400                                                                          
052500*          IDSKURVA                                                       
052600                                                                          
052700     IF MID-INRAD-IDSKURVA = ALL '+'                                      
052800         MOVE MFS-RENSA-FAELT       TO MOD-INRAD-IDSKURVA                 
052900         MOVE MFS-NUM-FAELT-RAETT   TO MOD-INRAD-IDSKURVA-ATTR            
053000     ELSE                                                                 
053100        IF MID-INRAD-IDSKURVA NUMERIC                                     
053200           MOVE MFS-NUM-FAELT-RAETT TO MOD-INRAD-IDSKURVA-ATTR            
053300        ELSE                                                              
053400           MOVE JA                  TO FELFLAGGA                          
053500           MOVE MFS-NUM-FAELT-FEL                                         
053600                     TO MOD-INRAD-IDSKURVA-ATTR                           
053700        END-IF                                                            
053800        MOVE MFS-ROER-EJ-FAELT                                            
053900                     TO MOD-INRAD-IDSKURVA                                
054000     END-IF                                                               
054100                                                                          
054200                                                                          
054300*          KDCMD                                                          
054400                                                                          
054500     IF MID-INRAD-KDCMD = ALL '+'                                         
054600         MOVE MFS-RENSA-FAELT       TO MOD-INRAD-KDCMD                    
054700         MOVE MFS-ALFA-FAELT-RAETT   TO MOD-INRAD-KDCMD-ATTR              
054800     ELSE                                                                 
054900        IF MID-INRAD-KDCMD = 'B' OR 'D'                                   
055000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-INRAD-KDCMD-ATTR              
055100        ELSE                                                              
055200           MOVE JA                  TO FELFLAGGA                          
055300           MOVE MFS-ALFA-FAELT-FEL                                        
055400                     TO MOD-INRAD-KDCMD-ATTR                              
055500        END-IF                                                            
055600        MOVE MFS-ROER-EJ-FAELT                                            
055700                     TO MOD-INRAD-KDCMD                                   
055800     END-IF                                                               
055900    EJECT                                                                 
056000        .                                                                 
056100                                                                          
056200 CC-UPPDATERINGSTYP-RELKOLL SECTION.                                      
056300                                                                          
056400                                                                          
056500*          I SEKTIONEN KONTROLLERAS INDATARADEN MED AVSEENDE PÅ           
056600*          DET INMATADE DATAT OCH VILKEN UPPDATERINGSTYP SOM              
056700*          GÄLLER.                                                        
056800*          KDMARK-BUDG MÅSTE VARA = 0 ELLER > 0 OCH < 97 (96              
056900*          MARKNADER. MARKNAD "0" BETYDER SAMTLIGA MARKNADER.             
057000*          KDPRODSL KONTROLLERAS MED HJÄLP AV WKPSKONV                    
057100*          SUTOTFSG-BUDG GENOMGÅR NUMERISKONTROLL OCH                     
057200*          PACKNING I WDECEDIT. IDFKNGRP ACCEPTERAS OM ANGIVET            
057300*          VÄRDE ÄR > 999. KONTROLL OM ANGIVEN IDSKURVA FINNS   CL        
057400*          GÖRS I 3134-SEGMENTET I WDG2-BASEN. DÄREFTER LÄSES             
057500*          3136-SEGMENTET I WDG2-BASEN FÖR ATT SE OM ANGIVEN              
057600*          KOMBINATION AV KDMARK-BUDG, KDPRODSL OCH IDFKNGRP              
057700*          FINNS.                                                         
057800*                                                                         
057900*          VID NYREGISTRERING KRÄVS ATT SUTOTFSG-BUDG OCH IDSKURVA        
058000*          ÄR IFYLLDA. 0 BETYDER ATT BUDGETEN SKALL VARA NOLL             
058100*          RESP, ATT IDSKURVA INTE TILLÄMPAS FÖR DELBUDGETEN.             
058200*          FINNS SEGMENT (ÄNDRING) MEDFÖR OIFYLLDA FÄLT ATT INGEN         
058300*          ÄNDRING SKALL GÖRAS, 0 ATT SUTOTFSG SKALL ÄNDRAS TILL          
058400*          0 RESP, ATT IDSKURVA INTE LÄNGRE SKALL FINNAS.                 
058500*                                                                         
058600*          VID FEL VISAS SPARAD BILD MED FELAKTIGA FÄLT UPPLYSTA,         
058700*          FELTEXTEN "UPPLYSTA FÄLT FEL" SAMT EV MEDDELANDETEXT           
058800*          "MARKNAD FINNS EJ" , "PRODUKTSLAG FINNS EJ",                   
058900*          "PRODUKTSLAG FINNS EJ" , "ISDKURVA FINNS EJ".                  
059000*          "FUNKTIONSGRUPP FINNS EJ" ELLER "SÄSONGSKURVA FINNS EJ"        
059100                                                                          
059200*          KONTROLL AV MARKNAD                                            
059300                                                                          
059400     IF MID-INRAD-KDMARK-BUDG > '000' AND < '097'                         
059500        MOVE MID-INRAD-KDMARK-BUDG                                        
059600                  TO WS-KDMARK-BUDG-NY                                    
059700     ELSE                                                                 
059800        MOVE JA TO FELFLAGGA                                              
059900        MOVE JA TO MED-4-FLAGGA                                           
060000        MOVE MFS-NUM-FAELT-FEL                                            
060100                  TO MOD-INRAD-KDMARK-BUDG-ATTR                           
060200     END-IF                                                               
060300                                                                          
060400*          KONTROLL OM PRODUKTSLAG FINNS                                  
060500                                                                          
060600     MOVE 2                         TO KPS-KDCALL                         
060700     MOVE MID-INRAD-KDPRODSL        TO KPS-KDPRODSL                       
060800     CALL WKPSKONV USING KPS-WKPSAREA                                     
060900     IF KPS-KDSVAR = ' '                                                  
061000        MOVE KPS-KDPRODSL           TO WS-KDPRODSL-NY                     
061100     ELSE                                                                 
061200        MOVE JA TO FELFLAGGA                                              
061300        MOVE JA TO MED-5-FLAGGA                                           
061400        MOVE MFS-NUM-FAELT-FEL                                            
061500                       TO MOD-INRAD-KDPRODSL-ATTR                         
061600     END-IF                                                               
061700                                                                          
061800                                                                          
061900*          KONTROLL AV IDFKNGRP                                           
062000                                                                          
062100     IF MID-INRAD-IDFKNGRP > ZERO                                         
062200        MOVE MID-INRAD-IDFKNGRP     TO WS-IDFKNGRP-NY                     
062300     ELSE                                                                 
062400        MOVE JA TO FELFLAGGA                                              
062500        MOVE MFS-NUM-FAELT-FEL                                            
062600                     TO MOD-INRAD-IDFKNGRP-ATTR                           
062700     END-IF                                                               
062800                                                                          
062900*          KONTROLL/PACKNING AV SUTOTFSG-BUDG                             
063000                                                                          
063100                                                                          
063200                                                                          
063300     IF MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00' OR                     
063400        MID-INRAD-SUTOTFSG-BUDG =  ZERO                                   
063500        CONTINUE                                                          
063600     ELSE                                                                 
063700        MOVE MID-INRAD-SUTOTFSG-BUDG                                      
063800                     TO DEC-IDFRIDATA                                     
063900        MOVE 11                     TO DEC-KVHELTAL                       
064000        MOVE 2                      TO DEC-KVDECIMAL                      
064100        CALL WDECEDIT USING DEC-WDECAREA                                  
064200        MOVE DEC-IDEDITDATA         TO WS-SUTOTFSG-NY                     
064300        IF DEC-KDSVAR-OK                                                  
064400                                                                          
064500           MOVE MFS-NUM-FAELT-RAETT                                       
064600                     TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                      
064700        ELSE                                                              
064800           MOVE JA                  TO FELFLAGGA                          
064900           MOVE MFS-NUM-FAELT-FEL                                         
065000                 TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                          
065100        END-IF                                                            
065200     END-IF                                                               
065300                                                                          
065400                                                                          
065500*          KONTROLL OM SÄSONGSKURVA FINNS                                 
065600                                                                          
065700     IF MID-INRAD-IDSKURVA = ALL '+' OR                                   
065800        MID-INRAD-IDSKURVA =  ZERO                                        
065900        CONTINUE                                                          
066000     ELSE                                                                 
066100        MOVE MID-INRAD-IDSKURVA     TO W-IDSKURVA                         
066200        PERFORM IMS-GU-WLXXCH11-IDSKURVA                                  
066300        IF SEGMENT-FINNS                                                  
066400           MOVE MID-INRAD-IDSKURVA   TO WS-IDSKURVA-NY                    
066500        ELSE                                                              
066600           MOVE JA TO FELFLAGGA                                           
066700           MOVE MFS-NUM-FAELT-FEL    TO MOD-INRAD-IDSKURVA-ATTR           
066800           MOVE '111'                TO MED-IDMFSINF                      
066900           CALL WMEDKONV             USING MED-WMEDAREA                   
067000           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
067100        END-IF                                                            
067200     END-IF                                                               
067300                                                                          
067400     IF FELFLAGGA = JA                                                    
067500        CONTINUE                                                          
067600     ELSE                                                                 
067700                                                                          
067800*           UPPPDATERINGSTYP ?                                            
067900                                                                          
068000        MOVE WS-KDMARK-BUDG-NY      TO W-KDMARK-BUDG                      
068100        MOVE WS-KDPRODSL-NY         TO W-KDPRODSL                         
068200        MOVE WS-IDFKNGRP-NY         TO W-IDFKNGRP                         
068300                                                                          
068400        IF MID-INRAD-KDCMD = 'B' OR 'D'                                   
068500           PERFORM IMS-GHNP-WLXXCI11-HAEMTA                               
068600           IF SEGMENT-SAKNAS                                              
068700              MOVE JA               TO FELFLAGGA                          
068800              MOVE '010'            TO MED-IDMFSFEL                       
068900              CALL WMEDKONV         USING MED-WMEDAREA                    
069000              MOVE MED-MFSFEL       TO MOD-TEMFSFEL                       
069100              MOVE MFS-NUM-FAELT-FEL                                      
069200                     TO MOD-INRAD-KDMARK-BUDG-ATTR                        
069300                        MOD-INRAD-KDPRODSL-ATTR                           
069400                        MOD-INRAD-IDFKNGRP-ATTR                           
069500                        MOD-INRAD-KDCMD-ATTR                              
069600            ELSE                                                          
069700               MOVE JA              TO DLET-FLAGGA                        
069800            END-IF                                                        
069900                                                                          
070000*          EJ BORTTAG                                                     
070100                                                                          
070200        ELSE                                                              
070300           PERFORM IMS-GHNP-WLXXCI11-HAEMTA                               
070400           IF SEGMENT-SAKNAS                                              
070500*                                    NYREG                                
070600                                                                          
070700              IF MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00'               
070800                 MOVE JA               TO FELFLAGGA                       
070900                 MOVE MFS-NUM-FAELT-FEL                                   
071000                        TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                   
071100              ELSE                                                        
071200                 IF MID-INRAD-SUTOTFSG-BUDG = '00000000000.00'            
071300                    MOVE JA            TO FELFLAGGA                       
071400                    MOVE MFS-NUM-FAELT-FEL                                
071500                           TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                
071600                 ELSE                                                     
071700                    MOVE JA            TO ISRT-FLAGGA                     
071800                    MOVE WS-SUTOTFSG-NY                                   
071900                        TO WS-SUTOTFSG-BUDG                               
072000                 END-IF                                                   
072100              END-IF                                                      
072200                                                                          
072300*             IDSKURVA                                                    
072400                                                                          
072500              IF MID-INRAD-IDSKURVA = ALL '+'                             
072600                 MOVE JA               TO FELFLAGGA                       
072700                 MOVE MFS-NUM-FAELT-FEL                                   
072800                        TO MOD-INRAD-IDSKURVA-ATTR                        
072900              ELSE                                                        
073000                 IF MID-INRAD-IDSKURVA = ZERO                             
073100                    MOVE ZERO          TO WS-IDSKURVA-NY                  
073200                 ELSE                                                     
073300                    MOVE WS-IDSKURVA-NY                                   
073400                        TO WS-IDSKURVA                                    
073500                 END-IF                                                   
073600              END-IF                                                      
073700                                                                          
073800           ELSE                                                           
073900*          ÄNDRING                                                        
074000                                                                          
074100              MOVE JA                  TO REPL-FLAGGA                     
074200              PERFORM CCA-FLYTTA-BAS-WS                                   
074300              IF MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00'               
074400                 CONTINUE                                                 
074500              ELSE                                                        
074600                 IF MID-INRAD-SUTOTFSG-BUDG = '00000000000.00'            
074700                    MOVE JA            TO FELFLAGGA                       
074800                    MOVE MFS-NUM-FAELT-FEL TO                             
074900                                 MOD-INRAD-SUTOTFSG-BUDG-ATTR             
075000                 ELSE                                                     
075100                    MOVE WS-SUTOTFSG-NY                                   
075200                        TO WS-SUTOTFSG-BUDG                               
075300                 END-IF                                                   
075400              END-IF                                                      
075500                                                                          
075600                                                                          
075700              IF MID-INRAD-IDSKURVA = ALL '+'                             
075800                 CONTINUE                                                 
075900              ELSE                                                        
076000                 IF MID-INRAD-IDSKURVA = ZERO                             
076100                    MOVE ZERO          TO WS-IDSKURVA                     
076200                 ELSE                                                     
076300                    MOVE WS-IDSKURVA-NY                                   
076400                        TO WS-IDSKURVA                                    
076500                 END-IF                                                   
076600              END-IF                                                      
076700           END-IF                                                         
076800        END-IF                                                            
076900     END-IF.                                                              
077000    EJECT                                                                 
077100                                                                          
077200                                                                          
077300                                                                          
077400 CCA-FLYTTA-BAS-WS SECTION.                                               
077500                                                                          
077600*          VID ÄNDRING FLYTTTAS DATA FRÅN BASEN TILL WS-FÄLT              
077700*          FÖR ATT BLI ÖVERSKRIVNA AV FÄLT MED ÄNDRINGAR.                 
077800                                                                          
077900                                                                          
078000     MOVE 3136-SUTOTFSG-BUDG        TO WS-SUTOTFSG-BUDG                   
078100     MOVE 3136-IDSKURVA             TO WS-IDSKURVA.                       
078200   EJECT                                                                  
078300                                                                          
078400                                                                          
078500                                                                          
078600 D-UPPDATERA SECTION.                                                     
078700                                                                          
078800*          BASEN UPPDATERAS OCH MEDDELANDE "NY DELBUDGET UPPLAGD"         
078900*          ELLER "DELBUDGET ÄNDRAD" VISAS.                                
079000                                                                          
079100     IF ISRT-FLAGGA = JA                                                  
079200        PERFORM DA-FLYTTA-WS-BAS                                          
079300        PERFORM IMS-ISRT-WLXXCI11                                         
079400        MOVE '101'            TO MED-IDMFSINF                             
079500        CALL WMEDKONV         USING MED-WMEDAREA                          
079600        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
079700        PERFORM DB-BLANKA-INRAD                                           
079800     END-IF                                                               
079900                                                                          
080000     IF REPL-FLAGGA = JA                                                  
080100        PERFORM DA-FLYTTA-WS-BAS                                          
080200        PERFORM IMS-REPL-WLXXCI11                                         
080300        MOVE '101'            TO MED-IDMFSINF                             
080400        CALL WMEDKONV         USING MED-WMEDAREA                          
080500        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
080600        PERFORM DB-BLANKA-INRAD                                           
080700     END-IF                                                               
080800                                                                          
080900     IF DLET-FLAGGA = JA                                                  
081000        PERFORM IMS-DLET-WLXXCI11                                         
081100        MOVE '101'            TO MED-IDMFSINF                             
081200        CALL WMEDKONV         USING MED-WMEDAREA                          
081300        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
081400        PERFORM DB-BLANKA-INRAD                                           
081500        PERFORM IMS-GNP-WLXXCI11-DELBUDG-NEXT                             
081600     END-IF                                                               
081700                                                                          
081800                                                                          
081810      .                                                                   
082000    EJECT                                                                 
082100                                                                          
082200                                                                          
082300 DA-FLYTTA-WS-BAS SECTION.                                                
082400                                                                          
082500                                                                          
082600*          FLYTTAR GODKÄNT INDATA TILL I/O AREAN.                         
082700                                                                          
082800     MOVE W-KDMARK-BUDG             TO 3136-KDMARK-BUDG                   
082900     MOVE W-KDPRODSL                TO 3136-KDPRODSL                      
083000     MOVE W-IDFKNGRP                TO 3136-IDFKNGRP                      
083100     MOVE WS-SUTOTFSG-BUDG          TO 3136-SUTOTFSG-BUDG                 
083200     MOVE WS-IDSKURVA               TO 3136-IDSKURVA                      
083300     MOVE DAGENS-DATUM              TO 3136-TIUPPDAT                      
083400    EJECT                                                                 
083500      .                                                                   
083600                                                                          
083700 DB-BLANKA-INRAD SECTION.                                                 
083800                                                                          
083900                                                                          
084000*          BLANKAR UT INDATARAD EFTER UTFÖRD UPPDATERING.                 
084100                                                                          
084200                                                                          
084300                                                                          
084400     MOVE MFS-RENSA-FAELT           TO MOD-INRAD-KDMARK-BUDG              
084500                                       MOD-INRAD-KDPRODSL                 
084600                                       MOD-INRAD-IDFKNGRP                 
084700                                       MOD-INRAD-SUTOTFSG-BUDG            
084800                                       MOD-INRAD-IDSKURVA                 
084900                                       MOD-INRAD-KDCMD                    
085000     MOVE SPACE                                                           
085100                     TO MOD-INRAD-SUTOTFSG-NOLLFAELT                      
085200     MOVE MFS-RENSA-FAELT           TO MOD-INRAD-IDSKURVA.                
085300    EJECT                                                                 
085400                                                                          
085500                                                                          
085600 E-VISA-RIKTIG-BILD SECTION.                                              
085700                                                                          
085800                                                                          
085900*          VISAR NYLÄST BILD EFTER SÖKNING ELLER UTFÖRD                   
086000*          UPPDATERING. VID UPPDATERING MEDDELANDE OM                     
086100*          UPPDATERINGSTYP.                                               
086200                                                                          
086300     MOVE +1                        TO RAD-IX                             
086400     PERFORM UNTIL RAD-IX = MAX-RAD-IX OR SEGMENT-SAKNAS                  
086500                                                                          
086600        IF 3136-KDMARK-BUDG = ZERO                                        
086700           MOVE '  0'      TO MOD-UTRAD-KDMARK-BUDG(RAD-IX)               
086800        ELSE                                                              
086900           MOVE 3136-KDMARK-BUDG    TO WS-3-COMP-UPPACKNING               
087000           MOVE WS-3-COMP-UPPACKNING                                      
087100                     TO WS-KDMARK-BUDG-RIGHT                              
087200           MOVE WS-KDMARK-BUDG-RIGHT                                      
087300                              TO MOD-UTRAD-KDMARK-BUDG(RAD-IX)            
087400           INSPECT MOD-UTRAD-KDMARK-BUDG(RAD-IX)                          
087500                              REPLACING LEADING ZERO BY SPACE             
087600        END-IF                                                            
087700                                                                          
087800                                                                          
087900        MOVE 3136-KDPRODSL         TO WS-3-COMP-UPPACKNING                
088000        MOVE WS-3-COMP-UPPACKNING  TO MOD-UTRAD-KDPRODSL(RAD-IX)          
088100                                                                          
088200        MOVE 3136-IDFKNGRP         TO WS-5-COMP-UPPACKNING                
088300        MOVE WS-5-COMP-UPPACKNING                                         
088400                     TO MOD-UTRAD-IDFKNGRP(RAD-IX)                        
088500                                                                          
088600        IF 3136-SUTOTFSG-BUDG = ZERO                                      
088700           MOVE RED-SUTOTFSG-BUDG-ZERO                                    
088800                     TO MOD-UTRAD-SUTOTFSG-BUDG(RAD-IX)                   
088900        ELSE                                                              
089000           MOVE 3136-SUTOTFSG-BUDG  TO WS-13-COMP-UPPACKNING              
089100           MOVE WS-13-COMP-UPPACKNING                                     
089200                     TO MOD-UTRAD-SUTOTFSG-BUDG(RAD-IX)                   
089300                                                                          
089400        END-IF                                                            
089500                                                                          
089600        MOVE 3136-IDSKURVA          TO WS-3-COMP-UPPACKNING               
089700        MOVE WS-3-COMP-UPPACKNING                                         
089800                     TO MOD-UTRAD-IDSKURVA(RAD-IX)                        
089900        INSPECT MOD-UTRAD-IDSKURVA(RAD-IX)                                
090000                     REPLACING FIRST ZERO BY SPACE                        
090100                                                                          
090200        IF RAD-IX = 1                                                     
090300        MOVE MOD-UTRAD-KDMARK-BUDG(1) TO MOD-KDMARK-BUDG-ENTER            
090400        INSPECT MOD-KDMARK-BUDG-ENTER                                     
090500                  REPLACING LEADING SPACE BY ZERO                         
090600        MOVE MOD-UTRAD-KDPRODSL(1)  TO MOD-KDPRODSL-ENTER                 
090700        MOVE MOD-UTRAD-IDFKNGRP(1)  TO MOD-IDFKNGRP-ENTER                 
090800        END-IF                                                            
090900                                                                          
091000        PERFORM IMS-GNP-WLXXCI11-DELBUDG-NEXT                             
091100        ADD 1 TO RAD-IX                                                   
091200     END-PERFORM                                                          
091300                                                                          
091400     IF RAD-IX = MAX-RAD-IX                                               
091500        MOVE 3136-KDMARK-BUDG       TO WS-3-COMP-UPPACKNING               
091600        MOVE WS-3-COMP-UPPACKNING   TO WS-KDMARK-BUDG-RIGHT               
091700        MOVE WS-KDMARK-BUDG-RIGHT   TO MOD-KDMARK-BUDG-PFK8               
091800        INSPECT MOD-KDMARK-BUDG-PFK8                                      
091900                     REPLACING LEADING SPACE BY ZERO                      
092000                                                                          
092100        MOVE 3136-KDPRODSL          TO WS-3-COMP-UPPACKNING               
092200        MOVE WS-3-COMP-UPPACKNING   TO MOD-KDPRODSL-PFK8                  
092300                                                                          
092400        MOVE 3136-IDFKNGRP          TO WS-5-COMP-UPPACKNING               
092500        MOVE WS-5-COMP-UPPACKNING                                         
092600                     TO MOD-IDFKNGRP-PFK8                                 
092700        IF MFS-UPDATE                                                     
092800           CONTINUE                                                       
092900        ELSE                                                              
093000           MOVE '105'               TO MED-IDMFSINF                       
093100           CALL WMEDKONV            USING MED-WMEDAREA                    
093200           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
093300        END-IF                                                            
093400     END-IF                                                               
093500                                                                          
093600     IF SEGMENT-SAKNAS                                                    
093700        PERFORM UNTIL RAD-IX = MAX-RAD-IX                                 
093800           MOVE MFS-RENSA-FAELT TO MOD-UTRAD-KDMARK-BUDG(RAD-IX)          
093900                                MOD-UTRAD-KDPRODSL(RAD-IX)                
094000                                MOD-UTRAD-IDFKNGRP(RAD-IX)                
094100                                MOD-UTRAD-SUTOTFSG-BUDG(RAD-IX)           
094200                                MOD-UTRAD-IDSKURVA(RAD-IX)                
094300           ADD 1 TO RAD-IX                                                
094400        END-PERFORM                                                       
094500        MOVE MOD-UTRAD-KDMARK-BUDG(1) TO MOD-KDMARK-BUDG-PFK8             
094600        INSPECT MOD-KDMARK-BUDG-PFK8                                      
094700                             REPLACING LEADING SPACE BY ZERO              
094800        MOVE MOD-UTRAD-KDPRODSL(1)    TO MOD-KDPRODSL-PFK8                
094900        MOVE MOD-UTRAD-IDFKNGRP(1)    TO MOD-IDFKNGRP-PFK8                
095000        IF MFS-UPDATE                                                     
095100           CONTINUE                                                       
095200        ELSE                                                              
095300           MOVE '106'               TO MED-IDMFSINF                       
095400           CALL WMEDKONV            USING MED-WMEDAREA                    
095500           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
095600        END-IF                                                            
095700     END-IF.                                                              
095800  EJECT                                                                   
095900                                                                          
096000                                                                          
096100                                                                          
096200 F-VISA-FELBILD-UPPDATERING SECTION.                                      
096300                                                                          
096400*          VID ICKE GODKÄNT INDATA VISAS SPARAD BILD OCH                  
096500*          SPARAD INDATARAD MED FELAKTIGA FÄLT UPPLYSTA.                  
096600*          FELTEXT "UPPLYSTA FÄLT FEL" SAMT EV MEDDELANDEN.               
096700                                                                          
096800                                                                          
096900     IF FEL-1-FLAGGA = JA                                                 
097000        MOVE '011'                  TO MED-IDMFSFEL                       
097100        CALL WMEDKONV               USING MED-WMEDAREA                    
097200        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
097300     ELSE                                                                 
097400        MOVE '001'                  TO MED-IDMFSFEL                       
097500        CALL WMEDKONV               USING MED-WMEDAREA                    
097600        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
097700     END-IF                                                               
097800     MOVE +1 TO RAD-IX                                                    
097900     PERFORM UNTIL RAD-IX = MAX-RAD-IX                                    
098000        MOVE MFS-ROER-EJ-FAELT                                            
098100                     TO MOD-UTRAD-KDMARK-BUDG(RAD-IX)                     
098200                        MOD-UTRAD-KDPRODSL(RAD-IX)                        
098300                        MOD-UTRAD-IDFKNGRP(RAD-IX)                        
098400                        MOD-UTRAD-SUTOTFSG-BUDG(RAD-IX)                   
098500                       MOD-UTRAD-IDSKURVA(RAD-IX)                         
098600        ADD 1 TO RAD-IX                                                   
098700     END-PERFORM                                                          
098800     IF MED-4-FLAGGA = JA                                                 
098900        MOVE '108'                  TO MED-IDMFSINF                       
099000        CALL WMEDKONV               USING MED-WMEDAREA                    
099100        MOVE MED-MFSINF             TO MOD-TEMFSINF                       
099200     ELSE                                                                 
099300        IF MED-5-FLAGGA = JA                                              
099400           MOVE '109'               TO MED-IDMFSINF                       
099500           CALL WMEDKONV            USING MED-WMEDAREA                    
099600           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
099700        END-IF                                                            
099800     END-IF.                                                              
099900   EJECT                                                                  
100000                                                                          
100100                                                                          
100200 G-INRAD-KONTROLL SECTION.                                                
100300                                                                          
100400                                                                          
100500*       KONTROLLERAR OM INDARATARAD ÄR IFYLLD VID                         
100600*       ENTERTRYCKNING, I SÅ FALL SLÅS FELFLAGGA PÅ OCH                   
100700*       SPARAD BILD VISAS.                                                
100800                                                                          
100900                                                                          
101000     IF MID-INRAD-KDMARK-BUDG = ALL '+'   AND                             
101100            MID-INRAD-KDPRODSL = ALL '+'  AND                             
101200            MID-INRAD-IDFKNGRP = ALL '+'  AND                             
101300            MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00' AND                
101400            MID-INRAD-IDSKURVA = ALL '+'  AND                             
101500            MID-INRAD-KDCMD = ALL '+'                                     
101600        CONTINUE                                                          
101700     ELSE                                                                 
101800        MOVE JA                     TO FELFLAGGA                          
101900        MOVE '003'                  TO MED-IDMFSFEL                       
102000        CALL WMEDKONV               USING MED-WMEDAREA                    
102100        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
102200     END-IF.                                                              
102300    EJECT                                                                 
102400                                                                          
102500 H-DOLD-ENTER-HANTERING SECTION.                                          
102600                                                                          
102700     MOVE MID-KDMARK-BUDG-ENTER     TO W-KDMARK-BUDG                      
102800                                                                          
102900     MOVE MID-KDPRODSL-ENTER        TO W-KDPRODSL                         
103000                                                                          
103100     MOVE MID-IDFKNGRP-ENTER        TO W-IDFKNGRP                         
103200                                                                          
103300                                                                          
103400                                                                          
103500     PERFORM IMS-GNP-WLXXCI11-DELBUDG-LIKA.                               
103600                                                                          
103700                                                                          
103800 I-PFK7-HANTERING SECTION.                                                
103900                                                                          
104000     MOVE ZERO                      TO W-KDMARK-BUDG                      
104100     MOVE ZERO                      TO W-KDPRODSL                         
104200     MOVE ZERO                      TO W-IDFKNGRP                         
104300     PERFORM IMS-GNP-WLXXCI11-DELBUDG-NEXT.                               
104400                                                                          
104500 J-DOLD-PFK8-HANTERING SECTION.                                           
104600                                                                          
104700     MOVE MID-KDMARK-BUDG-PFK8      TO W-KDMARK-BUDG                      
104800                                                                          
104900     MOVE MID-KDPRODSL-PFK8         TO W-KDPRODSL                         
105000                                                                          
105100     MOVE MID-IDFKNGRP-PFK8         TO W-IDFKNGRP                         
105200                                                                          
105300     PERFORM IMS-GNP-WLXXCI11-DELBUDG-FIRST.                              
105400                                                                          
105500 K-VISA-FELBILD-SOEKNING SECTION.                                         
105600                                                                          
105700                                                                          
105800*          VID ENTER OCH IFYLLDA FÄLT PÅ INRADEN LÄGGS                    
105900*          SPARAD BILD UT MED FELTEXT "TRYCK PF11 FÖR                     
106000*          UPPDATERING"                                                   
106100                                                                          
106200                                                                          
106300     IF MID-INRAD-KDMARK-BUDG = ALL '+'                                   
106400        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-KDMARK-BUDG              
106500     ELSE                                                                 
106600        MOVE MFS-ADD-LAES-IN-FAELT                                        
106700                   TO MOD-INRAD-KDMARK-BUDG-ATTR                          
106800        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-KDMARK-BUDG              
106900     END-IF                                                               
107000                                                                          
107100                                                                          
107200     IF MID-INRAD-KDPRODSL = ALL '+'                                      
107300        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-KDPRODSL                 
107400     ELSE                                                                 
107500        MOVE MFS-ADD-LAES-IN-FAELT                                        
107600                     TO MOD-INRAD-KDPRODSL-ATTR                           
107700        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-KDPRODSL                 
107800     END-IF                                                               
107900                                                                          
108000                                                                          
108100     IF MID-INRAD-IDFKNGRP = ALL '+'                                      
108200        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-IDFKNGRP                 
108300     ELSE                                                                 
108400        MOVE MFS-ADD-LAES-IN-FAELT                                        
108500                     TO MOD-INRAD-IDFKNGRP-ATTR                           
108600        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-IDFKNGRP                 
108700     END-IF                                                               
108800                                                                          
108900                                                                          
109000     IF MID-INRAD-SUTOTFSG-BUDG = '++++++++000.00'                        
109100        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-SUTOTFSG-BUDG            
109200     ELSE                                                                 
109300        MOVE MFS-ADD-LAES-IN-FAELT                                        
109400                     TO MOD-INRAD-SUTOTFSG-BUDG-ATTR                      
109500        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-SUTOTFSG-BUDG            
109600     END-IF                                                               
109700                                                                          
109800     IF MID-INRAD-IDSKURVA = ALL '+'                                      
109900        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-IDSKURVA                 
110000     ELSE                                                                 
110100        MOVE MFS-ADD-LAES-IN-FAELT                                        
110200                   TO MOD-INRAD-IDSKURVA-ATTR                             
110300        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-IDSKURVA                 
110400     END-IF                                                               
110500                                                                          
110600     IF MID-INRAD-KDCMD = ALL '+'                                         
110700        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-KDCMD                    
110800     ELSE                                                                 
110900        MOVE MFS-ADD-LAES-IN-FAELT                                        
111000                   TO MOD-INRAD-KDCMD-ATTR                                
111100        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-KDCMD                    
111200     END-IF                                                               
111300                                                                          
111400     MOVE 1 TO RAD-IX                                                     
111500     PERFORM UNTIL RAD-IX = MAX-RAD-IX                                    
111600        MOVE MFS-ROER-EJ-FAELT                                            
111700                     TO MOD-UTRAD-KDMARK-BUDG(RAD-IX)                     
111800                        MOD-UTRAD-KDPRODSL(RAD-IX)                        
111900                        MOD-UTRAD-IDFKNGRP(RAD-IX)                        
112000                        MOD-UTRAD-SUTOTFSG-BUDG (RAD-IX)                  
112100                        MOD-UTRAD-IDSKURVA(RAD-IX)                        
112200        ADD 1 TO RAD-IX                                                   
112300     END-PERFORM.                                                         
112400 EJECT                                                                    
112500                                                                          
112600                                                                          
112700*                                                                         
112800******************************************************************        
112900* IMS SEKTIONER                                                           
113000******************************************************************        
113100*                                                                         
113200    SKIP3                                                                 
113300 IMS-GET-MSG SECTION.                                                     
113400                                                                          
113500*          HÄMTA IN BILD                                                  
113600                                                                          
113700     MOVE '  QC' TO GODK-STATUSKODER                                      
113800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
113900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114000     PERFORM IMS-STATUSKONTROLL.                                          
114100   EJECT                                                                  
114200                                                                          
114300 IMS-INSERT-MSG SECTION.                                                  
114400                                                                          
114500*          LÄGGA UT BILD PÅ SKÄRMEN                                       
114600                                                                          
114610     IF NOT ENGLISH-TEXT                                                  
114620       MOVE '0' TO MFS-KDHUVOMR                                           
114630     END-IF                                                               
114700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
114800     MOVE SPACE TO GODK-STATUSKODER                                       
114900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
115000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115100     PERFORM IMS-STATUSKONTROLL.                                          
115200    EJECT                                                                 
115300                                                                          
115400                                                                          
115500 IMS-GU-WLXXCI01-DELBUDG-ROT SECTION.                                     
115600                                                                          
115700*          SÖKA DELBUDGET ROT                                             
115800                                                                          
115900     STRING 'WLXXCI01(WDGXKEY  =' W-3135-X  ')'                           
116000            DELIMITED BY SIZE INTO SSA1                                   
116100     MOVE '  ' TO GODK-STATUSKODER                                        
116200     CALL CBLTDLI USING GU XXCI-PCB DLI-IO-AREA SSA1                      
116300     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL.                                          
116500   EJECT                                                                  
116600                                                                          
116700 IMS-GNP-WLXXCI11-DELBUDG-FIRST SECTION.                                  
116800                                                                          
116900*          SÖKA DELBUDGET FÖRSTA EFTER UPPDAT                             
117000                                                                          
117100     STRING 'WLXXCI11*F(WDGXKEY  =' W-DELBUDGET-X ')'                     
117200            DELIMITED BY SIZE INTO SSA1                                   
117300     MOVE '  GE' TO GODK-STATUSKODER                                      
117400     CALL CBLTDLI USING GNP XXCI-PCB DLI-IO-AREA SSA1                     
117500     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL.                                          
117700   EJECT                                                                  
117800                                                                          
117900                                                                          
118000 IMS-GNP-WLXXCI11-DELBUDG-NEXT SECTION.                                   
118100                                                                          
118200*          SÖKA DELBUDGET NÄSTA I LISTA/PF7                               
118300                                                                          
118400     STRING 'WLXXCI11(WDGXKEY  >' W-DELBUDGET-X ')'                       
118500            DELIMITED BY SIZE INTO SSA1                                   
118600     MOVE '  GE' TO GODK-STATUSKODER                                      
118700     CALL CBLTDLI USING GNP XXCI-PCB DLI-IO-AREA SSA1                     
118800     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
118900     PERFORM IMS-STATUSKONTROLL.                                          
119000   EJECT                                                                  
119100                                                                          
119200                                                                          
119300 IMS-GNP-WLXXCI11-DELBUDG-LIKA SECTION.                                   
119400                                                                          
119500*          SÖKA DELBUDGET MED ENTER PF8                                   
119600                                                                          
119700     STRING 'WLXXCI11(WDGXKEY =>' W-DELBUDGET-X ')'                       
119800            DELIMITED BY SIZE INTO SSA1                                   
119900     MOVE '  GE' TO GODK-STATUSKODER                                      
120000     CALL CBLTDLI USING GNP XXCI-PCB DLI-IO-AREA SSA1                     
120100     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
120200     PERFORM IMS-STATUSKONTROLL.                                          
120300   EJECT                                                                  
120400                                                                          
120500 IMS-GHNP-WLXXCI11-HAEMTA SECTION.                                        
120600                                                                          
120700*          HÄMTA FÖR UPPDATERING                                          
120800                                                                          
120900     STRING 'WLXXCI11(WDGXKEY  =' W-DELBUDGET-X ')'                       
121000            DELIMITED BY SIZE INTO SSA1                                   
121100     MOVE '  GE' TO GODK-STATUSKODER                                      
121200     CALL CBLTDLI USING GHNP XXCI-PCB DLI-IO-AREA SSA1                    
121300     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
121400     PERFORM IMS-STATUSKONTROLL.                                          
121500    EJECT                                                                 
121600                                                                          
121700                                                                          
121800 IMS-ISRT-WLXXCI11 SECTION.                                               
121900                                                                          
122000*          "INSERTA"                                                      
122100                                                                          
122200     STRING 'WLXXCI01(WDGXKEY  =' W-3135-X ')'                            
122300            DELIMITED BY SIZE INTO SSA1                                   
122400     MOVE   'WLXXCI11 ' TO SSA2                                           
122500     MOVE '  ' TO GODK-STATUSKODER                                        
122600     CALL CBLTDLI USING ISRT XXCI-PCB DLI-IO-AREA SSA1 SSA2               
122700     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
122800     PERFORM IMS-STATUSKONTROLL.                                          
122900     EJECT                                                                
123000                                                                          
123100                                                                          
123200                                                                          
123300 IMS-REPL-WLXXCI11 SECTION.                                               
123400                                                                          
123500*          "REPLACA"                                                      
123600                                                                          
123700     MOVE '  ' TO GODK-STATUSKODER                                        
123800     CALL CBLTDLI USING REPL XXCI-PCB DLI-IO-AREA                         
123900     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
124000     PERFORM IMS-STATUSKONTROLL.                                          
124100     EJECT                                                                
124200                                                                          
124300                                                                          
124400 IMS-DLET-WLXXCI11 SECTION.                                               
124500                                                                          
124600*          "DELETA"                                                       
124700                                                                          
124800     MOVE '  ' TO GODK-STATUSKODER                                        
124900     CALL CBLTDLI USING DLET XXCI-PCB DLI-IO-AREA                         
125000     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
125100     PERFORM IMS-STATUSKONTROLL.                                          
125200     EJECT                                                                
125300                                                                          
125400 IMS-GU-WLXXCH11-IDSKURVA SECTION.                                        
125500                                                                          
125600*          SÖKA IDSKURVA ROT                                              
125700                                                                          
125800     STRING 'WLXXCH01(WDGXKEY  =' W-3133-X  ')'                           
125900            DELIMITED BY SIZE INTO SSA1                                   
126000     CALL CBLTDLI USING GU XXCH-PCB DLI-IO-AREA-2 SSA1                    
126100     STRING 'WLXXCH11(WDGXKEY  =' W-IDSKURVA-X ')'                        
126200            DELIMITED BY SIZE INTO SSA2                                   
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GU XXCH-PCB DLI-IO-AREA-2 SSA1 SSA2               
126500     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL.                                          
126700   EJECT                                                                  
126800                                                                          
126900                                                                          
127000 IMS-STATUSKONTROLL SECTION.                                              
127100     SKIP2                                                                
127200     SET STATUS-IX TO 1                                                   
127300     SEARCH GODK-STATUS AT END CALL FELLOG                                
127400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127500     END-SEARCH.                                                          
127600   EJECT                                                                  
