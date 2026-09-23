000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014700.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   06/12/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ANSKAFFNINGENS LEVERANSPLANEFÖRSLAGSKÖ                           
000810*        DENNA BILD GÄLLER BARA FÖR CDC ANSKAFFNINGEN. MOTSVARANDE        
000820*        KÖBILD FÖR KINAS ANSKAFFNING ÄR 2447.                            
000830*                                                                         
000900*        PROGRAMMET LÄSER      WDD6 (LEV.PLANEBASEN)                      
001000*    INDATA.                                                              
001100*        TRANSAKTION: W2T147                                              
001200*        MID:         W2I14701                                            
001300*    UTDATA.                                                              
001400*        MOD:         W2O14701                                            
001500*                                                                         
001600*    DATA TILL 2103                                                       
001700*        PROGSW-MID:  W2I14701                                            
001710*                                                                         
001720**** ÄNDRINGAR:                                                           
001730*    2013-03-13  E'TRACKER 10143273 CHINA  LOCAL SOURCING                 
001740*                LAGT TILL IDDC PÅ WDD601.                                
001750*                                                                         
001760*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE Section.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W2014700'.            
002600 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
002601 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
002620                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 01  Filler                      PIC X(16) Value 'FELTEXT:'.              
002900 01  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000 01  Filler                      PIC X(16) Value 'INFOTEXT prev:'.        
003100 01  INFOTEXT-PREV               PIC X(80) VALUE SPACE.                   
003200 01  Filler                      PIC X(16) Value 'INFOTEXT:'.             
003300 01  INFOTEXT                    PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 01  JA                          PIC X       VALUE 'J'.                   
003600 01  NEJ                         PIC X       VALUE 'N'.                   
003700 01  OCH                         PIC X       VALUE '&'.                   
003800 01  ELLER                       PIC X       VALUE '!'.                   
003810 01  IX                          PIC 9(2)    VALUE ZERO.                  
003900*    --- INDEX FÖR VISNINGSRADER                                          
004000 01  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 01  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004200*    --- INDEX FÖR SPARADE SID-NYCKLAR (För bläddring PREVIOUS)           
004300 01  SIDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 01  MAX-SIDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004500*    --- INDEX FÖR KDLPORS på WDD6                                        
004600 01  OIX                         PIC s9  VALUE Zero COMP-3.               
004700*    --- INDEX FÖR Orsaks-TEXT till skärm                                 
004800 01  TIX                         PIC s9  VALUE Zero COMP-3.               
004900*    --- Giltigt KDLPORS-nummer i W221W005                                
005000 01  KDLPORS-NUM                 PIC s999 VALUE Zero COMP-3.              
005100                                                                          
005200                                                                          
005300*    --- ARBETSFÄLT FÖR VÄRDEN FRÅN SKÄRMEN                               
005400                                                                          
005500 01  FILLER                      PIC X(11)   VALUE 'NYCKLAR-OK='.         
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 01  FILLER                      PIC X(12)   VALUE 'KDCMDVAL-OK='.        
006100 77  KDCMDVAL-SW                 PIC X       VALUE 'J'.                   
006200     88  KDCMDVAL-OK                         VALUE 'J'.                   
006300     88  NO-KDCMDVAL                         VALUE 'N'.                   
006400                                                                          
006500*    --- ARBETSFÄLT FÖR SELEKTION AV DATABAS-POSTER                       
006600 01  FILLER                      PIC X(10)   VALUE 'GODK-POST='.          
006700 77  GODK-POST-SW                PIC X       VALUE 'J'.                   
006800     88  GODK-POST                           VALUE 'J'.                   
006900     88  EJ-GODK-POST                        VALUE 'N'.                   
007000                                                                          
007100 01  FILLER                      PIC X(12)   VALUE 'SET-KVRADER='.        
007200 77  SET-KVRADER-SW              PIC X       VALUE 'J'.                   
007300     88  SET-KVRADER                         VALUE 'J'.                   
007400     88  SET-EJ-KVRADER                      VALUE 'N'.                   
007500                                                                          
007600 01  FILLER                      PIC X(10)   VALUE 'W-IDTRANS='.          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  2103-MID                            VALUE '2103'.                
007900     88  EGEN-MID                            VALUE '2147'.                
008000     88  GODK-MID                            VALUE 'XXXX'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200     EJECT                                                                
008201*01  -COPY WWDCKONS                                                       
008210     EJECT                                                                
008300                                                                          
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     EJECT                                                                
009200                                                                          
009300 01  GENERELL-RETURKOD.                                                   
009400     03  RKOD                    PIC S9(4)   VALUE ZERO BINARY.           
009500                                                                          
009600*    --- Hjälpvariabler och Arbetsfält                                    
009700 01  Filler                      Pic X(8) VALUE 'WS-AREA'.                
009800 01  WS-AREA.                                                             
009900     03  WS-IDANSK-TO-UT             Pic 9(3).                            
010000     03  Filler Redefines WS-IDANSK-TO-UT.                                
010100         05 WS-IDANSK-TO-UT-2        PIC 9(2).                            
010200         05 WS-IDANSK-TO-UT-3        PIC 9.                               
010300                                                                          
010400     03  IMS-SSA-TYP                 Pic 9 Value 9.                       
010500                                                                          
010800                                                                          
010900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011000*01 -COPY WMEDAREA                                                        
011100     SKIP3                                                                
011200 01  MESSAGE-CODES.                                                       
011300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011400     03  INF-NO-LINES-EXISTS     PIC X(3)    VALUE '056'.                 
011500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011600     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '106'.                 
011700     03  INF-PRESS-F9-TO-JUMP    PIC X(3)    VALUE '127'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     03  ERR-WRONG-CODE          PIC X(3)    VALUE '013'.                 
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012400                                                                          
012500*01 -COPY WMSGINIT                                                        
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                  PIC X(16)   VALUE 'SPAR-AREA'.               
012900                                                                          
013000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
013010*                                                                         
013100*01 -COPY WW20147S.                                                       
013300                                                                          
013400     EJECT                                                                
013500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013800     SKIP3                                                                
013900*01  MID -COPY W2I14701                                                   
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014200     SKIP3                                                                
014300*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014500     03  MOD REDEFINES MSG-AREA.                                          
014600*      05  -COPY W2O14701                                                 
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)  VALUE 'KDLPORS-AREA'.         
014900*01   -COPY W221W005.                                                     
015000     EJECT                                                                
015100*                                                                         
015200 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
015300*    -- P-WS-LL  sätts i A-init.                                          
015400 01  W-PROG-TO-PROG-SW.                                                   
015500     03  P-WS-LL     PIC S9(4)  COMP SYNC.                                
015600     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
015700     03  KDTRANS-WS  PIC X(8)   VALUE 'W2T103  '.                         
015800     03  P-IDTRANS   PIC X(4)   VALUE '2147'.                             
015900     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
016000                                                                          
016100*    03  MID -COPY W2I14701 -PRE PROGSW-.                                 
016200     EJECT                                                                
016300*                                                                         
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016600     SKIP3                                                                
016700*01  -COPY WMFSAREA                                                       
016800     EJECT                                                                
016900                                                                          
017000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500*    --- MIN-VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN        
017600     03  W-WDD601KY-MIN-X.                                                
017700            05 W-IDDC-MIN      PIC X(2)  Value Space.                     
017710            05 W-IDLEVNR-MIN   PIC X(5)  Value Space.                     
017800            05 W-IDARTNR-MIN   PIC S9(9) Value Zero COMP-3.               
017910            05 W-IDANSK-MIN-X.                                            
018000              07 W-IDANSK-MIN  PIC S9(3)        COMP-3.                   
018100                                                                          
018200*    --- MAX-VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN        
018300     03  W-WDD601KY-MAX-X.                                                
018310            05 W-IDDC-MAX      PIC X(2)  Value '99'.                      
018400            05 W-IDLEVNR-MAX   PIC X(5)  Value High-Value.                
018500            05 W-IDARTNR-MAX   PIC S9(9) Value +999999999 COMP-3.         
018600            05 W-IDANSK-MAX-X.                                            
018700              07 W-IDANSK-MAX  PIC S9(3)        COMP-3.                   
018800                                                                          
018880                                                                          
018900     03  W-KDLPORS-X.                                                     
019000         05  W-KDLPORS         PIC S9(3) Value Zero COMP-3.               
019100                                                                          
019200     03  W-KDLEVPLF-X.                                                    
019300         05  W-KDLEVPLF        PIC X(1)  Value Space.                     
019400                                                                          
019410     03  W-IDARTNR-X.                                                     
019420         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
019500                                                                          
019600     SKIP2                                                                
019700*    --- STATUS-KOD FRÅN IMS                                              
019800 01  STATUS-WS                   PIC XX.                                  
019900     88  SEGMENT-FINNS                       VALUE '  '.                  
020000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020300     SKIP2                                                                
020400 01  GODK-STATUSKODER.                                                    
020500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020600     SKIP3                                                                
020700 01  SSA1                        PIC X(255).                              
020800 01  SSA2                        PIC X(255).                              
020900     EJECT                                                                
021000*    --- IMS FUNKTIONSKODER                                               
021100*01  -COPY W0003                                                          
021200     EJECT                                                                
021300*    ---  DLI INPUT-OUTPUT AREA                                           
021400                                                                          
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
021600 01  DLI-IO-WDD601.                                                       
021700*    03  -COPY WDD601                                                     
021800     EJECT                                                                
021810 01  FILLER                  PIC X(16)  VALUE 'WDD3-IO-AREA'.             
021820     SKIP3                                                                
021830 01  WDD3-IO-AREA.                                                        
021840     SKIP2                                                                
021850*    03  -COPY WDD311                                                     
021860     EJECT                                                                
021900 LINKAGE Section.                                                         
022000*01  -COPY W0009   -PRE MSG-                                              
022100*01  -COPY W0008     -PRE ALT-                                            
022200         05  FILLER              PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008   -PRE WDP7-                                             
022500     05  FILLER                  PIC X.                                   
022600                                                                          
022700*01  -COPY W0008  -PRE WDD6-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000                                                                          
023010*01  -COPY W0008  -PRE WDD3-                                              
023020     05  FILLER                  PIC X.                                   
023030     EJECT                                                                
023040                                                                          
023100 PROCEDURE DIVISION  Using MSG-PCB ALT-PCB WDP7-PCB WDD6-PCB              
023110                           WDD3-PCB.                                      
023200 MAIN Section.                                                            
023300     Entry 'DLITCBL' Using MSG-PCB ALT-PCB WDP7-PCB WDD6-PCB              
023310                           WDD3-PCB.                                      
023400     Perform IMS-GET-MSG                                                  
023500     If SEGMENT-FINNS                                                     
023600                                                                          
023700       Perform A-INITIERA                                                 
023800                                                                          
023900       Perform B-ORDNA-KONTROLLERA-NYCKLAR                                
024000                                                                          
024100       If NYCKLAR-OK                                                      
024200          Evaluate True                                                   
024300             When  MFS-FIRST      Perform C-FOERSTA-SIDA                  
024400             When  MFS-NEXT       Perform D-NAESTA-SIDA                   
024500             When  MFS-ENTER      Perform E-SAMMA-SIDA                    
024600             When  MFS-PREVIOUS   Perform F-FOEREG-SIDA                   
024700             When  MFS-SPLIT      Perform HOPP-TILL-2103                  
024800             When  Other    Continue                                      
024900          End-Evaluate                                                    
025000                                                                          
025100          If MFS-SPLIT                                                    
025200             If KDCMDVAL-OK                                               
025300                  Move '002'       To MSGI-KDCALL                         
025400                  Move '2147'      To SPAR-IDTRANS                        
025500                  Move SPAR-AREA   To MSGI-SPAR-AREA                      
025600                  Call W005INIT Using MSGI-WMSGINIT WDP7-PCB              
025700                Move MID-W2I14701 To PROGSW-MID-W2I14701                  
025800                Compute P-WS-LL = Length Of PROGSW-MID + 17               
025900                Perform IMS-INSERT-ALT-MSG                                
026000             End-If                                                       
026100          Else                                                            
026200             Perform G-LAES-VISA-INFO                                     
026300          End-If                                                          
026400       End-If                                                             
026500                                                                          
026600       If Not MFS-SPLIT                                                   
026700       Or ( MFS-SPLIT And NO-KDCMDVAL )                                   
026800          Compute MSG-KVLL = Length Of MOD-W2O14701 + 4                   
026900          Perform IMS-INSERT-MSG                                          
027000       End-If                                                             
027100     End-If                                                               
027200     Move Zero To Return-Code                                             
027300     Goback                                                               
027400     .                                                                    
027500     EJECT                                                                
027600                                                                          
027700 A-INITIERA Section.                                                      
027710     MOVE 'A-INITIERA '  TO CURRENT-SECTION                               
027800     SKIP2                                                                
027900     If MSG-DUBBLA-TRANSKODER                                             
028000       Move MSG-INDATA-MINUS-2-TRANSKODER To MID-W2I14701                 
028100       Move MSG-IDTRANS-2  To MFS-IDTRANS                                 
028200       Move MSG-KDMFSFOR-2 To MFS-KDMFSFOR                                
028300     Else                                                                 
028400       Move MSG-INDATA-MINUS-1-TRANSKOD  To MID-W2I14701                  
028500       Move MSG-IDTRANS-1  To MFS-IDTRANS                                 
028600       Move MSG-KDMFSFOR-1 To MFS-KDMFSFOR                                
028700     End-If                                                               
028800                                                                          
028900     Move MSG-KDTRTYP To MFS-KDTRTYP                                      
029000     Move MSG-IDPFK   To MFS-IDPFK                                        
029100     Move MFS-IDTRANS To W-IDTRANS                                        
029200                                                                          
029300     Move ALL '+'           To MSGI-WMSGINIT                              
029400     Move '001'             To MSGI-KDCALL                                
029500     Move MSG-SIGNON-USERID To MSGI-IDUSER                                
029600     Move MSG-LTERM-NAME    To MSGI-IDLTERM-USER                          
029700     Move '2147'            To MSGI-IDTRANS                               
029800                                                                          
029900                                                                          
030000     Move Low-Value To MSG-AREA                                           
030100     Move 'W2O147N1' To MFS-IDMOD                                         
030200     Move '2147' To MOD-IDTRANS                                           
030300     Move MFS-RENSA-FAELT To MOD-TEMFSFEL MOD-TEMFSINF                    
030400                                                                          
030500     If EGEN-MID Or HELP-MID                                              
030600       Continue                                                           
030700     Else                                                                 
030800       Move Space   To MFS-KDTRTYP                                        
030900       Move '7'     To MFS-IDPFK                                          
031000     End-If                                                               
031100                                                                          
031500     Move 'GB '            To MED-IDSKYLT                                 
031700                                                                          
031710     MOVE WC-CDC-SE        TO W-IDDC-MIN                                  
031720                              W-IDDC-MAX                                  
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 B-ORDNA-KONTROLLERA-NYCKLAR Section.                                     
032110     MOVE 'B-ORDNA-KONTROLLERA-NYCKLAR '  TO CURRENT-SECTION              
032200     SKIP2                                                                
032300     Move JA To NYCKLAR-SW                                                
032400                                                                          
032500     If EGEN-MID                                                          
032600       Move MID-IDANSK-IN   To MSGI-IDANSK                                
032700       Move MID-IDLEVNR-IN  To MSGI-IDLEVNR                               
032800     Else                                                                 
032900       Move All '+'         To MSGI-IDANSK                                
033000                               MSGI-IDLEVNR                               
033100                               MID-IDANSK-IN                              
033200                               MID-IDLEVNR-IN                             
033300     End-If                                                               
033400*    --- Hämtar USER-SPARAREA  KDCALL = 001                               
033500     Call W005INIT Using MSGI-WMSGINIT WDP7-PCB                           
033600                                                                          
033710     Move MSGI-SPAR-AREA    To SPAR-AREA                                  
033800     If SPAR-IDTRANS = '2147'                                             
033810       CONTINUE                                                           
034000     Else                                                                 
034100       Move Space           To SPAR-AREA                                  
034200       Move Zero            To                                            
034300            SPAR-SIDNR                 SPAR-KVRADER                       
034400            SPAR-KEY-IDARTNR-ENTER     SPAR-KEY-IDANSK-ENTER              
034500            SPAR-KEY-IDARTNR-NEXT      SPAR-KEY-IDANSK-NEXT               
034600            SPAR-KEY-IDARTNR-PREV (1)  SPAR-KEY-IDANSK-PREV (1)           
034700            SPAR-KEY-IDARTNR-PREV (2)  SPAR-KEY-IDANSK-PREV (2)           
034800            SPAR-KEY-IDARTNR-PREV (3)  SPAR-KEY-IDANSK-PREV (3)           
034900            SPAR-KEY-IDARTNR-PREV (4)  SPAR-KEY-IDANSK-PREV (4)           
035000            SPAR-KEY-IDARTNR-PREV (5)  SPAR-KEY-IDANSK-PREV (5)           
035100            SPAR-KEY-IDARTNR-PREV (6)  SPAR-KEY-IDANSK-PREV (6)           
035200            SPAR-KEY-IDARTNR-PREV (7)  SPAR-KEY-IDANSK-PREV (7)           
035300            SPAR-KEY-IDARTNR-PREV (8)  SPAR-KEY-IDANSK-PREV (8)           
035400            SPAR-KEY-IDARTNR-PREV (9)  SPAR-KEY-IDANSK-PREV (9)           
035500            SPAR-KEY-IDARTNR-PREV (10) SPAR-KEY-IDANSK-PREV (10)          
035600            SPAR-KEY-IDANSK-TO         SPAR-KEY-IDANSK-FROM               
035700            SPAR-KEY-KDLPORS                                              
035701                                                                          
035710       MOVE WC-CDC-SE               TO SPAR-KEY-IDDC-ENTER                
035712                                       SPAR-KEY-IDDC-NEXT                 
035713       MOVE +1  TO IX                                                     
035714       PERFORM UNTIL IX > 10                                              
035720         MOVE WC-CDC-SE             TO SPAR-KEY-IDDC-PREV (IX)            
035730         ADD +1 TO IX                                                     
035740       END-PERFORM                                                        
035800     End-if                                                               
035900                                                                          
035910                                                                          
035911     If SPAR-SIDNR Not Numeric                                            
035920     OR SPAR-SIDNR = ZERO                                                 
035930       Move +1    To SPAR-SIDNR                                           
035940     End-If                                                               
035950                                                                          
036000     If SPAR-KEY-KDLPORS  Not Numeric                                     
036100       Move Zero  To SPAR-KEY-KDLPORS                                     
036200     End-If                                                               
036300                                                                          
036400     If SPAR-KEY-KDLEVPLF Not =                                           
036500       'Y' And                                                            
036510       'J' And                                                            
036600       'N' And                                                            
036700       'S' And                                                            
036800       'G' And                                                            
036900       'P' And                                                            
037000       ' '                                                                
037100       Move Space To SPAR-KEY-KDLEVPLF                                    
037200     End-If                                                               
037300                                                                          
037400     If SPAR-KVRADER      Not Numeric                                     
037500       Move Zero  To SPAR-KVRADER                                         
037600     End-If                                                               
037700                                                                          
037800*    |                                                                    
037900*    -- Kontroll av obligatorisk nyckel                                   
038000*    |                                                                    
038100     Move MFS-RENSA-FAELT To MOD-IDANSK-IN                                
038200                                                                          
038300     IF MID-IDANSK-IN  Not = ALL '+'                                      
038400       Move '7'         To MFS-IDPFK                                      
038500       Move Space       To MFS-KDTRTYP                                    
038600*      --- Ny huvudnyckel ger första sidan                                
038700*      --- Ny huvudnyckel ger OCKSÅ total rader för urvalet               
038800*      --- Görs i G-LAES                                                  
038900*      --- Man skall här också nollställa övriga spar-nycklar             
039000       Move Zero        To SPAR-KEY-IDANSK-TO                             
039100                           SPAR-KEY-KDLPORS                               
039200                           SPAR-KVRADER                                   
039300       Move Space       To SPAR-KEY-IDLEVNR                               
039400                           SPAR-KEY-KDLEVPLF                              
039500     End-If                                                               
039600                                                                          
039700*    --- Ny eller tidigare obligatorisk nyckel                            
039800     If NYCKLAR-OK                                                        
039900       If MSGI-IDANSK Not Numeric                                         
040000         Move MFS-NUM-FAELT-FEL To MOD-IDANSK-IN-ATTR                     
040100         Move ERR-WRONG-KEY    To MED-IDMFSFEL                            
040200         Move NEJ To NYCKLAR-SW                                           
040300       Else                                                               
040400         Move MSGI-IDANSK    To MOD-IDANSK-UT                             
040500                                W-IDANSK-MIN                              
040600                                SPAR-KEY-IDANSK-FROM                      
040700       End-If                                                             
040800     Else                                                                 
040900       Move MFS-RENSA-FAELT To MOD-IDANSK-UT                              
041000     End-If                                                               
041100*    |                                                                    
041200*    +-- Kontroll av begränsningsnycklarna                                
041300*    |                                                                    
041400     If NYCKLAR-OK                                                        
041500       If MID-IDANSK-TO-IN = All '+' Or SPACE                             
041600         If SPAR-KEY-IDANSK-TO Not Numeric                                
041700           Move Zero To SPAR-KEY-IDANSK-TO                                
041800         End-If                                                           
041900       Else                                                               
042000         If MID-IDANSK-TO-IN Numeric                                      
042100           Move MID-IDANSK-TO-IN To SPAR-KEY-IDANSK-TO                    
042200*          --- Ny Begr.nyckel ger första sidan                            
042300*          --- Ny begr.nyckel ger OCKSÅ ny utr. av KVRADER                
042400*          --- Görs i G-LAES                                              
042500           Move '7'       To MFS-IDPFK                                    
042600           Move Space     To MFS-KDTRTYP                                  
042700           Move Zero      To SPAR-KVRADER                                 
042800         Else                                                             
042900           Move Zero To SPAR-KEY-IDANSK-TO                                
043000           If EGEN-MID                                                    
043100*            --- bara felmeddelande om inmatat på egen mid                
043200             Move MFS-NUM-FAELT-FEL To MOD-IDANSK-TO-IN-ATTR              
043300             Move ERR-WRONG-KEY To MED-IDMFSFEL                           
043400             Move NEJ To NYCKLAR-SW                                       
043500           End-If                                                         
043600         End-If                                                           
043700       End-If                                                             
043800                                                                          
043900       If SPAR-KEY-IDANSK-TO = Zero                                       
044000*        -- Normalt maxvärde  (IDANSK med slutsiffra = 9)                 
044100         Move MSGI-IDANSK      To WS-IDANSK-TO-UT                         
044200         If  WS-IDANSK-TO-UT > Zero                                       
044300         And WS-IDANSK-TO-UT-3 = Zero                                     
044400           Move 9                To WS-IDANSK-TO-UT-3                     
044500         End-If                                                           
044600         Move WS-IDANSK-TO-UT    To SPAR-KEY-IDANSK-TO                    
044700       End-if                                                             
044800                                                                          
044900       Move SPAR-KEY-IDANSK-TO To W-IDANSK-MAX                            
045000                                  MOD-IDANSK-TO-UT                        
045100     End-If                                                               
045200                                                                          
045300*    |                                                                    
045400*    +--- Bered IDLEVNR begränsning (primär nyckel i WDD601KY )           
045500*    |                                                                    
045600     Move MFS-RENSA-FAELT   To MOD-IDLEVNR-UT                             
045700     If NYCKLAR-OK                                                        
045800       Move Low-Values        To W-IDLEVNR-MIN                            
045900       Move High-Values       To W-IDLEVNR-MAX                            
046000                                                                          
046100       If  EGEN-MID Or HELP-MID                                           
046200         If MID-IDLEVNR-IN = All '+'                                      
046300           If SPAR-IDTRANS = '2147'                                       
046400           And SPAR-KEY-IDLEVNR > Space                                   
046500             Move SPAR-KEY-IDLEVNR To W-IDLEVNR-MIN                       
046600                                      W-IDLEVNR-MAX                       
046700                                      MOD-IDLEVNR-UT                      
046800           End-If                                                         
046900         Else                                                             
047000*          --- Ny Begr.nyckel IDLEVNR ger första sidan                    
047100           Move '7'     To MFS-IDPFK                                      
047200           Move Space   To MFS-KDTRTYP                                    
047300           Move Zero    To SPAR-KVRADER                                   
047400*          --- Ny begr.nyckel ger OCKSÅ ny uträkning av KVRADER           
047500*          --- Görs i G-LAES                                              
047600           If MID-IDLEVNR-IN = Space Or '0    '                           
047700*            -- Man vill återställa till normal funktion                  
047800             Move Space      To SPAR-KEY-IDLEVNR                          
047900           Else                                                           
048000             Move MID-IDLEVNR-IN To W-IDLEVNR-MIN                         
048100                                    W-IDLEVNR-MAX                         
048200                                    SPAR-KEY-IDLEVNR                      
048300                                    MOD-IDLEVNR-UT                        
048400           End-If                                                         
048500         End-If                                                           
048600       Else                                                               
048700           If SPAR-IDTRANS = '2147'                                       
048800           And SPAR-KEY-IDLEVNR > Space                                   
048900             Move SPAR-KEY-IDLEVNR To W-IDLEVNR-MIN                       
049000                                      W-IDLEVNR-MAX                       
049100                                      MOD-IDLEVNR-UT                      
049200           End-If                                                         
049300       End-If                                                             
049400     End-If                                                               
049500*    |                                                                    
049600*    +--- Bered för KDLPORS begränsning                                   
049700*    |                                                                    
049800     Move MFS-RENSA-FAELT   To MOD-KDLPORS-UT                             
049900     If NYCKLAR-OK                                                        
050000       Move Zero              To W-KDLPORS                                
050100                                                                          
050200       If EGEN-MID                                                        
050300         If MID-KDLPORS-IN = All '+'                                      
050400           If SPAR-IDTRANS = '2147'                                       
050500           And SPAR-KEY-KDLPORS Not = Zero                                
050600             Move SPAR-KEY-KDLPORS To W-KDLPORS                           
050700                                      MOD-KDLPORS-UT                      
050800           End-If                                                         
050900         Else                                                             
051000           If MID-KDLPORS-IN = Space Or '0 ' Or Zero                      
051100           Or MID-KDLPORS-IN Numeric                                      
051200*            --- Ny Begr.nyckel ger första sidan                          
051300             Move '7'   To MFS-IDPFK                                      
051400             Move Space To MFS-KDTRTYP                                    
051500             Move Zero  To SPAR-KVRADER                                   
051600*            --- Ny begr.nyckel ger OCKSÅ ny utr. av KVRADER              
051700*            --- Görs i G-LAES                                            
051800             If MID-KDLPORS-IN = Space Or '0 ' Or Zero                    
051900*              -- Man vill återställa till normal funktion                
052000               Move Zero To SPAR-KEY-KDLPORS                              
052100             Else                                                         
052200               Move MID-KDLPORS-IN To W-KDLPORS                           
052300                                      SPAR-KEY-KDLPORS                    
052400                                      MOD-KDLPORS-UT                      
052500             End-If                                                       
052600           Else                                                           
052700             Move MFS-NUM-FAELT-FEL To MOD-KDLPORS-IN-ATTR                
052800             Move ERR-WRONG-CODE To MED-IDMFSFEL                          
052900             Move NEJ To NYCKLAR-SW                                       
053000           End-If                                                         
053100         End-If                                                           
053200       Else                                                               
053300         If SPAR-IDTRANS = '2147'                                         
053400         And SPAR-KEY-KDLPORS Not = Zero                                  
053500           Move SPAR-KEY-KDLPORS To W-KDLPORS                             
053600                                    MOD-KDLPORS-UT                        
053700         Else                                                             
053800           Move Zero             To W-KDLPORS                             
053900                                    MOD-KDLPORS-UT                        
054000         End-If                                                           
054100       End-If                                                             
054200     End-If                                                               
054300*    |                                                                    
054400*    +--- Bered KDLEVPLF begränsning                                      
054500*    |                                                                    
054600     Move MFS-RENSA-FAELT   To MOD-KDLEVPLF-UT                            
054700     If NYCKLAR-OK                                                        
054800       Move Space             To W-KDLEVPLF                               
054900                                                                          
055000       If  EGEN-MID                                                       
055100         If MID-KDLEVPLF-IN = All '+'                                     
055200           If SPAR-IDTRANS = '2147'                                       
055300           And SPAR-KEY-KDLEVPLF Not = Space                              
055400             Move SPAR-KEY-KDLEVPLF To W-KDLEVPLF                         
055500                                       MOD-KDLEVPLF-UT                    
055600           End-If                                                         
055700         Else                                                             
055800           If MID-KDLEVPLF-IN =                                           
055900                  'J' Or 'N' Or 'S' Or 'G' Or 'P' Or ' ' Or 'Y'           
056000*            --- Ny Begr.nyckel ger första sidan                          
056100             Move '7' To MFS-IDPFK                                        
056200             Move Space To MFS-KDTRTYP                                    
056300             Move Zero To SPAR-KVRADER                                    
056400*            --- Ny begr.nyckel ger OCKSÅ ny utr. av KVRADER              
056500*            --- Görs i G-LAES                                            
056600             If MID-KDLEVPLF-IN = Space                                   
056700*              -- Man vill återställa till normal funktion                
056800               Move Space      To SPAR-KEY-KDLEVPLF                       
056900             Else                                                         
057000               Move MID-KDLEVPLF-IN To W-KDLEVPLF                         
057100                                       SPAR-KEY-KDLEVPLF                  
057200                                       MOD-KDLEVPLF-UT                    
057210               IF MID-KDLEVPLF-IN = 'J'                                   
057220                 MOVE 'Y'           TO MOD-KDLEVPLF-UT                    
057230               END-IF                                                     
057300             End-If                                                       
057400           Else                                                           
057500             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDLEVPLF-IN-ATTR              
057600             Move ERR-WRONG-CODE To MED-IDMFSFEL                          
057700             Move NEJ To NYCKLAR-SW                                       
057800           End-If                                                         
057900         End-If                                                           
058000       Else                                                               
058100         If SPAR-IDTRANS = '2147'                                         
058200         And SPAR-KEY-KDLEVPLF Not = Space                                
058300           Move SPAR-KEY-KDLEVPLF To W-KDLEVPLF                           
058400                                     MOD-KDLEVPLF-UT                      
058410           IF SPAR-KEY-KDLEVPLF = 'J'                                     
058420             MOVE 'Y'               TO MOD-KDLEVPLF-UT                    
058430           END-IF                                                         
058500         Else                                                             
058600           Move Space      To W-KDLEVPLF                                  
058700                              MOD-KDLEVPLF-UT                             
058800         End-If                                                           
058900       End-If                                                             
059000     End-If                                                               
059100                                                                          
059200     If NYCKLAR-FEL                                                       
059300       Call WMEDKONV Using MED-WMEDAREA                                   
059400       Move MED-MFSFEL To MOD-TEMFSFEL                                    
059500       Perform MFS-RENSA-FAELT-IN                                         
059600       Perform MFS-RENSA-FAELT-UT                                         
059700     End-If                                                               
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 C-FOERSTA-SIDA Section.                                                  
060110     MOVE 'C-FOERSTA-SIDA '  TO CURRENT-SECTION                           
060200     SKIP2                                                                
060300*    -- Minst ett nytt nyckelfält, eller PF7 tryckt.                      
060400     Move INF-FIRST-PAGE To MED-IDMFSINF                                  
060500     Call WMEDKONV Using MED-WMEDAREA                                     
060600     Move MED-MFSINF To MOD-TEMFSFEL                                      
060700     Perform MFS-RENSA-FAELT-IN                                           
060800                                                                          
060900     Perform S02-INIT-PREVIOUS                                            
061000     Move +1  To SPAR-SIDNR                                               
061100     Move NEJ To SPAR-SISTA-SIDAN                                         
061200     Initialize SPAR-KEY-WDD601KY-NEXT                                    
061210     MOVE WC-CDC-SE  TO SPAR-KEY-IDDC-NEXT                                
061220                                                                          
061300     .                                                                    
061400     EJECT                                                                
061500                                                                          
061600 D-NAESTA-SIDA Section.                                                   
061610     MOVE 'D-NAESTA-SIDA '   TO CURRENT-SECTION                           
061700     SKIP2                                                                
061800*    -- PF8 tryckt.                                                       
061900     If SPAR-IDTRANS = '2147'                                             
062000*      -- Läs senast visade sidans nästa ovisade artikel                  
062100*      -- ... eller samma sida ifall alla visades senast.                 
062200                                                                          
062300       If SPAR-SISTA-SIDAN = NEJ                                          
062400         Move SPAR-KEY-WDD601KY-NEXT To W-WDD601KY-MIN-X                  
062500*        -- Spara senast visade sidans Start-nyckel till PREV             
062600         Move SPAR-SIDNR To SIDX                                          
062700                                                                          
062800         If SPAR-SIDNR = +10                                              
062900           Perform DA-FLYTTA-PREVIOUS                                     
063000         Else                                                             
063100           Add +1 To SPAR-SIDNR                                           
063200         End-If                                                           
063300                                                                          
063400         Move SPAR-KEY-WDD601KY-ENTER                                     
063500                           To SPAR-KEY-WDD601KY-PREV (SIDX)               
063600       Else                                                               
063700         Move SPAR-KEY-WDD601KY-ENTER To W-WDD601KY-MIN-X                 
063800         Move MSGI-IDANSK             To W-IDANSK-MIN                     
063900       End-If                                                             
064000                                                                          
064100     Else                                                                 
064200       Perform MFS-RENSA-FAELT-IN                                         
064300     End-If                                                               
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
064700 DA-FLYTTA-PREVIOUS          Section.                                     
064710     MOVE 'DA-FLYTTA-PREVIOUS '    TO CURRENT-SECTION                     
064800     SKIP2                                                                
064900*    -- Det finns bara 10 sidor i PREVIOUS-nycklarna                      
065000*    -- Flytta alla visade sidor ETT steg bakåt i tabellen.               
065100     Move +2  To SIDX                                                     
065200     Perform until SIDX > +10                                             
065300       Move SPAR-KEY-WDD601KY-PREV (SIDX)                                 
065400                            To SPAR-KEY-WDD601KY-PREV (SIDX - 1)          
065500       Add +1 To SIDX                                                     
065600     End-Perform                                                          
065700                                                                          
065800*    -- SPAR-SIDNR är fortfarande 10, återställ SIDX                      
065900     Move SPAR-SIDNR        To SIDX                                       
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300 E-SAMMA-SIDA Section.                                                    
066310     MOVE 'E-SAMMA-SIDA '   TO CURRENT-SECTION                            
066400     SKIP2                                                                
066500*    -- ENTER tryckt.                                                     
066600     If SPAR-IDTRANS = '2147' Or '0551'                                   
066700                                                                          
066800       Move SPAR-KEY-WDD601KY-ENTER To W-WDD601KY-MIN-X                   
066900       Move MSGI-IDANSK             To W-IDANSK-MIN                       
067000                                                                          
067100       If MID-IDANSK-IN     = All '+'                                     
067200       And MID-IDANSK-TO-IN = All '+'                                     
067300       And MID-IDLEVNR-IN   = All '+'                                     
067400       And MID-KDLPORS-IN   = All '+'                                     
067500       And MID-KDLEVPLF-IN  = All '+'                                     
067600       And MID-KDCMDVAL(1)  = '+' And MID-KDCMDVAL(2) = '+'               
067700       And MID-KDCMDVAL(3)  = '+' And MID-KDCMDVAL(4) = '+'               
067800       And MID-KDCMDVAL(5)  = '+' And MID-KDCMDVAL(6) = '+'               
067900       And MID-KDCMDVAL(7)  = '+' And MID-KDCMDVAL(8) = '+'               
068000       And MID-KDCMDVAL(9)  = '+' And MID-KDCMDVAL(10) = '+'              
068100       And MID-KDCMDVAL(11) = '+' And MID-KDCMDVAL(12) = '+'              
068200       And MID-KDCMDVAL(13) = '+' And MID-KDCMDVAL(14) = '+'              
068300       And MID-KDCMDVAL(15) = '+'                                         
068400         Perform MFS-RENSA-FAELT-IN                                       
068500       Else                                                               
068600         If ( MID-IDANSK-IN   = All '+'                                   
068700         And MID-IDANSK-TO-IN = All '+'                                   
068800         And MID-IDLEVNR-IN   = All '+'                                   
068900         And MID-KDLPORS-IN   = All '+'                                   
069000         And MID-KDLEVPLF-IN  = All '+' )                                 
069100                                                                          
069200         And ((MID-KDCMDVAL(1) Not = '+') Or                              
069300              (MID-KDCMDVAL(2) Not = '+') Or                              
069400              (MID-KDCMDVAL(3) Not = '+') Or                              
069500              (MID-KDCMDVAL(4) Not = '+') Or                              
069600              (MID-KDCMDVAL(5) Not = '+') Or                              
069700              (MID-KDCMDVAL(6) Not = '+') Or                              
069800              (MID-KDCMDVAL(7) Not = '+') Or                              
069900              (MID-KDCMDVAL(8) Not = '+') Or                              
070000              (MID-KDCMDVAL(9) Not = '+') Or                              
070100              (MID-KDCMDVAL(10) Not = '+') Or                             
070200              (MID-KDCMDVAL(11) Not = '+') Or                             
070300              (MID-KDCMDVAL(12) Not = '+') Or                             
070400              (MID-KDCMDVAL(13) Not = '+') Or                             
070500              (MID-KDCMDVAL(14) Not = '+') Or                             
070600              (MID-KDCMDVAL(15) Not = '+') )                              
070700                                                                          
070800           Move INF-PRESS-F9-TO-JUMP To MED-IDMFSINF                      
070900           Call WMEDKONV          Using MED-WMEDAREA                      
071000           Move MED-MFSINF           To MOD-TEMFSFEL                      
071100                                                                          
071200           Perform EA-MID-INDATA-TILL-MOD                                 
071300         End-If                                                           
071400       End-If                                                             
071500     Else                                                                 
071600       Perform MFS-RENSA-FAELT-IN                                         
071700     End-If                                                               
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072100 EA-MID-INDATA-TILL-MOD Section.                                          
072110     MOVE 'EA-MID-INDATA-TILL-MOD '    TO CURRENT-SECTION                 
072200     SKIP2                                                                
072300*    -- för varje mid-fält                                                
072400     If MID-IDANSK-IN = All '+'                                           
072500*      --            flytta rensa-fält till mod-indata-fält               
072600       Move MFS-RENSA-FAELT       To MOD-IDANSK-IN                        
072700     Else                                                                 
072800*      -- om mid-fält Not = all '+' flytta mid-fält till mod-indat        
072900*      --        flytta mfs-add-laes-in-faelt till mod-indata-attr        
073000       Move MID-IDANSK-IN         To MOD-IDANSK-IN                        
073100       Move MFS-ADD-LAES-IN-FAELT To MOD-IDANSK-IN-ATTR                   
073200     End-If                                                               
073300                                                                          
073400     If MID-IDANSK-TO-IN = All '+'                                        
073500       Move MFS-RENSA-FAELT       To MOD-IDANSK-TO-IN                     
073600     Else                                                                 
073700       Move MID-IDANSK-TO-IN      To MOD-IDANSK-TO-IN                     
073800       Move MFS-ADD-LAES-IN-FAELT To MOD-IDANSK-TO-IN-ATTR                
073900     End-If                                                               
074000                                                                          
074100     If MID-IDLEVNR-IN  = All '+'                                         
074200       Move MFS-RENSA-FAELT       To MOD-IDLEVNR-IN                       
074300     Else                                                                 
074400       Move MID-IDLEVNR-IN        To MOD-IDLEVNR-IN                       
074500       Move MFS-ADD-LAES-IN-FAELT To MOD-IDLEVNR-IN-ATTR                  
074600     End-If                                                               
074700                                                                          
074800     If MID-KDLPORS-IN = All '+'                                          
074900       Move MFS-RENSA-FAELT       To MOD-KDLPORS-IN                       
075000     Else                                                                 
075100       Move MID-KDLPORS-IN        To MOD-KDLPORS-IN                       
075200       Move MFS-ADD-LAES-IN-FAELT To MOD-KDLPORS-IN-ATTR                  
075300     End-If                                                               
075400                                                                          
075500     If MID-KDLEVPLF-IN = All '+'                                         
075600       Move MFS-RENSA-FAELT       To MOD-KDLEVPLF-IN                      
075700     Else                                                                 
075800       Move MID-KDLEVPLF-IN       To MOD-KDLEVPLF-IN                      
075810       IF MID-KDLEVPLF-IN = 'J'                                           
075820         MOVE 'Y'                   TO MOD-KDLEVPLF-IN                    
075830       END-IF                                                             
075900       Move MFS-ADD-LAES-IN-FAELT To MOD-KDLEVPLF-IN-ATTR                 
076000     End-If                                                               
076100                                                                          
076200     Move +1 To INDX                                                      
076300     Perform Until INDX > MAX-INDX                                        
076400       If MID-KDCMDVAL(INDX) = All '+'                                    
076500         Move MFS-RENSA-FAELT       To MOD-KDCMDVAL(INDX)                 
076600       Else                                                               
076700         Move MID-KDCMDVAL(INDX)    To MOD-KDCMDVAL(INDX)                 
076800         Move MFS-ADD-LAES-IN-FAELT To MOD-KDCMDVAL-ATTR(INDX)            
076900       End-If                                                             
077000       Add +1 To INDX                                                     
077100     End-Perform                                                          
077200     .                                                                    
077300     EJECT                                                                
077400                                                                          
077500 F-FOEREG-SIDA   Section.                                                 
077510     MOVE 'F-FOEREG-SIDA  '   TO CURRENT-SECTION                          
077600     SKIP2                                                                
077700*    -- PF6 tryckt. Föregående sida skall visas. Max 10 sidor             
077800*    --             Rensa PREV-nycklar för hoppsidan                      
077900     If SPAR-SIDNR > +1                                                   
078000*      -- Rensa först innevarande sida i tabellen.                        
078100       Move SPAR-SIDNR To SIDX                                            
078200       Initialize SPAR-KEY-WDD601KY-PREV(SIDX)                            
078300                                                                          
078400       Subtract +1     From SPAR-SIDNR                                    
078500       Move SPAR-SIDNR To   SIDX                                          
078600       Move SPAR-KEY-WDD601KY-PREV(SIDX) To W-WDD601KY-MIN-X              
078700     Else                                                                 
078800       Move SPAR-KEY-WDD601KY-ENTER To W-WDD601KY-MIN-X                   
078900       Move MSGI-IDANSK             To W-IDANSK-MIN                       
079000                                                                          
079100       Move INF-FIRST-PAGE To MED-IDMFSINF                                
079200       Call WMEDKONV Using MED-WMEDAREA                                   
079300       Move MED-MFSINF To MOD-TEMFSFEL                                    
079400     End-If                                                               
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800 G-LAES-VISA-INFO Section.                                                
079810     MOVE 'G-LAES-VISA-INFO '    TO CURRENT-SECTION                       
079900     SKIP2                                                                
080000     If ( MID-IDANSK-IN    Not = All '+' )                                
080100     Or ( MID-IDANSK-TO-IN Not = All '+' )                                
080200     Or ( MID-IDLEVNR-IN   Not = All '+' )                                
080300     Or ( MID-KDLPORS-IN   Not = All '+' )                                
080400     Or ( MID-KDLEVPLF-IN  Not = All '+' )                                
080500*      -- Någon urvalsnyckel är inmatad, eller ...                        
080600     Or MFS-IDPFK = '7'                                                   
080700         Set SET-KVRADER To True                                          
080800         Move +0 To SPAR-KVRADER                                          
080900     Else                                                                 
081000         Set SET-EJ-KVRADER To True                                       
081100     End-If                                                               
081200     Perform IMS-GU-WDD601                                                
081300     If SEGMENT-FINNS                                                     
081400       If MFS-NEXT Or MFS-PREVIOUS                                        
081500         Move Zero         To W-IDARTNR-MIN                               
081600         Move 999999999    To W-IDARTNR-MAX                               
081700                                                                          
081800         Move MSGI-IDANSK        To W-IDANSK-MIN                          
081900         Move SPAR-KEY-IDANSK-TO To W-IDANSK-MAX                          
082000       End-If                                                             
082100                                                                          
082200       Perform GA-KOLLA-OM-GODK-POST                                      
082300       Perform Until GODK-POST                                            
082400               Or    SEGMENT-SAKNAS Or SEGMENT-SLUT                       
082500         If W-IDLEVNR-MIN = Low-Value                                     
082600*so        Perform IMS-GN-WDD601-IDANSK                                   
082610           Perform IMS-GN-WDD601                                          
082700         Else                                                             
082800*so?       Perform IMS-GN-WDD601-IDLEVNR                                  
082810           Perform IMS-GN-WDD601                                          
082900         End-IF                                                           
083000         Perform GA-KOLLA-OM-GODK-POST                                    
083100       End-Perform                                                        
083200       If GODK-POST And SET-KVRADER                                       
083300         Move +1 To SPAR-KVRADER                                          
083400       End-If                                                             
083500     Else                                                                 
083600        Set EJ-GODK-POST To True                                          
083700     End-If                                                               
083800                                                                          
083900     If GODK-POST                                                         
084000*      -- Den första godkända posten enligt begr.nycklarna.               
084100*      -- Sätt INDX till MOD-RAD 1                                        
084200       Move +1 To INDX                                                    
084300       Perform GB-FLYTTA-DATA-TILL-MOD                                    
084400*      -- Första raden på ny sida sparas till ENTER                       
084500       Move LPF-IDDC     To SPAR-KEY-IDDC-ENTER                           
084510       Move LPF-IDLEVNR  To SPAR-KEY-IDLEVNR-ENTER                        
084600       Move LPF-IDARTNR  To SPAR-KEY-IDARTNR-ENTER                        
084700       Move LPF-IDANSK   To SPAR-KEY-IDANSK-ENTER                         
084800*      -- Spara IDANSK för ev.anv. i 2103                                 
084900       Move W-IDANSK-MIN To SPAR-KEY-IDANSK-FROM                          
085000       Move W-IDANSK-MAX To SPAR-KEY-IDANSK-TO                            
085100                                                                          
085200*      -- Fyll på med resten av GODK-poster                               
085300       If W-IDLEVNR-MIN = Low-Value                                       
085400*so      Perform IMS-GN-WDD601-IDANSK                                     
085410         Perform IMS-GN-WDD601                                            
085500       Else                                                               
085600*so      Perform IMS-GN-WDD601-IDLEVNR                                    
085610         Perform IMS-GN-WDD601                                            
085700       End-If                                                             
085800       Add 1 To INDX                                                      
085900                                                                          
086000       Perform Until SEGMENT-SAKNAS Or SEGMENT-SLUT                       
086100                  Or INDX > MAX-INDX                                      
086200         Perform GA-KOLLA-OM-GODK-POST                                    
086300         If GODK-POST                                                     
086400           If SET-KVRADER                                                 
086500             Add +1 To SPAR-KVRADER                                       
086600           End-If                                                         
086700           Perform GB-FLYTTA-DATA-TILL-MOD                                
086800           Add 1 to INDX                                                  
086900         End-If                                                           
087000         If W-IDLEVNR-MIN = Low-Value                                     
087100*so        Perform IMS-GN-WDD601-IDANSK                                   
087110           Perform IMS-GN-WDD601                                          
087200         Else                                                             
087300*so        Perform IMS-GN-WDD601-IDLEVNR                                  
087310           Perform IMS-GN-WDD601                                          
087400         End-IF                                                           
087500       End-Perform                                                        
087600*                                                                         
087700       If INDX <= MAX-INDX                                                
087800*        -- Töm sista raden / resten av raderna på skärmen                
087900         Perform Until INDX > MAX-INDX                                    
088000           Move MFS-RENSA-FAELT To MOD-IDLEVNR (INDX)                     
088100                                   MOD-IDARTNR (INDX)                     
088200                                   MOD-KDLPORS-TEXT (INDX, 1)             
088300                                   MOD-KDLPORS-TEXT (INDX, 2)             
088400                                   MOD-KDLPORS-TEXT (INDX, 3)             
088500                                   MOD-KDLEVPLF (INDX)                    
088600                                   MOD-BEART    (INDX)                    
088700                                   MOD-TIOMSPEC (INDX)                    
088800           Add 1 To INDX                                                  
088900         End-Perform                                                      
089000       End-If                                                             
089100*                                                                         
089200       If SEGMENT-FINNS                                                   
089300*        -- Det fanns poster kvar att läsa på basen                       
089400*        -- Läs ev. fram till GODK-POST och fixa i.s.f. NEXT-keys         
089500         Perform GA-KOLLA-OM-GODK-POST                                    
089600         Perform Until GODK-POST Or SEGMENT-SLUT                          
089700                                 Or SEGMENT-SAKNAS                        
089800           If W-IDLEVNR-MIN = Low-Value                                   
089900*so          Perform IMS-GN-WDD601-IDANSK                                 
089910             Perform IMS-GN-WDD601                                        
090000           Else                                                           
090100*so          Perform IMS-GN-WDD601-IDLEVNR                                
090110             Perform IMS-GN-WDD601                                        
090200           End-IF                                                         
090300           If SEGMENT-FINNS                                               
090400             Perform GA-KOLLA-OM-GODK-POST                                
090500           End-If                                                         
090600         End-Perform                                                      
090720                                                                          
090800         If GODK-POST                                                     
090900           If SET-KVRADER                                                 
091000*            -- Fortsätter räkna upp KVRADER                              
091100             Add +1 To SPAR-KVRADER                                       
091200           End-If                                                         
091300*          -- Flytta denna nyckel till NEXT-keys !                        
091400           Move LPF-IDDC     To SPAR-KEY-IDDC-NEXT                        
091410           Move LPF-IDLEVNR  To SPAR-KEY-IDLEVNR-NEXT                     
091500           Move LPF-IDARTNR  To SPAR-KEY-IDARTNR-NEXT                     
091600           Move LPF-IDANSK   To SPAR-KEY-IDANSK-NEXT                      
091700                                                                          
091800           Move INF-MORE-INFO-EXISTS To MED-IDMFSINF                      
091900           Call WMEDKONV Using MED-WMEDAREA                               
092000           Move MED-TEMFSINF To MOD-TEMFSINF                              
092100         End-If                                                           
092200       End-if                                                             
092300*      --- Slutkontroll                                                   
092400       If SEGMENT-SAKNAS  Or SEGMENT-SLUT                                 
092500*        -- Det fanns inga GODK poster kvar på basen.                     
092600         Move JA To SPAR-SISTA-SIDAN                                      
092700         Move SPAR-KEY-WDD601KY-ENTER To SPAR-KEY-WDD601KY-NEXT           
092800         Move INF-LAST-PAGE-SHOWN  To MED-IDMFSINF                        
092900         Call WMEDKONV Using MED-WMEDAREA                                 
093000         Move MED-TEMFSINF  To MOD-TEMFSINF                               
093100       Else                                                               
093200*        -- Det fanns minst en GODK post kvar på basen.                   
093300         Move NEJ To SPAR-SISTA-SIDAN                                     
093400         If SET-KVRADER                                                   
093500*          -- Fortsätter räkna upp KVRADER                                
093600*          -- läser vidare till BASENS slut och räknar GODK-POST          
093700           If W-IDLEVNR-MIN = Low-Value                                   
093800*so          Perform IMS-GN-WDD601-IDANSK                                 
093810             Perform IMS-GN-WDD601                                        
093900           Else                                                           
094000*so          Perform IMS-GN-WDD601-IDLEVNR                                
094010             Perform IMS-GN-WDD601                                        
094100           End-IF                                                         
094200           Perform Until SEGMENT-SAKNAS Or SEGMENT-SLUT                   
094300             Perform GA-KOLLA-OM-GODK-POST                                
094400             If GODK-POST                                                 
094500               Add +1 To SPAR-KVRADER                                     
094600             End-If                                                       
094700             If W-IDLEVNR-MIN = Low-Value                                 
094800*so            Perform IMS-GN-WDD601-IDANSK                               
094810               Perform IMS-GN-WDD601                                      
094900             Else                                                         
095000*so            Perform IMS-GN-WDD601-IDLEVNR                              
095010               Perform IMS-GN-WDD601                                      
095100             End-IF                                                       
095200           End-Perform                                                    
095300         End-If                                                           
095400       End-If                                                             
095500       Move SPAR-KVRADER To MOD-KVRADER                                   
095600       Move SPAR-SIDNR   To MOD-IDPAGE                                    
095700                                                                          
095800       Move '002'      To MSGI-KDCALL                                     
095900       Move '2147'     To SPAR-IDTRANS                                    
096000       Move SPAR-AREA  To MSGI-SPAR-AREA                                  
096100       Call W005INIT Using MSGI-WMSGINIT WDP7-PCB                         
096200     Else                                                                 
096300       Move INF-NO-LINES-EXISTS To MED-IDMFSINF                           
096400       Call WMEDKONV         Using MED-WMEDAREA                           
096500       Move MED-MFSINF          To MOD-TEMFSFEL                           
096600       Perform MFS-RENSA-FAELT-IN                                         
096700     End-If                                                               
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 GA-KOLLA-OM-GODK-POST     Section.                                       
097110     MOVE 'GA-KOLLA-OM-GODK-POST '  TO CURRENT-SECTION                    
097200     SKIP2                                                                
097300*    *----------------------------------------------------*               
097400*    * Här kollas övriga ev.inmatade begränsningsnycklar. *               
097500*    *----------------------------------------------------*               
097600     Set GODK-POST To True                                                
097700                                                                          
097800     If W-KDLPORS > Zero                                                  
097900     Or W-KDLEVPLF > Space                                                
098000                                                                          
098100        If W-KDLEVPLF > Space                                             
098110           IF W-KDLEVPLF = 'Y'                                            
098120             MOVE 'J'   TO W-KDLEVPLF                                     
098130           END-IF                                                         
098200           If LPF-KDLEVPLF Not = W-KDLEVPLF                               
098300              Set EJ-GODK-POST To True                                    
098400           End-If                                                         
098500        End-If                                                            
098600                                                                          
098700        If W-KDLPORS > Zero                                               
098800           If LPF-KDLPORS(1) Not = W-KDLPORS                              
098900           And LPF-KDLPORS(2) Not = W-KDLPORS                             
099000           And LPF-KDLPORS(3) Not = W-KDLPORS                             
099100              Set EJ-GODK-POST To True                                    
099200           End-If                                                         
099300        End-If                                                            
099400                                                                          
099500     End-If                                                               
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900 GB-FLYTTA-DATA-TILL-MOD Section.                                         
099910     MOVE 'GB-FLYTTA-DATA-TILL-MOD ' TO CURRENT-SECTION                   
100000     SKIP2                                                                
100100     Move LPF-IDLEVNR    To MOD-IDLEVNR  (INDX)                           
100200     Move LPF-IDARTNR    To MOD-IDARTNR  (INDX)                           
100300     Move Space          To MOD-KDLPORS-TEXT (INDX, 1)                    
100400                            MOD-KDLPORS-TEXT (INDX, 2)                    
100500                            MOD-KDLPORS-TEXT (INDX, 3)                    
100600     Move +1 To OIX, TIX                                                  
100700     Perform Until OIX > +3                                               
100800       If LPF-KDLPORS(OIX) > Zero And <= MAXINDEX-1                       
100900         Move LPF-KDLPORS(OIX) To KDLPORS-NUM                             
101000         Move TELPORS(KDLPORS-NUM)                                        
101100                         To MOD-KDLPORS-TEXT (INDX, TIX)                  
101200         Add +1 to TIX                                                    
101300       End-If                                                             
101400       Add +1 to OIX                                                      
101500     End-Perform                                                          
101510     IF LPF-KDLEVPLF = 'J'                                                
101520       MOVE 'Y'          To MOD-KDLEVPLF (INDX)                           
101530     ELSE                                                                 
101600       Move LPF-KDLEVPLF To MOD-KDLEVPLF (INDX)                           
101700     END-IF                                                               
101800     Move LPF-TIOMSPEC   To MOD-TIOMSPEC (INDX)                           
101810     PERFORM GBA-READ-ENGLISH-BEART                                       
101900     .                                                                    
102000     EJECT                                                                
102001                                                                          
102010 GBA-READ-ENGLISH-BEART SECTION.                                          
102100                                                                          
102101     MOVE LPF-IDARTNR   TO W-IDARTNR                                      
102102     PERFORM IMS-GET-WDD311-ENGLISH                                       
102103     IF SEGMENT-FINNS                                                     
102104       Move TEXT-BEART    To MOD-BEART    (INDX)                          
102105     ELSE                                                                 
102106       Move LPF-BEART    To MOD-BEART    (INDX)                           
102107     END-IF                                                               
102110     .                                                                    
102120     EJECT                                                                
102200 HOPP-TILL-2103 Section.                                                  
102210     MOVE 'HOPP-TILL-2103 '  TO CURRENT-SECTION                           
102300     SKIP2                                                                
102400     Move JA To KDCMDVAL-SW                                               
102500                                                                          
102600     If ((MID-KDCMDVAL(1) = '+') And                                      
102700         (MID-KDCMDVAL(2) = '+') And                                      
102800         (MID-KDCMDVAL(3) = '+') And                                      
102900         (MID-KDCMDVAL(4) = '+') And                                      
103000         (MID-KDCMDVAL(5) = '+') And                                      
103100         (MID-KDCMDVAL(6) = '+') And                                      
103200         (MID-KDCMDVAL(7) = '+') And                                      
103300         (MID-KDCMDVAL(8) = '+') And                                      
103400         (MID-KDCMDVAL(9) = '+') And                                      
103500         (MID-KDCMDVAL(10) = '+') And                                     
103600         (MID-KDCMDVAL(11) = '+') And                                     
103700         (MID-KDCMDVAL(12) = '+') And                                     
103800         (MID-KDCMDVAL(13) = '+') And                                     
103900         (MID-KDCMDVAL(14) = '+') And                                     
104000         (MID-KDCMDVAL(15) = '+') )                                       
104100         Move NEJ To KDCMDVAL-SW                                          
104200                                                                          
104300         Perform MFS-ROER-EJ-FAELT-UT                                     
104400         Perform MFS-FORM-ATTR                                            
104500         Move 'INGEN RAD VALD för HOPP till 2103'                         
104600                        To MOD-TEMFSFEL                                   
104700     End-If                                                               
104800     .                                                                    
104900     EJECT                                                                
105000                                                                          
105100 S02-INIT-PREVIOUS    Section.                                            
105110     MOVE 'S02-INIT-PREVIOUS  '    TO CURRENT-SECTION                     
105200     SKIP2                                                                
105300*    -- För att inte kunna köra PF6(prev) efter PF7 (first)               
105400     Move +1 To SIDX                                                      
105500     Perform Until SIDX > MAX-SIDX                                        
105600       Initialize  SPAR-KEY-WDD601KY-PREV   (SIDX)                        
105700       Add +1 To SIDX                                                     
105800     End-Perform                                                          
105900     .                                                                    
106000     EJECT                                                                
106100                                                                          
106200 MFS-RENSA-FAELT-UT Section.                                              
106300     SKIP2                                                                
106400*    --- ALLA UTDATA-FÄLT  INKL. BLÄDDRINGSNYCKLAR                        
106500*    Move MFS-RENSA-FAELT To MOD-IDANSK-UT                                
106600*                            MOD-IDANSK-TO-UT                             
106700*                            MOD-IDLEVNR-UT                               
106800*                            MOD-KDLPORS-UT                               
106900*                            MOD-KDLEVPLF-UT                              
107000     Move MFS-RENSA-FAELT To                                              
107100                             MOD-KVRADER                                  
107200                             MOD-IDPAGE                                   
107300     Move +1 To INDX                                                      
107400     Perform MFS-RENSA-RAD-FAELT-UT                                       
107500     .                                                                    
107600     SKIP2                                                                
107700 MFS-RENSA-RAD-FAELT-UT Section.                                          
107800                                                                          
107900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
108000*****Move +1 To INDX   *** Sätt index före hopp hit                       
108100                                                                          
108200     Perform Until INDX > MAX-INDX                                        
108300       Move MFS-RENSA-FAELT To MOD-KDCMDVAL(INDX)                         
108400                               MOD-IDARTNR (INDX)                         
108500                               MOD-BEART (INDX)                           
108600                               MOD-IDLEVNR (INDX)                         
108700                               MOD-KDLPORS-TEXT (INDX, 1)                 
108800                               MOD-KDLPORS-TEXT (INDX, 2)                 
108900                               MOD-KDLPORS-TEXT (INDX, 3)                 
109000                               MOD-KDLEVPLF (INDX)                        
109100                               MOD-TIOMSPEC (INDX)                        
109200       Add +1 To INDX                                                     
109300     End-Perform                                                          
109400     .                                                                    
109500     SKIP3                                                                
109600                                                                          
109700 MFS-RENSA-FAELT-IN Section.                                              
109800     SKIP2                                                                
109900*    --- ALLA INDATA-FÄLT                                                 
110000     Move MFS-RENSA-FAELT To MOD-IDANSK-IN                                
110100                             MOD-IDANSK-TO-IN                             
110200                             MOD-IDLEVNR-IN                               
110300                             MOD-KDLPORS-IN                               
110400                             MOD-KDLEVPLF-IN                              
110500     Move +1 To INDX                                                      
110600     Perform Until INDX > MAX-INDX                                        
110700       Move MFS-RENSA-FAELT To MOD-KDCMDVAL(INDX)                         
110800       Add +1 To INDX                                                     
110900     End-Perform                                                          
111000     .                                                                    
111100     EJECT                                                                
111200                                                                          
111300 MFS-ROER-EJ-FAELT-UT  Section.                                           
111400     SKIP2                                                                
111500*    --- ALLA UTDATA-FÄLT                                                 
111600*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
111700     Move MFS-ROER-EJ-FAELT To MOD-IDANSK-UT                              
111800                               MOD-IDANSK-TO-UT                           
111900                               MOD-IDLEVNR-UT                             
112000                               MOD-KDLPORS-UT                             
112100                               MOD-KDLEVPLF-UT                            
112200                               MOD-KVRADER                                
112300                               MOD-IDPAGE                                 
112400     Move +1 To INDX                                                      
112500     Perform Until INDX > MAX-INDX                                        
112600       Perform MFS-ROER-EJ-EN-RAD-FAELT-UT                                
112700       Add +1 To INDX                                                     
112800     End-Perform                                                          
112900     .                                                                    
113000     SKIP2                                                                
113100*                                                                         
113200 MFS-ROER-EJ-EN-RAD-FAELT-UT  Section.                                    
113300     SKIP2                                                                
113400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
113500     Move MFS-ROER-EJ-FAELT To MOD-IDARTNR(INDX)                          
113600                               MOD-BEART(INDX)                            
113700                               MOD-IDLEVNR(INDX)                          
113800                               MOD-KDLPORS-TEXT(INDX, 1)                  
113900                               MOD-KDLPORS-TEXT(INDX, 2)                  
114000                               MOD-KDLPORS-TEXT(INDX, 3)                  
114100                               MOD-KDLEVPLF(INDX)                         
114200                               MOD-TIOMSPEC (INDX)                        
114300     .                                                                    
114400     SKIP3                                                                
114500*                                                                         
114600*                                                                         
114700 MFS-FORM-ATTR Section.                                                   
114800*                                                                         
114900*    --- ALLA INDATA-FÄLT                                                 
115000     Move MFS-FORMATETS-ATTR To MOD-IDANSK-IN-ATTR                        
115100                                MOD-IDANSK-TO-IN-ATTR                     
115200                                MOD-IDLEVNR-IN-ATTR                       
115300                                MOD-KDLPORS-IN-ATTR                       
115400                                MOD-KDLEVPLF-IN-ATTR                      
115500     Move +1 To INDX                                                      
115600     Perform Until INDX > MAX-INDX                                        
115700       Move MFS-FORMATETS-ATTR To MOD-KDCMDVAL-ATTR(INDX)                 
115800       Add +1 To INDX                                                     
115900     End-Perform                                                          
116000     .                                                                    
116100     EJECT                                                                
116200*                                                                         
116300                                                                          
116400* --- IMS SEKTIONER ---                                                   
116500     SKIP3                                                                
116600 IMS-GET-MSG Section.                                                     
116700                                                                          
116800     Move '  QC' To GODK-STATUSKODER                                      
116900     Call CBLTDLI Using GU MSG-PCB MSG-IO-AREA                            
117000     Move MSG-STATUS-CODE To STATUS-WS                                    
117100     Perform IMS-STATUSKONTROLL                                           
117200     .                                                                    
117300     SKIP3                                                                
117400                                                                          
117500 IMS-INSERT-MSG Section.                                                  
117600                                                                          
117800     Move 'N' To MFS-KDHUVOMR                                             
118000     Move Low-Value To MSG-KDZ1 MSG-KDZ2                                  
118100     Move Space To GODK-STATUSKODER                                       
118200     Call CBLTDLI Using ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
118300     Move MSG-STATUS-CODE To STATUS-WS                                    
118400     Perform IMS-STATUSKONTROLL                                           
118500     .                                                                    
118600     EJECT                                                                
118700                                                                          
118800 IMS-INSERT-ALT-MSG Section.                                              
118900     SKIP2                                                                
119000     Move Space To GODK-STATUSKODER                                       
119200     Move '2' To P-KDMFSFOR                                               
119400     Call CBLTDLI Using ISRT ALT-PCB W-PROG-TO-PROG-SW                    
119500     Move ALT-STATUS-CODE To STATUS-WS                                    
119600     Perform IMS-STATUSKONTROLL                                           
119700     Continue                                                             
119800     .                                                                    
119900     EJECT                                                                
120000*                                                                         
120100 IMS-GU-WDD601        Section.                                            
120110     MOVE 'IMS-GU-WDD601 '   TO DBS-SECTION                               
120200     SKIP2                                                                
120300     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
120400                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
120500                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
120600                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
120700     Delimited By Size Into SSA1                                          
120800     Move '  GE' To GODK-STATUSKODER                                      
120900     Call CBLTDLI Using GU WDD6-PCB DLI-IO-WDD601 SSA1                    
121000     Move WDD6-STATUS-CODE To STATUS-WS                                   
121100     Perform IMS-STATUSKONTROLL                                           
121200     .                                                                    
121300     EJECT                                                                
121400 IMS-GN-WDD601        Section.                                            
121410     MOVE 'IMS-GN-WDD601 '  TO DBS-SECTION                                
121500                                                                          
121600     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
121700                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
121800                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
121900                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
122000     Delimited By Size Into SSA1                                          
122100     Move '  GEGB' To GODK-STATUSKODER                                    
122200     Call CBLTDLI Using GN WDD6-PCB DLI-IO-WDD601 SSA1                    
122300     Move WDD6-STATUS-CODE To STATUS-WS                                   
122400     Perform IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122610 IMS-GET-WDD311-ENGLISH SECTION.                                          
122620     MOVE 'IMS-GET-WDD311-ENGLISH '  TO DBS-SECTION                       
122630     SKIP2                                                                
122640     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
122650            DELIMITED BY SIZE INTO SSA1                                   
122660     MOVE   'WDD311  (IDSKYLT  =GB )'                                     
122670                                TO SSA2                                   
122680     MOVE '  GE' TO GODK-STATUSKODER                                      
122690     CALL CBLTDLI USING GU   WDD3-PCB WDD3-IO-AREA SSA1 SSA2              
122691     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
122692     PERFORM IMS-STATUSKONTROLL                                           
122693     .                                                                    
122694     EJECT                                                                
122700*                                                                         
122800*IMS-GN-WDD601-IDANSK        Section.                                     
122810*    MOVE 'IMS-GN-WDD601-IDANSK '   TO DBS-SECTION                        
122900*    SKIP2                                                                
123000*    STRING 'WDD601  (IDANSK  >=' W-IDANSK-MIN-X                          
123100*                OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
123200*    Delimited By Size Into SSA1                                          
123300*    Move '  GEGB' To GODK-STATUSKODER                                    
123400*    Call CBLTDLI Using GN WDD6-PCB DLI-IO-WDD601 SSA1                    
123500*    Move WDD6-STATUS-CODE To STATUS-WS                                   
123600*    Perform IMS-STATUSKONTROLL                                           
123700*    .                                                                    
123800*    EJECT                                                                
123900*IMS-GN-WDD601-IDLEVNR       Section.                                     
123910*    MOVE 'IMS-GN-WDD601-IDLEVNR '  TO DBS-SECTION                        
124000*    SKIP2                                                                
124100*    STRING 'WDD601  (IDLEVNR >=' W-IDLEVNR-MIN                           
124200*                OCH 'IDLEVNR <=' W-IDLEVNR-MAX                           
124300*                OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
124400*                OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
124500*    Delimited By Size Into SSA1                                          
124600*    Move '  GEGB' To GODK-STATUSKODER                                    
124700*    Call CBLTDLI Using GN WDD6-PCB DLI-IO-WDD601 SSA1                    
124800*    Move WDD6-STATUS-CODE To STATUS-WS                                   
124900*    Perform IMS-STATUSKONTROLL                                           
125000*    .                                                                    
125100*    EJECT                                                                
125200*                                                                         
125300 IMS-STATUSKONTROLL Section.                                              
125400                                                                          
125500     Set STATUS-IX To 1                                                   
125600     Search GODK-STATUS                                                   
125700       At End                                                             
125800         String ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
125900         Delimited By Size Into FELTEXT                                   
126000         Call FELLOG                                                      
126100       When GODK-STATUS (STATUS-IX) = STATUS-WS                           
126200         Continue                                                         
126300     End-Search                                                           
126400     .                                                                    
