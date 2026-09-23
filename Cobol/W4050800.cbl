000100******************************************************************        
000200*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0143      *        
000300******************************************************************        
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W4050800.                                                
000700 AUTHOR.         STIG MULLER.                                             
000800     DATE-WRITTEN JUN  85.                                                
000900                                                                          
001000     REMARKS.                                                             
001100*                                                                         
001200*                                                                         
001300*                                                                         
001400*    FUNKTION.                                                            
001500*                                                                         
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T508                                              
002000*        MID:         W4I50801                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O50801                                            
002400*    SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  PROGRAM-NAMN              PIC X(8)    VALUE 'W4050800'.              
003300 77  JA                        PIC X       VALUE 'J'.                     
003400 77  NEJ                       PIC X       VALUE 'N'.                     
003500 77  NYCKLAR-OK                PIC X       VALUE 'J'.                     
003600 77  PRODNR-OK                 PIC X       VALUE 'J'.                     
003700 77  ORDERNR-OK                PIC X       VALUE 'J'.                     
003800 77  IDORDNR7-WS               PIC X(7)    VALUE SPACE.                   
003900 77  IDPRODNR-WS               PIC X(7)    VALUE SPACE.                   
004000 77  IDKOLLI-WS                PIC X(5)    VALUE SPACE.                   
004100 77  IDARTNR-WS                PIC X(9)    VALUE SPACE.                   
004200 77  IDDC-WS                   PIC X(2)    VALUE SPACE.                   
004300 77  IX                        PIC S9(9)   VALUE +1   COMP-3.             
004400 77  MAX-RADER                 PIC S9(3)   VALUE +11   COMP SYNC.         
004500 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +1342 COMP SYNC.         
004600 77  SPRAK-INDEX               PIC S9(1)   COMP-3.                        
004700 77  OK-VISA-ALLT              PIC X       VALUE ' '.                     
004800 77  OBEHORIG                  PIC X       VALUE 'F'.                     
004900 77  IMPORTER                  PIC X       VALUE '1'.                     
005000 77  DEALER                    PIC X       VALUE '2'.                     
005100 77  INTERNDISTRIKT            PIC X       VALUE '3'.                     
005200                                                                          
005300 01  WS-TEDDI.                                                            
005400     03 FILLER                   PIC X(6) VALUE SPACE.                    
005500     03 WS-KDVALISO              PIC X(3).                                
005600     03 FILLER                   PIC X(2) VALUE SPACE.                    
005700                                                                          
005800*      --- VALID IDDC CODES                                               
005900*01    -COPY WWDCKONS                                                     
006000*01    -COPY WWDC99                                                       
006100*01    -COPY WWDC99 -PRE VORD-                                            
006200       EJECT                                                              
006300 77  WS-VKORDBTO                 PIC S9(6)V9.                             
006400 77  WS-VLORDBTO                 PIC S9(4)V9(3).                          
006500                                                                          
006600 01  WS-KDMATT                   PIC X.                                   
006700     88 US-MEASUREMENT           VALUE 'U'.                               
006800     88 SIS-MEASUREMENT          VALUE 'S'.                               
006900                                                                          
007000 01  W-SPAR-IDKUNDRF.                                                     
007100     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
007200     03  FILLER                  PIC X(3)    VALUE '+++'.                 
007300                                                                          
007400     EJECT                                                                
007500 77  WS-IDTRANS                  PIC X(4).                                
007600     88  EGEN-MID                          VALUE '4508'.                  
007700     88  GODK-MID                          VALUE '4501' '4502'            
007800               '4503' '4504' '4505' '4506' '4507' '4508' '4509'.          
007900                                                                          
008000 77  ALLT-SW                     PIC X     VALUE 'J'.                     
008100     88  ALLT-OK                           VALUE 'J'.                     
008200     88  ALLT-FEL                          VALUE 'N'.                     
008300                                                                          
008400 77  AVERAGECOST-SW             PIC X      VALUE 'J'.                     
008500     88  AVERAGECOST                       VALUE 'J'.                     
008600                                                                          
008700 77  BOUNCE-SW                  PIC X      VALUE 'J'.                     
008800     88  BOUNCEORDER                       VALUE 'J'.                     
008900                                                                          
009000 77  VISA-ORDER-SW              PIC X      VALUE 'J'.                     
009100     88  VISA-ORDER                        VALUE 'J'.                     
009200     EJECT                                                                
009300 01  IDDISTR.                                                             
009400     03  IDDISTR-WS                PIC X(4)    VALUE SPACE.               
009500     03  IDDISTR-NUM REDEFINES IDDISTR-WS PIC 9(4).                       
009600                                                                          
009700 01  IDKUNDNR.                                                            
009800     03  IDKUNDNR-WS               PIC X(6)    VALUE SPACE.               
009900     03  IDKUNDNR-NUM REDEFINES IDKUNDNR-WS PIC 9(6).                     
010000 01  ADFLOMR.                                                             
010100     03  ADFLOMR1-5                PIC X(5)    VALUE SPACE.               
010200     03 FILLER REDEFINES ADFLOMR1-5.                                      
010300       05  ADFLOMR1                PIC X.                                 
010400       05  ADFLOMR2-4              PIC X(3).                              
010500       05  ADFLOMR5                PIC X.                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)     VALUE 'SPAR AREA'.         
010800                                                                          
010900 01  SPAR-IDPRODNR               PIC S9(7)      VALUE ZERO COMP-3.        
011000 01  SPAR-KVKOLLI                PIC S9(5)      VALUE ZERO COMP-3.        
011100 01  SPAR-KVKOLLI-FAKT           PIC S9(5)      VALUE ZERO COMP-3.        
011200 01  SPAR-KVKOLLI-LAST           PIC S9(5)      VALUE ZERO COMP-3.        
011300 01  SPAR-KVORDRAD               PIC S9(5)      VALUE ZERO COMP-3.        
011400 01  SPAR-SUORDV                 PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011500 01  SPAR-SUORDV-LOC             PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011600 01  SPAR-SUORDV-LOCPREL         PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011700 01  SPAR-VKORDBTO               PIC S9(6)V9(1) VALUE ZERO COMP-3.        
011800 01  SPAR-VLORDBTO               PIC S9(4)V9(3) VALUE ZERO COMP-3.        
011900 01  HELP-SUMMA                  PIC S9(9)V9(2) VALUE ZERO COMP-3.        
012000 01  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
012100     EJECT                                                                
012200 01    NYCKLAR-TILL-DLI.                                                  
012300     03  W-WDE4A1KY-MIN-X.                                                
012400         05 W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
012500         05 W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
012600         05 W-IDKUNDRF-MIN.                                               
012700            07 W-IDORDNR5-MIN    PIC  9(5)   VALUE ZERO.                  
012800            07 FILLER            PIC  X(5)   VALUE SPACE.                 
012900         05 W-IDPRODNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
013000         05 FILLER               PIC  X(2)   VALUE LOW-VALUE.             
013100                                                                          
013200     03  W-WDE4A1KY-MAX-X.                                                
013300         05 W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
013400         05 W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
013500         05 W-IDKUNDRF-MAX.                                               
013600            07 W-IDORDNR5-MAX    PIC  9(5)   VALUE ZERO.                  
013700            07 FILLER            PIC  X(5)   VALUE SPACE.                 
013800         05 W-IDPRODNR-MAX       PIC S9(7)  VALUE +9999999 COMP-3.        
013900         05 FILLER               PIC  X(2)   VALUE HIGH-VALUE.            
014000                                                                          
014100     03  W-IDPRODNR-X.                                                    
014200         05 W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.          
014300                                                                          
014400     03  W-IDKOLLI-MIN-X.                                                 
014500         05 W-IDKOLLI-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
014600     03  W-IDKOLLI-MAX-X.                                                 
014700         05 W-IDKOLLI-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
014800*                                                                         
014900     EJECT                                                                
015000 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
016000     SKIP3                                                                
016100 01  FILLER REDEFINES TEST-IDDISTR.                                       
016200*  03 -COPY WWDIST03.                                                     
016300 01  FILLER REDEFINES TEST-IDDISTR.                                       
016400*  03 -COPY WWDIST79                                                      
016500*     ----DISTR-DEALER-PRICE----                                          
016600     EJECT                                                                
016700 01    MEDDELANDE.                                                        
016800   03    FEL1.                                                            
016900     05  FILLER                  PIC X(40)                                
017000         VALUE '901 FEL NYCKEL'.                                          
017100     05  FILLER                  PIC X(40)                                
017200         VALUE '901 WRONG KEY'.                                           
017300   03    FILLER REDEFINES FEL1.                                           
017400     05  FEL-1 OCCURS 2          PIC X(40).                               
017500     SKIP3                                                                
017600   03    FEL2.                                                            
017700     05  FILLER                  PIC X(40)                                
017800         VALUE '910 ORDERN SAKNAS'.                                       
017900     05  FILLER                  PIC X(40)                                
018000         VALUE '910 ORDER MISSING'.                                       
018100   03    FILLER REDEFINES FEL2.                                           
018200     05  FEL-2 OCCURS 2          PIC X(40).                               
018300     SKIP3                                                                
018400   03    FEL3.                                                            
018500     05  FILLER                  PIC X(40)                                
018600         VALUE '916 KOLLI SAKNAS'.                                        
018700     05  FILLER                  PIC X(40)                                
018800         VALUE '916 CASE MISSING'.                                        
018900   03    FILLER REDEFINES FEL3.                                           
019000     05  FEL-3 OCCURS 2          PIC X(40).                               
019100     SKIP3                                                                
019200   03    FEL4.                                                            
019300     05  FILLER                  PIC X(40)                                
019400         VALUE '115 OBEHÖRIG ANVÄNDARE                 '.                 
019500     05  FILLER                  PIC X(40)                                
019600         VALUE '115 USER NOT AUTHORIZED                 '.                
019700   03    FILLER REDEFINES FEL4.                                           
019800     05  FEL-4 OCCURS 2          PIC X(40).                               
019900     SKIP3                                                                
020000   03    MED1.                                                            
020100     05  FILLER                  PIC X(79)                                
020200         VALUE '    FLER KOLLI FINNS             '.                       
020300     05  FILLER                  PIC X(79)                                
020400         VALUE '    MORE CASES FOLLOWS           '.                       
020500   03    FILLER REDEFINES MED1.                                           
020600     05  MED-1 OCCURS 2          PIC X(79).                               
020700     EJECT                                                                
020800 01  DYNAMISK-SUBMODUL.                                                   
020900   03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.              
021000   03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.              
021100   03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.              
021200   03  W005INIT                PIC X(8)    VALUE 'W005INIT'.              
021300   03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.              
021400     SKIP2                                                                
021500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021600*01 -COPY WMSGINIT                                                        
021700     SKIP2                                                                
021800 01    FILLER                  PIC X(16)   VALUE 'WWOMVAND '.             
021900*01   -COPY WWOMVAND                                                      
022000     SKIP2                                                                
022100*01   -COPY WSECAREA                                                      
022200     SKIP2                                                                
022300******************************************************************        
022400*                                                                         
022500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
022600*                                                                         
022700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
022800     SKIP3                                                                
022900*01    MID -COPY W4I50801.                                                
023000     EJECT                                                                
023100*01    -COPY WMSGAREA                                                     
023200     EJECT                                                                
023300*  03    MOD -COPY W4O50801 -RED MSG-AREA.                                
023400     EJECT                                                                
023500*01    -COPY WMFSAREA                                                     
023600     EJECT                                                                
023700******************************************************************        
023800*                                                                         
023900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024000*                                                                         
024100 01    IMS-WS.                                                            
024200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
024300     SKIP3                                                                
024400*                        **** STATUS-KOD FRÅN IMS                         
024500   03    STATUS-WS               PIC XX.                                  
024600     88    SEGMENT-FINNS                     VALUE '  '.                  
024700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
024800     88    BASEN-SLUT                        VALUE 'GB'.                  
024900     SKIP3                                                                
025000   03    GODK-STATUSKODER.                                                
025100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
025200     SKIP3                                                                
025300 01    SSA1                      PIC X(96).                               
025400 01    SSA2                      PIC X(96).                               
025500     EJECT                                                                
025600*                            IMS FUNKTIONSKODER                           
025700*01    -COPY W0003                                                        
025800     EJECT                                                                
025900*                            DLI INPUT-OUTPUT AREA                        
026000 01  FILLER                      PIC X(16) VALUE 'DLI-WDE4A1'.            
026100                                                                          
026200 01    DLI-IO-AREA-WDE4A1.                                                
026300*  03             -COPY WDE4A1                                            
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16) VALUE 'DLI-WDE601'.            
026600                                                                          
026700 01    DLI-IO-AREA-WDE601.                                                
026800*  03             -COPY WDE601                                            
026900     EJECT                                                                
027000 01  FILLER                      PIC X(16) VALUE 'DLI-WDE611'.            
027100                                                                          
027200 01    DLI-IO-AREA-WDE611.                                                
027300*  03             -COPY WDE611                                            
027400     EJECT                                                                
027500 LINKAGE SECTION.                                                         
027600*01    -COPY W0009     -PRE MSG-                                          
027700     EJECT                                                                
027800*01    -COPY W0008     -PRE USEA-                                         
027900     05  FILLER                  PIC X.                                   
028000     SKIP3                                                                
029000*01    -COPY W0008     -PRE WDE6-                                         
030000     05  FILLER                  PIC X.                                   
030100     SKIP3                                                                
030200*01    -COPY W0008     -PRE WDE4A-                                        
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500 PROCEDURE DIVISION USING  MSG-PCB  USEA-PCB WDE6-PCB  WDE4A-PCB.         
030600     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB WDE6-PCB  WDE4A-PCB.         
030700                                                                          
030800     PERFORM IMS-GET-MSG                                                  
030900                                                                          
031000     IF SEGMENT-FINNS                                                     
032000       PERFORM A-INIT-SPARA-INPUT                                         
032100       PERFORM B-KOLLA-NYCKLAR                                            
032200                                                                          
032300       IF NYCKLAR-OK = JA                                                 
032400         PERFORM C-KONTROLL-SECURIT                                       
032500       END-IF                                                             
032600                                                                          
032700       IF SEC-KDSVAR = OBEHORIG                                           
032800         MOVE FEL-4 (SPRAK-INDEX)          TO MOD-MESSAGE-RAD1            
032900       ELSE                                                               
033000                                                                          
033100         EVALUATE TRUE                                                    
033200           WHEN PRODNR-OK = JA                                            
033300             PERFORM D-LAES-WDE6                                          
033400           WHEN ORDERNR-OK = JA                                           
033500             PERFORM E-LAES-WDE4A                                         
033600           WHEN OTHER                                                     
033700             MOVE FEL-1 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                 
033800             PERFORM S02-BLANKA-RADER                                     
033900         END-EVALUATE                                                     
034000                                                                          
034100         MOVE SPAR-KDVALISO TO WS-KDVALISO                                
034200         MOVE WS-TEDDI      TO MOD-TEDDI                                  
034300       END-IF                                                             
034400                                                                          
034500       PERFORM F-SPARA-NYCKLAR                                            
034600                                                                          
034700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
034800       PERFORM IMS-INSERT-MSG                                             
034900     END-IF                                                               
035000     MOVE ZERO TO RETURN-CODE                                             
035100     GOBACK                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 A-INIT-SPARA-INPUT SECTION.                                              
035500                                                                          
035600     MOVE SPACE                            TO SEC-KDSVAR                  
035700                                                                          
035800     IF MSG-DUBBLA-TRANSKODER                                             
035900       MOVE MSG-INDATA-MINUS-2-TRANSKODER  TO MID-W4I50801                
036000       MOVE MSG-IDTRANS-2                  TO MFS-IDTRANS                 
036100       MOVE MSG-KDMFSFOR-2                 TO MFS-KDMFSFOR                
036200     ELSE                                                                 
036300       MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I50801                
036400       MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                 
036500       MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR                
036600     END-IF                                                               
036700                                                                          
036800     MOVE MSG-KDTRTYP                      TO MFS-KDTRTYP                 
036900     MOVE MSG-IDPFK                        TO MFS-IDPFK                   
037000     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
037100                                                                          
037200     IF NOT EGEN-MID                                                      
037300       MOVE '7'                      TO MFS-IDPFK                         
037400     END-IF                                                               
037500                                                                          
037600     MOVE LOW-VALUE TO MSG-AREA                                           
037700     MOVE 'W4O508N1' TO MFS-IDMOD                                         
037800     MOVE '4508' TO MOD-IDTRANS                                           
037900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
038000                             MOD-IDKUNDNR-IN                              
038100                             MOD-IDORDNR7-IN                              
038200                             MOD-IDPRODNR-IN                              
038300                             MOD-IDKOLLI-IN                               
038400                             MOD-IDDC-IN                                  
038500                             MOD-MESSAGE-RAD1                             
038600                             MOD-MESSAGE-RAD23                            
038700     .                                                                    
038800     EJECT                                                                
038900 B-KOLLA-NYCKLAR SECTION.                                                 
039000                                                                          
039100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039200     MOVE '001'             TO MSGI-KDCALL                                
039300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039400     MOVE '4508'            TO MSGI-IDTRANS                               
039500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039600     IF EGEN-MID                                                          
039700        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
039800        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
039900        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
040000        IF MID-IDORDNR7-IN      NOT = ALL '+'                             
040100           MOVE MID-IDORDNR7-IN TO W-SPAR-IDORDNR7                        
040200           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
040300        END-IF                                                            
040400        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
040500        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
040600     END-IF                                                               
040700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040800                                                                          
040900     MOVE MSGI-KDMATT        TO WS-KDMATT                                 
041000                                                                          
041100     IF MSGI-IDLAND-SPR     = 'SE '                                       
041200        MOVE +1             TO SPRAK-INDEX                                
041300     ELSE                                                                 
041400        MOVE +2             TO SPRAK-INDEX                                
041500     END-IF                                                               
041600                                                                          
041700     IF GODK-MID                                                          
041800       CONTINUE                                                           
041900     ELSE                                                                 
042000       MOVE ZERO             TO MID-IDPRODNR-UT                           
042100     END-IF                                                               
042200                                                                          
042300     IF MID-IDDISTR-IN       NOT =  ALL '+'                               
042400       MOVE ZERO             TO MID-IDPRODNR-UT                           
042500       MOVE '7'              TO MFS-IDPFK                                 
042600     END-IF                                                               
042700                                                                          
042800     IF MID-IDKUNDNR-IN      NOT  = ALL '+'                               
042900       MOVE ZERO             TO MID-IDPRODNR-UT                           
043000       MOVE '7'              TO MFS-IDPFK                                 
043100     END-IF                                                               
043200                                                                          
043300     IF MID-IDORDNR7-IN               NOT  = ALL '+'                      
043400       MOVE ZERO                      TO MID-IDPRODNR-UT                  
043500       MOVE '7'                       TO MFS-IDPFK                        
043600     END-IF                                                               
043700                                                                          
043800     IF MID-IDPRODNR-IN               NOT = ALL '+'                       
043900       MOVE MID-IDPRODNR-IN           TO IDPRODNR-WS                      
044000       MOVE '7'                       TO MFS-IDPFK                        
044100     END-IF                                                               
044200                                                                          
044300     IF MID-IDKOLLI-IN                NOT = ALL '+'                       
044400       MOVE MID-IDKOLLI-IN            TO IDKOLLI-WS                       
044500       MOVE '7'                       TO MFS-IDPFK                        
044600     END-IF                                                               
044700                                                                          
044800     MOVE ZERO                           TO W-IDKOLLI-MIN                 
044900     MOVE +99999                         TO W-IDKOLLI-MAX                 
045000     MOVE NEJ                            TO NYCKLAR-OK                    
045100                                            PRODNR-OK                     
045200                                            ORDERNR-OK                    
045300                                                                          
045400     IF IDPRODNR-WS NUMERIC AND                                           
045500        IDPRODNR-WS NOT = ZERO                                            
045600       MOVE IDPRODNR-WS                  TO W-IDPRODNR                    
045700       MOVE ZERO                         TO IDORDNR7-WS                   
045800       MOVE JA                           TO PRODNR-OK                     
045900                                            NYCKLAR-OK                    
046000       MOVE NEJ                          TO ORDERNR-OK                    
046100     ELSE                                                                 
046200                                                                          
046300       IF MSGI-IDDISTR NUMERIC    AND                                     
046400          MSGI-IDDISTR    NOT = ZERO AND                                  
046500          MSGI-IDKUNDNR  NUMERIC   AND                                    
046600          MSGI-IDKUNDRF(1:7) NUMERIC                                      
046700         MOVE MSGI-IDDISTR                 TO W-IDDISTR-MIN               
046800                                              W-IDDISTR-MAX               
046900                                              TEST-IDDISTR                
047000         MOVE MSGI-IDKUNDNR                TO W-IDKUNDNR-MIN              
047100                                              W-IDKUNDNR-MAX              
047200         MOVE MSGI-IDKUNDRF (3:5)          TO W-IDORDNR5-MIN              
047300                                              W-IDORDNR5-MAX              
047400         MOVE JA                           TO ORDERNR-OK                  
047500                                              NYCKLAR-OK                  
047600       END-IF                                                             
047700     END-IF                                                               
047800                                                                          
047900     IF DIST79-DEALER-PRICE                                               
048000       IF ENGLISH-TEXT                                                    
048100         MOVE 'DEALERPRICE'    TO MOD-TEDDI                               
048200       ELSE                                                               
048300         MOVE '    ÅF PRIS'    TO MOD-TEDDI                               
048400       END-IF                                                             
048500     ELSE                                                                 
048600       MOVE SPACE              TO MOD-TEDDI                               
048700     END-IF                                                               
048800                                                                          
048900     IF IDKOLLI-WS NOT NUMERIC                                            
049000       MOVE ZERO               TO IDKOLLI-WS                              
050000     ELSE                                                                 
050100       IF IDKOLLI-WS NUMERIC AND                                          
050200          IDKOLLI-WS NOT = ZERO                                           
050300         MOVE IDKOLLI-WS       TO W-IDKOLLI-MIN                           
050400                                  W-IDKOLLI-MAX                           
050500       END-IF                                                             
050600     END-IF                                                               
050700                                                                          
050800     IF EGEN-MID                                                          
050900        IF MID-IDDC-IN     = ALL '+'                                      
051000          MOVE MID-IDDC-UT TO IDDC-WS                                     
051100        ELSE                                                              
051200          MOVE MID-IDDC-IN TO IDDC-WS                                     
051300          MOVE '7'        TO MFS-IDPFK                                    
051400          MOVE SPACE      TO MFS-KDTRTYP                                  
051500        END-IF                                                            
051600     ELSE                                                                 
051700        MOVE MSGI-IDDC       TO IDDC-WS                                   
051800     END-IF                                                               
051900                                                                          
052000     MOVE IDDC-WS                    TO MOD-IDDC-UT                       
052100                                        WS-IDDC                           
052200                                                                          
052300     IF GODK-MID                                                          
052400        IF MID-IDARTNR-IN      =  ALL '+'                                 
052500          MOVE MID-IDARTNR-UT  TO IDARTNR-WS                              
052600        ELSE                                                              
052700          MOVE MID-IDARTNR-IN  TO IDARTNR-WS                              
052800        END-IF                                                            
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 C-KONTROLL-SECURIT SECTION.                                              
053300                                                                          
053400     MOVE MSG-SIGNON-USERID            TO SEC-IDUSER                      
053500     MOVE '4508'                       TO SEC-IDTRANS                     
053600     MOVE MSGI-IDDISTR                 TO SEC-IDKEY                       
053700                                                                          
053800     CALL WSECURIT USING                  SEC-IDUSER                      
053900                                          SEC-IDTRANS                     
054000                                          SEC-IDKEY                       
054100                                          SEC-KDSVAR                      
054200     .                                                                    
054300     EJECT                                                                
054400 D-LAES-WDE6 SECTION.                                                     
054500                                                                          
054600     MOVE JA TO VISA-ORDER-SW                                             
054700     MOVE NEJ TO BOUNCE-SW                                                
054800     PERFORM DA-INITIERA-NYCKLAR-WDE6                                     
054900     MOVE +1 TO IX                                                        
055000     PERFORM IMS-GU-E601-PRODNR-UNIK                                      
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300       IF VORD-IDDC-EXP NOT = SPACE                                       
055400          MOVE JA TO BOUNCE-SW                                            
055500          IF VORD-IDDC-EXP = WC-CDC-SE                                    
055600*            *BOUNCE-CDC = 11 Global Export                               
055700*            *ORDERLINE IS NOT FROM DC 11                                 
055800             IF VORD-IDDC = IDDC-WS                                       
055900                MOVE JA  TO AVERAGECOST-SW                                
056000             ELSE                                                         
056100                IF VORD-IDDC-EXP NOT = IDDC-WS                            
056200                   MOVE NEJ TO VISA-ORDER-SW                              
056300                ELSE                                                      
056400                   MOVE NEJ TO AVERAGECOST-SW                             
056500                   IF VORD-IDDC-EXP = IDDC-WS AND                         
056600                      VORD-KVKOLLI-FAKT = 0                               
056700                      MOVE NEJ TO VISA-ORDER-SW                           
056800                   END-IF                                                 
056900                END-IF                                                    
057000             END-IF                                                       
057100          ELSE                                                            
057200*            *BOUNCE-DC NOT 11, MEANS VOR CHINA INDIA                     
057300*            *ORDERLINE IS FROM DC=11                                     
057400             IF VORD-IDDC = IDDC-WS                                       
057500                MOVE NEJ TO AVERAGECOST-SW                                
057600             ELSE                                                         
057700                IF VORD-IDDC-EXP NOT = IDDC-WS                            
057800                   MOVE NEJ TO VISA-ORDER-SW                              
057900                ELSE                                                      
058000                   MOVE JA  TO AVERAGECOST-SW                             
058100                   IF VORD-IDDC-EXP = IDDC-WS AND                         
058200                      VORD-KVKOLLI-FAKT = 0                               
058300                      MOVE NEJ TO VISA-ORDER-SW                           
058400                   END-IF                                                 
058500                END-IF                                                    
058600             END-IF                                                       
058700          END-IF                                                          
058800       ELSE                                                               
058900          IF VORD-SUORDV-EXP > 0                                          
059000             MOVE JA  TO AVERAGECOST-SW                                   
059100          ELSE                                                            
059200             MOVE NEJ TO AVERAGECOST-SW                                   
059300          END-IF                                                          
059400       END-IF                                                             
059500       IF VISA-ORDER                                                      
059600         PERFORM DB-FLYTTA-HUVUD                                          
059700         PERFORM DC-HAEMTA-ORDERNR                                        
059800         PERFORM IMS-GNP-E611-KOLLI                                       
059900         PERFORM DD-SPARA-ENTER-NYCKLAR                                   
060000                                                                          
060100         PERFORM UNTIL IX > MAX-RADER + 1                                 
060200           IF SEGMENT-FINNS                                               
060300             PERFORM DE-BEHANDLA-KOLLI-RADER                              
060400                                                                          
060500           ELSE                                                           
060600             IF IX = +1                                                   
060700               MOVE FEL-3 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1               
060800             END-IF                                                       
060900             PERFORM S02-BLANKA-RADER                                     
061000             MOVE 99999 TO MOD-IDKOLLI-NEXT                               
061100           END-IF                                                         
061200           ADD +1 TO IX                                                   
061300         END-PERFORM                                                      
061400       ELSE                                                               
061500         MOVE FEL-2 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                     
061600         PERFORM UNTIL IX > MAX-RADER                                     
061700           PERFORM S02-BLANKA-RADER                                       
061800           ADD +1 TO IX                                                   
061900         END-PERFORM                                                      
062000       END-IF                                                             
062100                                                                          
062200     ELSE                                                                 
062300       MOVE FEL-2 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                       
062400       PERFORM UNTIL IX > MAX-RADER                                       
062500         PERFORM S02-BLANKA-RADER                                         
062600         ADD +1 TO IX                                                     
062700       END-PERFORM                                                        
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 DA-INITIERA-NYCKLAR-WDE6 SECTION.                                        
063200                                                                          
063300     IF MFS-IDPFK = '8'                                                   
063400       MOVE MID-IDKOLLI-NEXT   TO W-IDKOLLI-MIN                           
063500     END-IF                                                               
063600                                                                          
063700     IF MFS-IDPFK = ' ' OR '9'                                            
063800       MOVE MID-IDKOLLI-ENTER  TO W-IDKOLLI-MIN                           
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 DB-FLYTTA-HUVUD SECTION.                                                 
064300                                                                          
064400     MOVE VORD-IDDISTR                 TO IDDISTR-NUM                     
064500     MOVE VORD-IDKUNDNR                TO IDKUNDNR-NUM                    
064600     MOVE VORD-KVKOLLI                 TO MOD-KVKOLLI                     
064700     MOVE VORD-KVKOLLI-FAKT            TO MOD-KVKOLLI-FAKT                
064800     MOVE VORD-KVKOLLI-LAST            TO MOD-KVKOLLI-LAST                
064900     MOVE VORD-KVORDRAD                TO MOD-KVORDRAD-TOT                
065000     MOVE VORD-IDDC                    TO VORD-WS-IDDC                    
065100                                                                          
065200     IF SEC-KDSVAR = 2 OR 6                                               
065300        MOVE MFS-RENSA-FAELT           TO MOD-SUORDV                      
065400     ELSE                                                                 
065500        IF DIST79-DEALER-PRICE OR                                         
065520           DIST79-ECOM-PRICE                                              
065600          COMPUTE HELP-SUMMA = VORD-SUORDV-LOC +                          
065700                               VORD-SUORDV-LOCPREL                        
065800          MOVE HELP-SUMMA              TO MOD-SUORDV                      
065900          IF VORD-SUORDV-LOCPREL = +0                                     
066000            MOVE ' '                   TO MOD-TEASTRIX                    
066100          ELSE                                                            
066200            MOVE '*'                   TO MOD-TEASTRIX                    
066300          END-IF                                                          
066400        ELSE                                                              
066500          IF AVERAGECOST                                                  
066600             MOVE VORD-SUORDV-EXP     TO MOD-SUORDV                       
066700          ELSE                                                            
066800             IF VORD-IDDC-EXP NOT = SPACE                                 
066900                IF VORD-IDDC-EXP = WC-CDC-SE                              
067000                  IF VORD-KVKOLLI-FAKT > 0                                
067100*                  *Second invoice for bounce global export               
067200                    MOVE VORD-SUORDV     TO MOD-SUORDV                    
067300                  ELSE                                                    
067400                    MOVE ZERO            TO MOD-SUORDV                    
067500                  END-IF                                                  
067600                ELSE                                                      
067700*                  *First invoice for bounce global export                
067800                  MOVE VORD-SUORDV       TO MOD-SUORDV                    
067900                END-IF                                                    
068000             ELSE                                                         
068100                MOVE VORD-SUORDV       TO MOD-SUORDV                      
068200             END-IF                                                       
068300          END-IF                                                          
068400          MOVE ' '                     TO MOD-TEASTRIX                    
068500        END-IF                                                            
068600     END-IF                                                               
068700                                                                          
068800     MOVE VORD-VLORDBTO      TO WS-VLORDBTO                               
068900     MOVE VORD-VKORDBTO      TO WS-VKORDBTO                               
069000     IF US-MEASUREMENT                                                    
069100       COMPUTE WS-VLORDBTO ROUNDED =                                      
069200               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
069300       COMPUTE WS-VKORDBTO ROUNDED =                                      
069400               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
069500     END-IF                                                               
069600     MOVE WS-VLORDBTO        TO MOD-VLORDBTO                              
069700     MOVE WS-VKORDBTO        TO MOD-VKORDBTO                              
069800                                                                          
069900     MOVE VORD-IDDISTR                 TO TEST-IDDISTR                    
070000     .                                                                    
070100     EJECT                                                                
070200 DC-HAEMTA-ORDERNR       SECTION.                                         
070300                                                                          
070400     MOVE LOW-VALUE       TO W-WDE4A1KY-MIN-X                             
070500     MOVE HIGH-VALUE      TO W-WDE4A1KY-MAX-X                             
070600                                                                          
070700     MOVE VORD-IDDISTR    TO W-IDDISTR-MIN                                
070800                             W-IDDISTR-MAX                                
070900                                                                          
071000     MOVE VORD-IDKUNDNR   TO W-IDKUNDNR-MIN                               
071100                             W-IDKUNDNR-MAX                               
071200                                                                          
071300     PERFORM IMS-GU-E4A1-ORDNR                                            
071400     IF SEGMENT-FINNS                                                     
071500         PERFORM UNTIL (VORD-IDPRODNR = SEQA-IDPRODNR) OR                 
071510                       SEGMENT-SAKNAS OR                                  
071520                       BASEN-SLUT                                         
071600           PERFORM IMS-GN-E4A1-ORDNR                                      
071700         END-PERFORM                                                      
071800         MOVE SEQA-IDDISTR  TO IDDISTR-NUM                                
071900         MOVE IDDISTR-WS    TO MSGI-IDDISTR                               
072000                               MOD-IDDISTR-UT                             
072100         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
072200                                                                          
072300         MOVE SEQA-IDKUNDNR TO IDKUNDNR-NUM                               
072400         MOVE IDKUNDNR-WS   TO MSGI-IDKUNDNR                              
072500                               MOD-IDKUNDNR-UT                            
072600         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
072700                                                                          
072800         MOVE '00'          TO MSGI-IDKUNDRF (1:2)                        
072900         MOVE SEQA-IDORDNR5 TO MSGI-IDKUNDRF (3:7)                        
073000                               MOD-IDORDNR7-UT                            
073100         INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE          
073200                                                                          
073300         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
073400     ELSE                                                                 
073500        CALL FELLOG                                                       
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 DD-SPARA-ENTER-NYCKLAR SECTION.                                          
074000                                                                          
074100     IF SEGMENT-FINNS                                                     
074200       MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-ENTER                            
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 DE-BEHANDLA-KOLLI-RADER SECTION.                                         
074700                                                                          
074800     IF DIST03-SVERIGE-2 AND                                              
074900        KOLLI-IDKOLLI > +99000                                            
075000       CONTINUE                                                           
075100                                                                          
075200     ELSE                                                                 
075300       IF IX NOT > MAX-RADER                                              
075400         IF KOLLI-IDKOLLI NOT < ZERO                                      
075500           PERFORM S01-FLYTTA-KOLLI-RADER                                 
075600                                                                          
075700         ELSE                                                             
075800           SUBTRACT +1 FROM IX                                            
075900         END-IF                                                           
076000         PERFORM IMS-GNP-E611-KOLLI                                       
076100                                                                          
076200       ELSE                                                               
076300         MOVE KOLLI-IDKOLLI       TO MOD-IDKOLLI-NEXT                     
076400         MOVE VORD-IDPRODNR       TO MOD-IDPRODNR-NEXT                    
076500         MOVE MED-1 (SPRAK-INDEX) TO MOD-MESSAGE-RAD23                    
076600       END-IF                                                             
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 E-LAES-WDE4A      SECTION.                                               
077100                                                                          
077200     MOVE JA TO VISA-ORDER-SW                                             
077300     MOVE NEJ TO BOUNCE-SW                                                
077400     PERFORM EA-INITIERA-NYCKLAR-WDE4                                     
077500     MOVE +1 TO IX                                                        
077600     PERFORM IMS-GU-E4A1-ORDNR                                            
077700                                                                          
077800     IF SEGMENT-FINNS                                                     
077900       MOVE SEQA-IDPRODNR TO MOD-IDPRODNR-ENTER                           
078000       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
078100         IF SPAR-IDPRODNR NOT = SEQA-IDPRODNR                             
078200           MOVE SEQA-IDPRODNR TO W-IDPRODNR                               
078300           PERFORM IMS-GU-E601-PRODNR                                     
078400           IF VORD-IDDC-EXP NOT = SPACE                                   
078500            IF VORD-IDDC-EXP = WC-CDC-SE                                  
078600*              *BOUNCE-CDC = 11 Global Export                             
078700*              *ORDERLINE IS NOT FROM DC 11                               
078800               MOVE JA TO BOUNCE-SW                                       
078900               IF VORD-IDDC = IDDC-WS                                     
079000                  MOVE JA  TO AVERAGECOST-SW                              
079100               ELSE                                                       
079200                  IF VORD-IDDC-EXP NOT = IDDC-WS                          
079300                     MOVE NEJ TO VISA-ORDER-SW                            
079400                  ELSE                                                    
079500                      MOVE NEJ TO AVERAGECOST-SW                          
079600                      IF VORD-IDDC-EXP = IDDC-WS AND                      
079700                         VORD-KVKOLLI-FAKT = 0                            
079800                         MOVE NEJ TO VISA-ORDER-SW                        
079900                      END-IF                                              
080000                  END-IF                                                  
080100               END-IF                                                     
080200            ELSE                                                          
080300*               *BOUNCE-DC NOT 11, MEANS VOR CHINA INDIA                  
080400*               *ORDERLINE IS FROM DC 11                                  
080500                IF VORD-IDDC = IDDC-WS                                    
080600                   MOVE NEJ TO AVERAGECOST-SW                             
080700                ELSE                                                      
080800                   IF VORD-IDDC-EXP NOT = IDDC-WS                         
080900                      MOVE NEJ TO VISA-ORDER-SW                           
081000                   ELSE                                                   
081100                     MOVE JA  TO AVERAGECOST-SW                           
081200                     IF VORD-IDDC-EXP = IDDC-WS AND                       
081300                        VORD-KVKOLLI-FAKT = 0                             
081400                        MOVE NEJ TO VISA-ORDER-SW                         
081500                     END-IF                                               
081600                   END-IF                                                 
081700                END-IF                                                    
081800            END-IF                                                        
081900           ELSE                                                           
082000              IF VORD-SUORDV-EXP > 0                                      
082100                 MOVE JA  TO AVERAGECOST-SW                               
082200              ELSE                                                        
082300                 MOVE NEJ TO AVERAGECOST-SW                               
082400              END-IF                                                      
082500           END-IF                                                         
082600           IF VISA-ORDER                                                  
082700             PERFORM EB-BERAEKNA-HUVUD-TOTALER                            
082800                                                                          
082900             IF IX > MAX-RADER                                            
083000               PERFORM EC-SPARA-NYCKLAR-FOER-PF8                          
083100                                                                          
083200             ELSE                                                         
083300               PERFORM ED-LAES-BEHANDLA-KOLLI                             
083400             END-IF                                                       
083500           END-IF                                                         
083600         END-IF                                                           
083700         MOVE SEQA-IDPRODNR TO SPAR-IDPRODNR                              
083800         PERFORM IMS-GN-E4A1-ORDNR                                        
083900       END-PERFORM                                                        
084000                                                                          
084100       IF IX = +1                                                         
084200         MOVE FEL-3 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                     
084300       END-IF                                                             
084400       PERFORM EE-VISA-BILD                                               
084500                                                                          
084600     ELSE                                                                 
084700       PERFORM EF-VISA-ORDER-SAKNAS                                       
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 EA-INITIERA-NYCKLAR-WDE4 SECTION.                                        
085200                                                                          
085300     IF MFS-IDPFK = '8'                                                   
085400       MOVE MID-IDPRODNR-NEXT TO W-IDPRODNR-MIN                           
085500     END-IF                                                               
085600                                                                          
085700     IF MFS-IDPFK = ' ' OR '9'                                            
085800       MOVE MID-IDPRODNR-ENTER TO W-IDPRODNR-MIN                          
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200 EB-BERAEKNA-HUVUD-TOTALER SECTION.                                       
086300                                                                          
086400     MOVE VORD-IDDISTR      TO IDDISTR-NUM                                
086500                               TEST-IDDISTR                               
086600     MOVE VORD-IDKUNDNR     TO IDKUNDNR-NUM                               
086700                                                                          
086800     ADD VORD-KVKOLLI       TO SPAR-KVKOLLI                               
086900     ADD VORD-KVKOLLI-FAKT  TO SPAR-KVKOLLI-FAKT                          
087000     ADD VORD-KVKOLLI-LAST  TO SPAR-KVKOLLI-LAST                          
087100     ADD VORD-KVORDRAD      TO SPAR-KVORDRAD                              
087200     IF AVERAGECOST                                                       
087300        ADD VORD-SUORDV-EXP TO SPAR-SUORDV                                
087400     ELSE                                                                 
087500        IF BOUNCEORDER                                                    
087600           IF VORD-IDDC-EXP  = WC-CDC-SE                                  
087700*            *Bounce-dc 11                                                
087800             IF VORD-KVKOLLI-FAKT > 0                                     
087900*               *Second invoice bounce global export                      
088000                ADD VORD-SUORDV  TO SPAR-SUORDV                           
088100             END-IF                                                       
088200           ELSE                                                           
088300*            *Bounce-dc not 11                                            
088400             ADD VORD-SUORDV  TO SPAR-SUORDV                              
088500           END-IF                                                         
088600        ELSE                                                              
088700           ADD VORD-SUORDV    TO SPAR-SUORDV                              
088800        END-IF                                                            
088900     END-IF                                                               
089000     ADD VORD-SUORDV-LOC    TO SPAR-SUORDV-LOC                            
089100     ADD VORD-SUORDV-LOCPREL TO SPAR-SUORDV-LOCPREL                       
089200     ADD VORD-VKORDBTO      TO SPAR-VKORDBTO                              
089300     ADD VORD-VLORDBTO      TO SPAR-VLORDBTO                              
089400                                                                          
089500     IF AVERAGECOST                                                       
089600        MOVE VORD-KDVALISO-EXP TO SPAR-KDVALISO                           
089700     ELSE                                                                 
089800        IF BOUNCEORDER                                                    
089900           IF VORD-IDDC-EXP = WC-CDC-SE                                   
090000*            *Bounce dc=11                                                
090100             IF VORD-KVKOLLI-FAKT > 0                                     
090200*               *Second invoice Global export                             
090300                MOVE VORD-KDVALISO   TO SPAR-KDVALISO                     
090400             ELSE                                                         
090500                MOVE SPACE           TO SPAR-KDVALISO                     
090600             END-IF                                                       
090700           ELSE                                                           
090800*            *Bounce dc not 11                                            
090900             MOVE VORD-KDVALISO      TO SPAR-KDVALISO                     
091000           END-IF                                                         
091100        ELSE                                                              
091200           MOVE VORD-KDVALISO        TO SPAR-KDVALISO                     
091300        END-IF                                                            
091400     END-IF                                                               
091500                                                                          
091600     MOVE VORD-IDDISTR      TO TEST-IDDISTR                               
091700     MOVE VORD-IDDC         TO VORD-WS-IDDC                               
091800     .                                                                    
091900     EJECT                                                                
092000 EC-SPARA-NYCKLAR-FOER-PF8 SECTION.                                       
092100                                                                          
092200     IF IX > MAX-RADER                                                    
092300       IF MOD-IDPRODNR-NEXT = ZERO                                        
092400                                                                          
092500         MOVE SEQA-IDPRODNR       TO MOD-IDPRODNR-NEXT                    
092600         MOVE MED-1 (SPRAK-INDEX) TO MOD-MESSAGE-RAD23                    
092700       END-IF                                                             
092800     END-IF                                                               
092900     .                                                                    
093000     EJECT                                                                
093100 ED-LAES-BEHANDLA-KOLLI SECTION.                                          
093200                                                                          
093300     IF (IDKOLLI-WS > ZERO AND VORD-FLDIRLEV = NEJ) OR                    
093400         IDKOLLI-WS = ZERO                                                
093500       PERFORM IMS-GNP-E611-KOLLI                                         
093600                                                                          
093700       IF SEGMENT-FINNS                                                   
093800                                                                          
093900         PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-RADER                   
094000           IF IX = +1                                                     
094100             PERFORM EDA-KOLLA-OM-RAETT-KOLLI                             
094200           END-IF                                                         
094300                                                                          
094400           IF ALLT-OK                                                     
094500             PERFORM EDB-BEHANDLA-KOLLI-RADER                             
094600                                                                          
094700           ELSE                                                           
094800             PERFORM IMS-GNP-E611-KOLLI                                   
094900           END-IF                                                         
095000         END-PERFORM                                                      
095100       END-IF                                                             
095200                                                                          
095300     END-IF                                                               
095400     .                                                                    
095500     EJECT                                                                
095600 EDA-KOLLA-OM-RAETT-KOLLI SECTION.                                        
095700                                                                          
095800     MOVE NEJ TO ALLT-SW                                                  
095900                                                                          
096000     IF MFS-IDPFK = '7'                                                   
096100       MOVE JA TO ALLT-SW                                                 
096200                                                                          
096300     ELSE                                                                 
096400       IF MFS-IDPFK = '8'                                                 
096500         IF VORD-IDPRODNR    = MID-IDPRODNR-NEXT AND                      
096600           (KOLLI-IDKOLLI    = MID-IDKOLLI-NEXT  OR                       
096700            MID-IDKOLLI-NEXT = ZERO)                                      
096800           MOVE JA TO ALLT-SW                                             
096900         END-IF                                                           
097000                                                                          
097100       ELSE                                                               
097200         IF MFS-IDPFK = ' ' OR '9'                                        
097300           IF  VORD-IDPRODNR     = MID-IDPRODNR-ENTER AND                 
097400              (KOLLI-IDKOLLI = MID-IDKOLLI-ENTER   OR                     
097500               MID-IDKOLLI-ENTER = ZERO)                                  
097600             MOVE JA TO ALLT-SW                                           
097700           END-IF                                                         
097800         END-IF                                                           
097900       END-IF                                                             
098000     END-IF                                                               
098100     .                                                                    
098200     EJECT                                                                
098300 EDB-BEHANDLA-KOLLI-RADER SECTION.                                        
098400                                                                          
098500     PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-RADER                       
098600        IF DIST03-SVERIGE-2 AND KOLLI-IDKOLLI > +99000                    
098700           CONTINUE                                                       
098800        ELSE                                                              
098900           IF IX = +1                                                     
099000* FELFIX                                                                  
099100* 916 CASE MISSING UPPSTOD IBLAND VID PF9                                 
099200* FÖRSTA KOLLIT VAR DÅ DDGS, DETTA VERKAR AVHJÄLPA DET FELET              
099300                                                                          
099400              MOVE VORD-IDPRODNR TO MOD-IDPRODNR-ENTER                    
099500* FELFIX-END                                                              
099600              MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-ENTER                     
099700           END-IF                                                         
099800           PERFORM S01-FLYTTA-KOLLI-RADER                                 
099900           ADD +1 TO IX                                                   
100000        END-IF                                                            
100100        PERFORM IMS-GNP-E611-KOLLI                                        
100200     END-PERFORM                                                          
100300                                                                          
100400     IF SEGMENT-FINNS                                                     
100500        MOVE KOLLI-IDKOLLI       TO MOD-IDKOLLI-NEXT                      
100600        MOVE VORD-IDPRODNR       TO MOD-IDPRODNR-NEXT                     
100700        MOVE MED-1 (SPRAK-INDEX) TO MOD-MESSAGE-RAD23                     
100800                                                                          
100900     ELSE                                                                 
101000        MOVE ZERO                TO MOD-IDKOLLI-NEXT                      
101100        MOVE ZERO                TO MOD-IDPRODNR-NEXT                     
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 EE-VISA-BILD SECTION.                                                    
101600                                                                          
101700     MOVE SPAR-KVKOLLI                 TO MOD-KVKOLLI                     
101800     MOVE SPAR-KVKOLLI-FAKT            TO MOD-KVKOLLI-FAKT                
101900     MOVE SPAR-KVKOLLI-LAST            TO MOD-KVKOLLI-LAST                
102000     MOVE SPAR-KVORDRAD                TO MOD-KVORDRAD-TOT                
102100                                                                          
102200     IF SEC-KDSVAR = 2 OR 6                                               
102300        MOVE MFS-RENSA-FAELT           TO MOD-SUORDV                      
102400     ELSE                                                                 
102500        IF DIST79-DEALER-PRICE OR                                         
102520           DIST79-ECOM-PRICE                                              
102600          COMPUTE HELP-SUMMA = SPAR-SUORDV-LOC +                          
102700                               SPAR-SUORDV-LOCPREL                        
102800          MOVE HELP-SUMMA              TO MOD-SUORDV                      
102900          IF SPAR-SUORDV-LOCPREL = +0                                     
103000            MOVE ' '                   TO MOD-TEASTRIX                    
103100          ELSE                                                            
103200            MOVE '*'                   TO MOD-TEASTRIX                    
103300          END-IF                                                          
103400        ELSE                                                              
103500          MOVE SPAR-SUORDV             TO MOD-SUORDV                      
103600          MOVE ' '                     TO MOD-TEASTRIX                    
103700        END-IF                                                            
103800     END-IF                                                               
103900                                                                          
104000     MOVE SPAR-VKORDBTO      TO WS-VKORDBTO                               
104100     MOVE SPAR-VLORDBTO      TO WS-VLORDBTO                               
104200                                                                          
104300     IF US-MEASUREMENT                                                    
104400       COMPUTE WS-VLORDBTO ROUNDED =                                      
104500               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
104600       COMPUTE WS-VKORDBTO ROUNDED =                                      
104700               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
104800     END-IF                                                               
104900                                                                          
105000     MOVE WS-VLORDBTO        TO MOD-VLORDBTO                              
105100     MOVE WS-VKORDBTO        TO MOD-VKORDBTO                              
105200     .                                                                    
105300     EJECT                                                                
105400 EF-VISA-ORDER-SAKNAS SECTION.                                            
105500                                                                          
105600     MOVE FEL-2 (SPRAK-INDEX) TO MOD-MESSAGE-RAD1                         
105700                                                                          
105800     PERFORM UNTIL IX > MAX-RADER                                         
105900        PERFORM S02-BLANKA-RADER                                          
106000        ADD +1 TO IX                                                      
106100     END-PERFORM                                                          
106200     .                                                                    
106300     EJECT                                                                
106400 F-SPARA-NYCKLAR SECTION.                                                 
106500                                                                          
106600     IF PRODNR-OK                    =  JA                                
106700        MOVE IDPRODNR-WS             TO MOD-IDPRODNR-UT                   
106800        INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE           
106900     ELSE                                                                 
107000        MOVE MFS-RENSA-FAELT            TO MOD-IDPRODNR-UT                
107100                                                                          
107200        MOVE MSGI-IDDISTR               TO MOD-IDDISTR-UT                 
107300        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
107400                                                                          
107500        MOVE MSGI-IDKUNDNR              TO MOD-IDKUNDNR-UT                
107600        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
107700                                                                          
107800        MOVE MSGI-IDKUNDRF(1:7)         TO MOD-IDORDNR7-UT                
107900        INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE           
108000     END-IF                                                               
108100                                                                          
108200     MOVE IDKOLLI-WS                 TO MOD-IDKOLLI-UT                    
108300     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
108400                                                                          
108500     MOVE IDARTNR-WS                 TO MOD-IDARTNR-UT                    
108600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
108700                                                                          
108800     .                                                                    
108900     EJECT                                                                
109000 S01-FLYTTA-KOLLI-RADER SECTION.                                          
109100                                                                          
109200     IF IDKOLLI-WS > ZERO                                                 
109300       MOVE KOLLI-IDSHIPM              TO MOD-IDSHIPM                     
109400       IF ENGLISH-TEXT                                                    
109500         MOVE 'SHIPMENT  '             TO MOD-TETEXT10                    
109600       ELSE                                                               
109700         MOVE 'SKEPPNING '             TO MOD-TETEXT10                    
109800       END-IF                                                             
109900     ELSE                                                                 
110000       MOVE SPACE                      TO MOD-TETEXT10                    
110100       MOVE ZERO                       TO MOD-IDSHIPM                     
110200     END-IF                                                               
110300                                                                          
110400     IF NDC                                                               
110500       MOVE SPACE                      TO MOD-ADKOLLI-GRP (IX)            
110600       MOVE KOLLI-IDTRPTNR             TO ADFLOMR2-4                      
110700       MOVE ADFLOMR                    TO MOD-ADFLOMR  (IX)               
110800     ELSE                                                                 
110900       IF VORD-NDC-PACIFIC OR VORD-NDC-NX OR VORD-NDC-NS                  
111000         MOVE KOLLI-ADFLOMR            TO ADFLOMR2-4                      
111100         MOVE ADFLOMR                  TO MOD-ADFLOMR (IX)                
111200       ELSE                                                               
111300         MOVE KOLLI-ADFLGEO            TO MOD-ADFLGEO (IX)                
111400         MOVE KOLLI-ADFLOMR            TO ADFLOMR2-4                      
111500         MOVE ADFLOMR                  TO MOD-ADFLOMR (IX)                
111600         MOVE KOLLI-ADRUTNIV           TO MOD-ADRUTNIV (IX)               
111700         MOVE KOLLI-ADVMODUL           TO MOD-ADVMODUL (IX)               
111800       END-IF                                                             
111900     END-IF                                                               
112000     MOVE KOLLI-IDPLOCK                TO MOD-IDPLOCK (IX)                
112100     MOVE KOLLI-KVORDRAD               TO MOD-KVORDRAD (IX)               
112200     IF KOLLI-KDFARLIG-KOLLI = 4                                          
112300     OR KOLLI-KDFARLIG-KOLLI = 7                                          
112400       IF ENGLISH-TEXT                                                    
112500        MOVE 'Y '                      TO MOD-TETEXTX2 (IX)               
112600       ELSE                                                               
112700        MOVE 'JA'                      TO MOD-TETEXTX2 (IX)               
112800       END-IF                                                             
112900     ELSE                                                                 
113000        MOVE SPACE                     TO MOD-TETEXTX2 (IX)               
113100     END-IF                                                               
113200     MOVE KOLLI-VKORDBTO-KOLLI         TO WS-VKORDBTO                     
113300     MOVE KOLLI-VLORDBTO-KOLLI         TO WS-VLORDBTO                     
113400     MOVE KOLLI-TILASTN                TO MOD-TILASTN (IX)                
113500                                                                          
113600     IF US-MEASUREMENT                                                    
113700       COMPUTE WS-VLORDBTO ROUNDED =                                      
113800               WS-VLORDBTO * CONV-M3-TO-FT3 END-COMPUTE                   
113900       COMPUTE WS-VKORDBTO ROUNDED =                                      
114000               WS-VKORDBTO * CONV-KG-TO-LB  END-COMPUTE                   
114100     END-IF                                                               
114200     MOVE WS-VKORDBTO                  TO MOD-VKORDBTO-KOLLI (IX)         
114300                                                                          
114400     IF MFS-SPLIT                                                         
114500       MOVE SPACE                      TO MOD-ADKOLLI-GRP (IX)            
114600       MOVE KOLLI-IDKOLLI-SAMP         TO ADFLOMR1-5                      
114700       MOVE ADFLOMR1-5                 TO MOD-ADFLOMR (IX)                
114800       MOVE KOLLI-IDKOLLI              TO MOD-IDKOLLI (IX)                
114900       MOVE SPACE                      TO MOD-ADFLGEO (IX)                
115000*      MOVE ZERO                       TO MOD-ADRUTNIV(IX)                
115100       IF AVERAGECOST AND BOUNCEORDER                                     
115200         MOVE KOLLI-TIFAKTID-EXP       TO MOD-TIFAKT (IX)                 
115300       ELSE                                                               
115400         MOVE KOLLI-TIFAKTID           TO MOD-TIFAKT (IX)                 
115500       END-IF                                                             
115600       MOVE KOLLI-TIPACTID             TO MOD-TIPACKN (IX)                
115700       MOVE KOLLI-KDKOLLI              TO MOD-KDKOLLI (IX)                
115800     ELSE                                                                 
115900       MOVE KOLLI-IDKOLLI              TO MOD-IDKOLLI (IX)                
116000       IF AVERAGECOST AND BOUNCEORDER                                     
116100          MOVE KOLLI-TIFAKT-EXP        TO MOD-TIFAKT (IX)                 
116200       ELSE                                                               
116300          MOVE KOLLI-TIFAKT            TO MOD-TIFAKT (IX)                 
116400       END-IF                                                             
116500       MOVE KOLLI-TIPACKN              TO MOD-TIPACKN (IX)                
116600       MOVE WS-VLORDBTO                TO MOD-VLORDBTO-KOLLI (IX)         
116700                                                                          
116800     END-IF                                                               
116900     .                                                                    
117000     EJECT                                                                
117100 S02-BLANKA-RADER SECTION.                                                
117200     SKIP2                                                                
117300     IF IX < 12                                                           
117400        MOVE MFS-RENSA-FAELT           TO MOD-RAD (IX)                    
117500     END-IF                                                               
117600     .                                                                    
117700     EJECT                                                                
117800* IMS SEKTIONER                                                           
117900     SKIP3                                                                
118000 IMS-GET-MSG SECTION.                                                     
118100     SKIP2                                                                
118200     MOVE '  QC' TO GODK-STATUSKODER                                      
118300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
118400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     SKIP3                                                                
118700     .                                                                    
118800 IMS-INSERT-MSG SECTION.                                                  
118900     SKIP2                                                                
119000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
119100        MOVE '0' TO MFS-KDHUVOMR                                          
119200       IF MFS-SPLIT                                                       
119300          MOVE 'Kolli Sam.kolli'        TO MOD-VARHEAD                    
119400          MOVE 'Packtid Faktid Lasttid' TO MOD-VARHEAD2                   
119500          MOVE 'Kollikod'               TO MOD-VARHEAD3                   
119600       ELSE                                                               
119700          MOVE 'Kolli   Adress '        TO MOD-VARHEAD                    
119800          MOVE 'Packdat Fakdat Lastdat' TO MOD-VARHEAD2                   
119900          MOVE ' Bto-vol'               TO MOD-VARHEAD3                   
120000       END-IF                                                             
120100     ELSE                                                                 
120200       IF MFS-SPLIT                                                       
120300          MOVE ' Case Cons.case'        TO MOD-VARHEAD                    
120400          MOVE ' Packti  Invti  Loadti' TO MOD-VARHEAD2                   
120500          MOVE 'Cse code'               TO MOD-VARHEAD3                   
120600       ELSE                                                               
120700          MOVE ' Case    Adress'        TO MOD-VARHEAD                    
120800          MOVE 'Packdat Invdat Loaddat' TO MOD-VARHEAD2                   
120900          MOVE ' Gr. vol'               TO MOD-VARHEAD3                   
121000       END-IF                                                             
121100     END-IF                                                               
121200                                                                          
121300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
121400     MOVE SPACE TO GODK-STATUSKODER                                       
121500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
121600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
121700     PERFORM IMS-STATUSKONTROLL                                           
121800     .                                                                    
121900     EJECT                                                                
122000 IMS-GU-E4A1-ORDNR SECTION.                                               
122100     SKIP2                                                                
122200     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4A1KY-MIN-X                        
122300                    '&WDE4A1KY=<' W-WDE4A1KY-MAX-X ')'                    
122400            DELIMITED BY SIZE INTO SSA1                                   
122500     MOVE '  GE' TO GODK-STATUSKODER                                      
122600     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA-WDE4A1 SSA1              
122700     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
122800     PERFORM IMS-STATUSKONTROLL                                           
122900     .                                                                    
123000     EJECT                                                                
123100 IMS-GN-E4A1-ORDNR SECTION.                                               
123200     SKIP2                                                                
123300     STRING 'WDE4A1  (WDE4A1KY=>' W-WDE4A1KY-MIN-X                        
123400                    '&WDE4A1KY=<' W-WDE4A1KY-MAX-X ')'                    
123500            DELIMITED BY SIZE INTO SSA1                                   
123600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123700     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA-WDE4A1 SSA1              
123800     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
123900     PERFORM IMS-STATUSKONTROLL                                           
124000     .                                                                    
124100     EJECT                                                                
124200 IMS-GU-E601-PRODNR-UNIK SECTION.                                         
124300     SKIP2                                                                
124400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
124500            DELIMITED BY SIZE INTO SSA1                                   
124600     MOVE '  GE' TO GODK-STATUSKODER                                      
124700     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
124800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     EJECT                                                                
125200 IMS-GU-E601-PRODNR SECTION.                                              
125300     SKIP2                                                                
125400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
125500            DELIMITED BY SIZE INTO SSA1                                   
125600     MOVE '  ' TO GODK-STATUSKODER                                        
125700     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
125800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     EJECT                                                                
126200 IMS-GNP-E611-KOLLI SECTION.                                              
126300     SKIP2                                                                
126400     STRING 'WDE611  (IDKOLLI =>' W-IDKOLLI-MIN-X                         
126500                    '&IDKOLLI =<' W-IDKOLLI-MAX-X ')'                     
126600            DELIMITED BY SIZE INTO SSA1                                   
126700     MOVE '  GE' TO GODK-STATUSKODER                                      
126800     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-AREA-WDE611 SSA1              
126900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
127000     PERFORM IMS-STATUSKONTROLL                                           
127100     .                                                                    
127200     EJECT                                                                
127300 IMS-STATUSKONTROLL SECTION.                                              
127400     SKIP2                                                                
127500     SET STATUS-IX TO 1                                                   
127600     SEARCH GODK-STATUS AT END CALL FELLOG                                
127700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127800     END-SEARCH                                                           
127900     .                                                                    
