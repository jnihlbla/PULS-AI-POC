000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2015100.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   APRIL 1988.                                              
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700****************************************************************          
000800*                                                              *          
000900*                     NYPON-BILD 2151                          *          
001000*                                                              *          
001100*            UPPDATERING KONTO/SÄK.FAKT                        *          
001200*                                                              *          
001300*  BILD FÖR ANSKAFFNINGSAVDELNINGEN SOM KAN FRÅGA PÅ OCH       *          
001400*  UPPDATERA ETT PRODUKTSLAGS PROJEKTINFORMATION. DENNA INFO   *          
001500*  BESTÅR AV KONTO OCH SÄKERHETSFAKTOR FÖR C1 OCH C2.          *          
001600*                                                              *          
001700*                                                              *          
001800*  FUNKTIONER: ENTER                                           *          
001900*              PF7                                             *          
002000*              PF8                                             *          
002100*              PF11                                            *          
002200*                                                              *          
002300****************************************************************          
002400                                                                          
002500     EJECT                                                                
002600*    INDATA.                                                              
002700*        TRANSAKTION: W2T151                                              
002800*                     W2T151U                                             
002900*        MID:         W2I15101                                            
003000*    UTDATA.                                                              
003100*        MOD:         W2O15101                                            
003200*    SUBPROGRAM.                                                          
003300*        FELLOG                                                           
003400*        CBLTDLI                                                          
003500*        WKPSKONV                                                         
003600*        WDECEDIT                                                         
003700                                                                          
003800     EJECT                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP3                                                                
004100 DATA DIVISION.                                                           
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500*                                                                         
004600******************************************************************        
004700*          W O R K I N G  S T O R A G E  S E C T I O N           *        
004800******************************************************************        
004900*                                                                         
005000 77  PROGRAMNAMN                 PIC X(08)  VALUE 'W2015100'.             
005100 77  JA                          PIC X(01)  VALUE 'J'.                    
005200 77  NEJ                         PIC X(01)  VALUE 'N'.                    
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +427 COMP SYNC.         
005400 77  MAX-RAD-IND                 PIC  9(2)  VALUE  10.                    
005500                                                                          
005600 77  WS-KDPRODSL                 PIC X(02)  VALUE SPACE.                  
005700                                                                          
005800 77  WS-IDTRANS                  PIC X(04).                               
005900     88  EGEN-BILD                          VALUE '2151'.                 
006000                                                                          
006100     EJECT                                                                
006200*                                                                         
006300*01  -COPY WWPRODSL                                                       
006400*                                                                         
006500******************************************************************        
006600*                     S W I T C H A R                            *        
006700******************************************************************        
006800*                                                                         
006900 01  SWITCHAR.                                                            
007000     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
007100                                                                          
007200*                                                                         
007300******************************************************************        
007400*               D I V E R S E  S P A R F Ä L T                   *        
007500******************************************************************        
007600*                                                                         
007700 01  SPAR-DIVERSE.                                                        
007800     05  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.               
007900     05  SPAR-RESLJUST-C1            PIC 9V9    VALUE ZERO.               
008000     05  SPAR-RESLJUST-C2            PIC 9V9    VALUE ZERO.               
008100     05  SPAR-IDLKTO                 PIC 9(7)   VALUE ZERO.               
008200     05  FILLER REDEFINES SPAR-IDLKTO.                                    
008300         07 SPAR-IDLKTO-POS1-2       PIC 9(2).                            
008400         07 SPAR-IDLKTO-POS3-7       PIC 9(5).                            
008500     EJECT                                                                
008600*                                                                         
008700******************************************************************        
008800*           D Y N A M I S K A  S U B P R O G R A M               *        
008900******************************************************************        
009000*                                                                         
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200     05  WDECEDIT                PIC X(08)  VALUE 'WDECEDIT'.             
009300     05  WKPSKONV                PIC X(08)  VALUE 'WKPSKONV'.             
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     EJECT                                                                
009700******************************************************************        
009800*    F E L M E D D E L A N D E N                                          
009900******************************************************************        
010000*                                                                         
010100 01  MEDDELANDE.                                                          
010200     03  MED-1                   PIC X(32) VALUE                          
010300            'MER INFO PÅ NÄSTA SIDA          '.                           
010400     03  MED-2                   PIC X(32) VALUE                          
010500            'TRYCK PF11 FÖR UPPDATERING      '.                           
010600     03  MED-3                   PIC X(32) VALUE                          
010700            'UPPDATERING UTFÖRD              '.                           
010800     03  MED-4                   PIC X(40) VALUE                          
010900            'FELAKTIGT KONTO FÖR DETTA PRODUKTSLAG   '.                   
011000     03  FEL-1                   PIC X(40) VALUE                          
011100            'SIDA 1 VISAS, FANNS EJ FLER PROJEKT     '.                   
011200     03  FEL-2                   PIC X(32) VALUE                          
011300            'UPPLYSTA FÄLT FEL               '.                           
011400     03  FEL-3                   PIC X(32) VALUE                          
011500            'PRODUKTSLAG EJ NUMERISKT        '.                           
011600     03  FEL-4                   PIC X(32) VALUE                          
011700            'PRODUKTSLAG SAKNAS              '.                           
011800     03  FEL-5                   PIC X(32) VALUE                          
011900            'PRODUKTSLAGET SAKNAR PROJEKT    '.                           
012000     EJECT                                                                
012100*                                                                         
012200******************************************************************        
012300*                    C O P Y T E X T E R    (DYNAMISKA ANROP)    *        
012400******************************************************************        
012500*                                                                         
012600 01  IMS-WS-1.                                                            
012700     03  FILLER                  PIC X(16)   VALUE 'RDEC-AREA'.           
012800     SKIP3                                                                
012900*01  -COPY WDECAREA                                                       
013000     SKIP3                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'RKPS-AREA'.           
013200     SKIP3                                                                
013300*01  -COPY WKPSAREA                                                       
013400     EJECT                                                                
013500*                                                                         
013600******************************************************************        
013700*                    M I D-C O P Y T E X T                       *        
013800******************************************************************        
013900*                                                                         
014000*                        ****    MFS OCH SKÄRMHANTERING                   
014100 01  IMS-WS-2.                                                            
014200     03  FILLER                  PIC X(16)   VALUE 'MID-COPY'.            
014300     SKIP3                                                                
014400*01  MID -COPY W2I15101                                                   
014500     EJECT                                                                
014600*                                                                         
014700******************************************************************        
014800*                    M S G - A R E A                             *        
014900******************************************************************        
015000*                                                                         
015100 01  IMS-WS-3.                                                            
015200     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
015300     SKIP3                                                                
015400*01  -COPY WMSGAREA                                                       
015500     EJECT                                                                
015600*                                                                         
015700******************************************************************        
015800*                    M O D-C O P Y T E X T                       *        
015900******************************************************************        
016000*                                                                         
016100*01  IMS-WS-4.                                                            
016200*    03  FILLER                  PIC X(16)   VALUE 'MOD-COPY'.            
016300     SKIP3                                                                
016400*    03  MOD -COPY W2O15101  -RED MSG-AREA.                               
016500     EJECT                                                                
016600*                                                                         
016700******************************************************************        
016800*                    M F S - A R E A                             *        
016900******************************************************************        
017000*                                                                         
017100 01  IMS-WS-5.                                                            
017200     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
017300     SKIP3                                                                
017400*01  -COPY WMFSAREA.                                                      
017500     EJECT                                                                
017600*                                                                         
017700******************************************************************        
017800*              N Y C K L A R  T I L L  D L I                     *        
017900******************************************************************        
018000*                                                                         
018100 01  NYCKLAR-TILL-DLI.                                                    
018200     03  W-1131KEY-X.                                                     
018300          05  FILLER           PIC X(04)  VALUE '1131'.                   
018400          05  W-KDPRODSL       PIC S9(3)  VALUE ZERO COMP-3.              
018500          05  FILLER           PIC X(24)  VALUE LOW-VALUE.                
018600                                                                          
018700     03  W-1132KEY-X.                                                     
018800          05  W-IDPROJK        PIC X(04)  VALUE SPACE.                    
018900          05  W-IDPROJOBJ      PIC X(04)  VALUE SPACE.                    
019000          05  W-IDPROJ         PIC X(04)  VALUE SPACE.                    
019100          05  FILLER           PIC X(03)  VALUE LOW-VALUE.                
019200                                                                          
019300     EJECT                                                                
019400*                                                                         
019500******************************************************************        
019600*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
019700******************************************************************        
019800*                                                                         
019900 01  IMS-WS-6.                                                            
020000     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
020100     SKIP3                                                                
020200*****                    **** STATUS-KOD FRÅN IMS                         
020300     03  STATUS-WS               PIC X(2).                                
020400         88  SEGMENT-FINNS                   VALUE '  '.                  
020500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
020600     SKIP3                                                                
020700     03  GODK-STATUSKODER.                                                
020800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
020900     SKIP3                                                                
021000 01  IMS-WS-7.                                                            
021100     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
021200     SKIP3                                                                
021300 01  SSA1                        PIC X(64).                               
021400 01  SSA2                        PIC X(64).                               
021500     EJECT                                                                
021600*                                                                         
021700******************************************************************        
021800*            I M S  F U N K T I O N S K O D E R                  *        
021900******************************************************************        
022000*                                                                         
022100*                                                                         
022200 01  IMS-WS-8.                                                            
022300     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
022400     SKIP3                                                                
022500*01  -COPY W0003                                                          
022600     EJECT                                                                
022700*                                                                         
022800******************************************************************        
022900*            D L I  I N P U T-O U T P U T A R E A                *        
023000******************************************************************        
023100*                                                                         
023200 01  IMS-WS-10.                                                           
023300     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA '.           
023400     SKIP3                                                                
023500 01  DLI-IO-AREA.                                                         
023600     03  IO-AREA                   PIC X(200) VALUE SPACE.                
023700     SKIP3                                                                
023800*                                                                         
023900******************************************************************        
024000*            S E G M E N T C O P Y T E X T E R                   *        
024100******************************************************************        
024200*                                                                         
024300*    03  WLXXAQ -COPY WDGX1131  -PRE WLXXAQ- -RED IO-AREA.                
024400     EJECT                                                                
024500*    03  WLXXAQ -COPY WDGX1132  -PRE WLXXAQ- -RED IO-AREA.                
024600     EJECT                                                                
024700*                                                                         
024800******************************************************************        
024900*            L I N K A G E  S E C T I O N                        *        
025000******************************************************************        
025100*                                                                         
025200 LINKAGE SECTION.                                                         
025300     SKIP2                                                                
025400*01  -COPY W0009     -PRE MSG-                                            
025500     EJECT                                                                
025600*01  -COPY W0008     -PRE WLXXAQ-                                         
025700         05  FILLER              PIC X.                                   
025800     EJECT                                                                
025900 PROCEDURE DIVISION USING MSG-PCB WLXXAQ-PCB.                             
026000     ENTRY 'DLITCBL' USING MSG-PCB WLXXAQ-PCB.                            
026100                                                                          
026200     PERFORM IMS-GET-MSG                                                  
026300     IF SEGMENT-FINNS                                                     
026400        PERFORM A-INIT-SPARA-INPUT                                        
026500        IF WS-KDPRODSL NUMERIC                                            
026600           MOVE WS-KDPRODSL   TO W-KDPRODSL                               
026700           PERFORM IMS-GET-WLXXAQ01                                       
026800           IF SEGMENT-FINNS                                               
026900              IF MFS-UPDATE  AND EGEN-BILD                                
027000                 PERFORM B-KOLLA-INPUT                                    
027100                 IF SW-INPUT-RAETT = JA                                   
027200                    PERFORM C-KOLLA-MOT-BASEN                             
027300                    IF SW-INPUT-RAETT = JA                                
027400                       PERFORM D-UPPDATERA-OCH-VISA-BILD                  
027500                    ELSE                                                  
027600                       MOVE FEL-2 TO MOD-TEMFSFEL                         
027700                    END-IF                                                
027800                 ELSE                                                     
027900                    MOVE FEL-2    TO MOD-TEMFSFEL                         
028000                 END-IF                                                   
028100              ELSE                                                        
028200                 IF (NOT EGEN-BILD)  OR (MFS-IDPFK = '7')                 
028300                    MOVE SPACE                 TO W-IDPROJ                
028400                                                  W-IDPROJOBJ             
028500                                                  W-IDPROJK               
028600                 ELSE                                                     
028700                    IF MFS-IDPFK = '8'                                    
028800                       MOVE MID-IDPROJ-11      TO W-IDPROJ                
028900                       MOVE MID-IDPROJOBJ-11   TO W-IDPROJOBJ             
029000                       MOVE MID-IDPROJK-11     TO W-IDPROJK               
029100                    ELSE                                                  
029200                       MOVE MID-IDPROJ-1       TO W-IDPROJ                
029300                       MOVE MID-IDPROJOBJ-1    TO W-IDPROJOBJ             
029400                       MOVE MID-IDPROJK-1      TO W-IDPROJK               
029500                    END-IF                                                
029600                 END-IF                                                   
029700                 PERFORM E-VISA-BILD                                      
029800              END-IF                                                      
029900           ELSE                                                           
030000              MOVE FEL-4                      TO MOD-TEMFSFEL             
030100           END-IF                                                         
030200        ELSE                                                              
030300           MOVE FEL-3                         TO MOD-TEMFSFEL             
030400        END-IF                                                            
030500                                                                          
030600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
030700        PERFORM IMS-INSERT-MSG                                            
030800     END-IF                                                               
030900                                                                          
031000     MOVE ZERO TO RETURN-CODE                                             
031100     GOBACK.                                                              
031200     EJECT                                                                
031300 A-INIT-SPARA-INPUT SECTION.                                              
031400     SKIP2                                                                
031500     IF MSG-DUBBLA-TRANSKODER                                             
031600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I15101               
031700         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS  WS-IDTRANS             
031800         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
031900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
032000         MOVE MSG-IDPFK            TO MFS-IDPFK                           
032100     ELSE                                                                 
032200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I15101                
032300         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS  WS-IDTRANS             
032400         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
032500         MOVE ' '                  TO MFS-KDTRTYP                         
032600                                      MFS-IDPFK                           
032700     END-IF                                                               
032800                                                                          
032900     IF MFS-UPDATE  AND EGEN-BILD                                         
033000        IF MID-IDPROJOBJ   = ALL '+'  AND                                 
033100           MID-IDPROJ      = ALL '+'  AND                                 
033200           MID-IDLKTO      = ALL '+'  AND                                 
033300           MID-RESLJUST-C1 = ALL '+'                                      
033400           MOVE SPACE              TO MFS-KDTRTYP                         
033500                                      MFS-IDPFK                           
033600                                      MID-IDPROJ-1                        
033700                                      MID-IDPROJ-11                       
033800                                      MID-IDPROJOBJ-1                     
033900                                      MID-IDPROJOBJ-11                    
034000                                      MID-IDPROJK-1                       
034100                                      MID-IDPROJK-11                      
034200        END-IF                                                            
034300     END-IF                                                               
034400                                                                          
034500     IF MID-KDPRODSL-IN = ALL '+'                                         
034600        INSPECT MID-KDPRODSL-UT REPLACING LEADING SPACE BY ZERO           
034700        MOVE MID-KDPRODSL-UT       TO WS-KDPRODSL                         
034800     ELSE                                                                 
034900        MOVE MID-KDPRODSL-IN       TO WS-KDPRODSL                         
035000        MOVE SPACE                 TO MFS-KDTRTYP                         
035100                                      MFS-IDPFK                           
035200                                      MID-IDPROJ-1                        
035300                                      MID-IDPROJ-11                       
035400                                      MID-IDPROJOBJ-1                     
035500                                      MID-IDPROJOBJ-11                    
035600                                      MID-IDPROJK-1                       
035700                                      MID-IDPROJK-11                      
035800     END-IF                                                               
035900                                                                          
036000     MOVE LOW-VALUE                TO MOD-W2O15101                        
036100     MOVE 'W2O15101'               TO MFS-IDMOD                           
036200     MOVE '2151'                   TO MOD-IDTRANS                         
036300                                                                          
036400     MOVE WS-KDPRODSL              TO MOD-KDPRODSL-UT                     
036500     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
036600                                                                          
036700     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
036800                                      MOD-TEMFSINF                        
036900                                      MOD-KDPRODSL-IN                     
037000                                                                          
037100     MOVE MID-IDPROJ-1             TO MOD-IDPROJ-1                        
037200     MOVE MID-IDPROJ-11            TO MOD-IDPROJ-11                       
037300     MOVE MID-IDPROJOBJ-1          TO MOD-IDPROJOBJ-1                     
037400     MOVE MID-IDPROJOBJ-11         TO MOD-IDPROJOBJ-11                    
037500     MOVE MID-IDPROJK-1            TO MOD-IDPROJK-1                       
037600     MOVE MID-IDPROJK-11           TO MOD-IDPROJK-11.                     
037700     EJECT                                                                
037800 B-KOLLA-INPUT SECTION.                                                   
037900     SKIP2                                                                
038000     MOVE JA TO SW-INPUT-RAETT                                            
038100                                                                          
038200     IF MID-IDPROJ       = ALL '+'                                        
038300        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-IN-ATTR                   
038400        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJ-IN                        
038500        MOVE NEJ                  TO SW-INPUT-RAETT                       
038600     ELSE                                                                 
038700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR                   
038800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJ-IN                        
038900     END-IF                                                               
039000                                                                          
039100     IF MID-IDPROJOBJ = ALL '+'                                           
039200        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJOBJ-IN                     
039300        MOVE MFS-ALFA-FAELT-RAETT                                         
039400                                  TO MOD-IDPROJOBJ-IN-ATTR                
039500     ELSE                                                                 
039600        MOVE WS-KDPRODSL          TO TEST-KDPRODSL                        
039700        IF KDPRODSL-SPARE-PARTS OR                                        
039800           KDPRODSL-CHEMICAL    OR                                        
039900           KDPRODSL-BYTES       OR                                        
040000           KDPRODSL-ACC         OR                                        
040100           KDPRODSL-SERVICES    OR                                        
040200           KDPRODSL-LOCAL-PARTS OR                                        
040300           KDPRODSL-LOCAL-CHEM  OR                                        
040400           KDPRODSL-LOCAL-BYTES OR                                        
040500           KDPRODSL-LOCAL-ACC                                             
040600           MOVE MFS-ALFA-FAELT-FEL                                        
040700                                  TO MOD-IDPROJOBJ-IN-ATTR                
040800           MOVE NEJ               TO SW-INPUT-RAETT                       
040900        ELSE                                                              
041000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJOBJ-IN-ATTR              
041100        END-IF                                                            
041200        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJOBJ-IN                     
041300     END-IF                                                               
041400                                                                          
041500     IF MID-IDLKTO       = ALL '+'    AND                                 
041600        MID-RESLJUST-C1  = ALL '+'                                        
041700        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDLKTO-IN-ATTR                   
041800                                     MOD-RESLJUST-C1-IN-ATTR              
041900        MOVE MFS-STAENG-FAELT     TO MOD-RESLJUST-C2-IN-ATTR              
042000        MOVE MFS-RENSA-FAELT      TO MOD-IDLKTO-IN                        
042100                                     MOD-RESLJUST-C1-IN                   
042200                                     MOD-RESLJUST-C2-IN                   
042300        MOVE NEJ                  TO SW-INPUT-RAETT                       
042400     ELSE                                                                 
042500        IF MID-IDLKTO = ALL '+'                                           
042600           MOVE MFS-RENSA-FAELT        TO MOD-IDLKTO-IN                   
042700        ELSE                                                              
042800           IF MID-IDLKTO NUMERIC                                          
042900              PERFORM BA-KOLLA-IDLKTO                                     
043000           ELSE                                                           
043100              MOVE MFS-NUM-FAELT-FEL   TO MOD-IDLKTO-IN-ATTR              
043200              MOVE NEJ                 TO SW-INPUT-RAETT                  
043300           END-IF                                                         
043400           MOVE MFS-ROER-EJ-FAELT      TO MOD-IDLKTO-IN                   
043500        END-IF                                                            
043600                                                                          
043700        IF MID-RESLJUST-C1 = ALL '+'                                      
043800           MOVE MFS-RENSA-FAELT        TO MOD-RESLJUST-C1-IN              
043900        ELSE                                                              
044000           MOVE MID-RESLJUST-C1        TO DEC-IDFRIDATA                   
044100           MOVE 1                      TO DEC-KVHELTAL                    
044200                                          DEC-KVDECIMAL                   
044300           CALL WDECEDIT USING DEC-WDECAREA                               
044400           IF DEC-KDSVAR-OK                                               
044500              IF DEC-IDEDITDATA > 0  AND  < 9.8                           
044600                 MOVE MFS-NUM-FAELT-RAETT                                 
044700                                       TO MOD-RESLJUST-C1-IN-ATTR         
044800                 MOVE DEC-IDEDITDATA   TO SPAR-RESLJUST-C1                
044900              ELSE                                                        
045000                 MOVE MFS-NUM-FAELT-FEL                                   
045100                                       TO MOD-RESLJUST-C1-IN-ATTR         
045200                 MOVE NEJ              TO SW-INPUT-RAETT                  
045300              END-IF                                                      
045400           ELSE                                                           
045500              MOVE MFS-NUM-FAELT-FEL   TO MOD-RESLJUST-C1-IN-ATTR         
045600              MOVE NEJ                 TO SW-INPUT-RAETT                  
045700           END-IF                                                         
045800           MOVE MFS-ROER-EJ-FAELT      TO MOD-RESLJUST-C1-IN              
045900        END-IF                                                            
046000                                                                          
046100           MOVE MFS-RENSA-FAELT        TO MOD-RESLJUST-C2-IN              
046200           MOVE MFS-STAENG-FAELT       TO MOD-RESLJUST-C2-IN-ATTR         
046300     END-IF                                                               
046400                                                                          
046500     IF SW-INPUT-RAETT = NEJ                                              
046600        PERFORM S04-ROER-EJ-FAELT                                         
046700     END-IF                                                               
046800     .                                                                    
046900     SKIP3                                                                
047000 BA-KOLLA-IDLKTO SECTION.                                                 
047100                                                                          
047200     MOVE WS-KDPRODSL TO KPS-KDPRODSL                                     
047300     MOVE 2           TO KPS-KDCALL                                       
047400     CALL WKPSKONV USING KPS-WKPSAREA                                     
047500     IF KPS-KDSVAR = 'F'                                                  
047600       MOVE NEJ               TO SW-INPUT-RAETT                           
047700       MOVE MFS-NUM-FAELT-FEL TO MOD-IDLKTO-IN-ATTR                       
047800     ELSE                                                                 
047900       MOVE MID-IDLKTO TO SPAR-IDLKTO                                     
048000       IF (SPAR-IDLKTO-POS3-7 > 10000 AND < 20000)                        
048100*******  KOLL ATT 1:A I 3:E POSITIONEN                                    
048200         IF SPAR-IDLKTO-POS1-2 = KPS-IDFTG                                
048300           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLKTO-IN-ATTR                 
048400         ELSE                                                             
048500           MOVE NEJ               TO SW-INPUT-RAETT                       
048600           MOVE MED-4             TO MOD-TEMFSINF                         
048700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDLKTO-IN-ATTR                   
048800         END-IF                                                           
048900       ELSE                                                               
049000         MOVE NEJ               TO SW-INPUT-RAETT                         
049100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDLKTO-IN-ATTR                     
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 C-KOLLA-MOT-BASEN SECTION.                                               
049700     SKIP2                                                                
049800     IF MID-IDPROJOBJ = ALL '+'                                           
049900        MOVE SPACE                     TO W-IDPROJOBJ                     
050000     ELSE                                                                 
050100        MOVE MID-IDPROJOBJ             TO W-IDPROJOBJ                     
050200     END-IF                                                               
050300                                                                          
050400     MOVE MID-IDPROJ                  TO W-IDPROJ                         
050500                                                                          
050600     PERFORM IMS-GET-WLXXAQ11-PROJ                                        
050700                                                                          
050800     IF SEGMENT-FINNS                                                     
050900        CONTINUE                                                          
051000     ELSE                                                                 
051100        MOVE MFS-ALFA-FAELT-FEL        TO MOD-IDPROJ-IN-ATTR              
051200        IF MID-IDPROJOBJ = ALL '+'                                        
051300           CONTINUE                                                       
051400        ELSE                                                              
051500           MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDPROJOBJ-IN-ATTR           
051600        END-IF                                                            
051700        MOVE NEJ                       TO SW-INPUT-RAETT                  
051800     END-IF                                                               
051900                                                                          
052000     IF SW-INPUT-RAETT = NEJ                                              
052100        PERFORM S04-ROER-EJ-FAELT                                         
052200     END-IF.                                                              
052300     EJECT                                                                
052400 D-UPPDATERA-OCH-VISA-BILD SECTION.                                       
052500     SKIP2                                                                
052600     MOVE WLXXAQ-1132-IDPROJ      TO MOD-IDPROJ-1                         
052700     MOVE WLXXAQ-1132-IDPROJOBJ   TO MOD-IDPROJOBJ-1                      
052800     MOVE WLXXAQ-1132-IDPROJK     TO MOD-IDPROJK-1                        
052900                                                                          
053000     SET MOD-RAD-IND              TO 1                                    
053100                                                                          
053200     IF MID-IDLKTO = ALL '+'                                              
053300        CONTINUE                                                          
053400     ELSE                                                                 
053500        MOVE MID-IDLKTO           TO WLXXAQ-1132-IDLKTO                   
053600                                     MOD-RAD-IDLKTO                       
053700                                        (MOD-RAD-IND)                     
053800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-IDLKTO-ATTR                 
053900                                        (MOD-RAD-IND)                     
054000     END-IF                                                               
054100                                                                          
054200     IF MID-RESLJUST-C1    = ALL '+'                                      
054300        CONTINUE                                                          
054400     ELSE                                                                 
054500        MOVE SPAR-RESLJUST-C1     TO WLXXAQ-1132-RESLJUST-C1              
054600                                     MOD-RAD-RESLJUST-C1                  
054700                                        (MOD-RAD-IND)                     
054800        MOVE MFS-ADD-LYS-UPP-FAELT TO                                     
054900                                   MOD-RAD-RESLJUST-C1-ATTR               
055000                                        (MOD-RAD-IND)                     
055100     END-IF                                                               
055200                                                                          
055300     PERFORM IMS-REPLACE                                                  
055400                                                                          
055500     PERFORM S02-LAGG-UT-RESTEN-I-MODDEN                                  
055600                                                                          
055700     MOVE MFS-RENSA-FAELT            TO MOD-IDPROJOBJ-IN                  
055800                                        MOD-IDPROJ-IN                     
055900                                        MOD-IDLKTO-IN                     
056000                                        MOD-RESLJUST-C1-IN                
056100                                        MOD-RESLJUST-C2-IN                
056200                                                                          
056300     MOVE MFS-FORMATETS-ATTR         TO MOD-IDPROJOBJ-IN-ATTR             
056400                                        MOD-IDPROJ-IN-ATTR                
056500                                        MOD-IDLKTO-IN-ATTR                
056600                                        MOD-RESLJUST-C1-IN-ATTR           
056700     MOVE MFS-STAENG-FAELT           TO MOD-RESLJUST-C2-IN-ATTR           
056800                                                                          
056900     MOVE MED-3                      TO MOD-TEMFSINF.                     
057000     EJECT                                                                
057100 E-VISA-BILD SECTION.                                                     
057200     SKIP2                                                                
057300     PERFORM IMS-GET-WLXXAQ11                                             
057400                                                                          
057500     IF SEGMENT-FINNS                                                     
057600        MOVE WLXXAQ-1132-IDPROJ     TO MOD-IDPROJ-1                       
057700        MOVE WLXXAQ-1132-IDPROJOBJ  TO MOD-IDPROJOBJ-1                    
057800        MOVE WLXXAQ-1132-IDPROJK    TO MOD-IDPROJK-1                      
057900        PERFORM S01-LAGG-UT-MODDEN                                        
058000     ELSE                                                                 
058100        MOVE SPACE                TO W-IDPROJ                             
058200        PERFORM IMS-GET-WLXXAQ11-FIRST                                    
058300        IF SEGMENT-FINNS                                                  
058400           MOVE WLXXAQ-1132-IDPROJ    TO MOD-IDPROJ-1                     
058500           MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-IDPROJOBJ-1                  
058600           MOVE WLXXAQ-1132-IDPROJK   TO MOD-IDPROJK-1                    
058700           PERFORM S01-LAGG-UT-MODDEN                                     
058800        ELSE                                                              
058900           MOVE FEL-5             TO MOD-TEMFSFEL                         
059000           MOVE SPACE             TO MOD-IDPROJ-1                         
059100                                     MOD-IDPROJ-11                        
059200                                     MOD-IDPROJOBJ-1                      
059300                                     MOD-IDPROJOBJ-11                     
059400                                     MOD-IDPROJK-1                        
059500                                     MOD-IDPROJK-11                       
059600        END-IF                                                            
059700     END-IF                                                               
059800                                                                          
059900     IF MID-KDPRODSL-IN        = ALL '+'  AND EGEN-BILD                   
060000        IF MID-IDPROJOBJ       = ALL '+'  AND                             
060100           MID-IDPROJ          = ALL '+'  AND                             
060200           MID-IDLKTO          = ALL '+'  AND                             
060300           MID-RESLJUST-C1     = ALL '+'                                  
060400           CONTINUE                                                       
060500        ELSE                                                              
060600           IF MID-IDPROJOBJ = ALL '+'                                     
060700              MOVE MFS-RENSA-FAELT    TO MOD-IDPROJOBJ-IN                 
060800           ELSE                                                           
060900              MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPROJOBJ-IN                 
061000              MOVE MFS-ALFA-FAELT-RAETT                                   
061100                                    TO MOD-IDPROJOBJ-IN-ATTR              
061200           END-IF                                                         
061300                                                                          
061400           IF MID-IDPROJ  = ALL '+'                                       
061500              MOVE MFS-RENSA-FAELT    TO MOD-IDPROJ-IN                    
061600           ELSE                                                           
061700              MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPROJ-IN                    
061800              MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDPROJ-IN-ATTR            
061900           END-IF                                                         
062000                                                                          
062100           IF MID-IDLKTO = ALL '+'                                        
062200              MOVE MFS-RENSA-FAELT    TO MOD-IDLKTO-IN                    
062300           ELSE                                                           
062400              MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLKTO-IN                    
062500              MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDLKTO-IN-ATTR            
062600           END-IF                                                         
062700                                                                          
062800           IF MID-RESLJUST-C1 = ALL '+'                                   
062900              MOVE MFS-RENSA-FAELT    TO MOD-RESLJUST-C1-IN               
063000           ELSE                                                           
063100              MOVE MFS-ROER-EJ-FAELT  TO MOD-RESLJUST-C1-IN               
063200              MOVE MFS-NUM-FAELT-RAETT TO                                 
063300                                      MOD-RESLJUST-C1-IN-ATTR             
063400           END-IF                                                         
063500                                                                          
063600              MOVE MFS-RENSA-FAELT    TO MOD-RESLJUST-C2-IN               
063700              MOVE MFS-STAENG-FAELT   TO                                  
063800                                      MOD-RESLJUST-C2-IN-ATTR             
063900                                                                          
064000           MOVE MED-2                 TO MOD-TEMFSINF                     
064100        END-IF                                                            
064200     ELSE                                                                 
064300        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
064400     END-IF.                                                              
064500     EJECT                                                                
064600 S01-LAGG-UT-MODDEN SECTION.                                              
064700                                                                          
064800     SET MOD-RAD-IND              TO 1                                    
064900                                                                          
065000     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
065100        IF SEGMENT-FINNS                                                  
065200           MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-RAD-IDPROJOBJ                
065300                                     (MOD-RAD-IND)                        
065400           MOVE WLXXAQ-1132-IDPROJ    TO MOD-RAD-IDPROJ                   
065500                                     (MOD-RAD-IND)                        
065600           MOVE WLXXAQ-1132-IDLKTO    TO MOD-RAD-IDLKTO                   
065700                                     (MOD-RAD-IND)                        
065800           MOVE WLXXAQ-1132-RESLJUST-C1                                   
065900                                      TO MOD-RAD-RESLJUST-C1              
066000                                     (MOD-RAD-IND)                        
066100           MOVE MFS-RENSA-FAELT                                           
066200                                      TO MOD-RAD-RESLJUST-C2              
066300                                     (MOD-RAD-IND)                        
066400           PERFORM IMS-GET-WLXXAQ11                                       
066500        ELSE                                                              
066600           MOVE MFS-RENSA-FAELT TO MOD-RAD-IDPROJOBJ                      
066700                                     (MOD-RAD-IND)                        
066800                                   MOD-RAD-IDPROJ                         
066900                                     (MOD-RAD-IND)                        
067000                                   MOD-RAD-IDLKTO                         
067100                                     (MOD-RAD-IND)                        
067200                                   MOD-RAD-RESLJUST-C1                    
067300                                     (MOD-RAD-IND)                        
067400                                   MOD-RAD-RESLJUST-C2                    
067500                                     (MOD-RAD-IND)                        
067600        END-IF                                                            
067700                                                                          
067800        SET MOD-RAD-IND UP BY 1                                           
067900     END-PERFORM                                                          
068000                                                                          
068100     IF SEGMENT-FINNS                                                     
068200        MOVE WLXXAQ-1132-IDPROJ     TO MOD-IDPROJ-11                      
068300        MOVE WLXXAQ-1132-IDPROJOBJ  TO MOD-IDPROJOBJ-11                   
068400        MOVE WLXXAQ-1132-IDPROJK    TO MOD-IDPROJK-11                     
068500        MOVE MED-1                  TO MOD-TEMFSINF                       
068600     ELSE                                                                 
068700        MOVE SPACE                  TO MOD-IDPROJ-11                      
068800                                       MOD-IDPROJOBJ-11                   
068900                                       MOD-IDPROJK-11                     
069000     END-IF.                                                              
069100     EJECT                                                                
069200                                                                          
069300 S02-LAGG-UT-RESTEN-I-MODDEN SECTION.                                     
069400                                                                          
069500     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
069600        IF SEGMENT-FINNS                                                  
069700           MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-RAD-IDPROJOBJ                
069800                                     (MOD-RAD-IND)                        
069900           MOVE WLXXAQ-1132-IDPROJ    TO MOD-RAD-IDPROJ                   
070000                                     (MOD-RAD-IND)                        
070100           MOVE WLXXAQ-1132-IDLKTO    TO MOD-RAD-IDLKTO                   
070200                                     (MOD-RAD-IND)                        
070300           MOVE WLXXAQ-1132-RESLJUST-C1                                   
070400                                      TO MOD-RAD-RESLJUST-C1              
070500                                     (MOD-RAD-IND)                        
070600           MOVE MFS-RENSA-FAELT                                           
070700                                      TO MOD-RAD-RESLJUST-C2              
070800                                     (MOD-RAD-IND)                        
070900           PERFORM IMS-GET-WLXXAQ11                                       
071000        ELSE                                                              
071100           MOVE MFS-RENSA-FAELT TO MOD-RAD-IDPROJOBJ                      
071200                                     (MOD-RAD-IND)                        
071300                                   MOD-RAD-IDPROJ                         
071400                                     (MOD-RAD-IND)                        
071500                                   MOD-RAD-IDLKTO                         
071600                                     (MOD-RAD-IND)                        
071700                                   MOD-RAD-RESLJUST-C1                    
071800                                     (MOD-RAD-IND)                        
071900                                   MOD-RAD-RESLJUST-C2                    
072000                                     (MOD-RAD-IND)                        
072100        END-IF                                                            
072200                                                                          
072300        SET MOD-RAD-IND UP BY 1                                           
072400     END-PERFORM                                                          
072500                                                                          
072600     IF SEGMENT-FINNS                                                     
072700        MOVE WLXXAQ-1132-IDPROJ     TO MOD-IDPROJ-11                      
072800        MOVE WLXXAQ-1132-IDPROJOBJ  TO MOD-IDPROJOBJ-11                   
072900        MOVE WLXXAQ-1132-IDPROJK    TO MOD-IDPROJK-11                     
073000     ELSE                                                                 
073100        MOVE SPACE                  TO MOD-IDPROJ-11                      
073200                                       MOD-IDPROJOBJ-11                   
073300                                       MOD-IDPROJK-11                     
073400     END-IF.                                                              
073500     EJECT                                                                
073600                                                                          
073700 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
073800     SKIP3                                                                
073900     MOVE MFS-RENSA-FAELT       TO MOD-IDPROJOBJ-IN                       
074000                                   MOD-IDPROJ-IN                          
074100                                   MOD-IDLKTO-IN                          
074200                                   MOD-RESLJUST-C1-IN                     
074300                                   MOD-RESLJUST-C2-IN.                    
074400     EJECT                                                                
074500 S04-ROER-EJ-FAELT SECTION.                                               
074600     SKIP3                                                                
074700     SET MOD-RAD-IND                 TO 1                                 
074800                                                                          
074900     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
075000        MOVE MFS-ROER-EJ-FAELT  TO MOD-RAD-IDPROJOBJ                      
075100                                   (MOD-RAD-IND)                          
075200                                   MOD-RAD-IDPROJ                         
075300                                   (MOD-RAD-IND)                          
075400                                   MOD-RAD-IDLKTO                         
075500                                   (MOD-RAD-IND)                          
075600                                   MOD-RAD-RESLJUST-C1                    
075700                                   (MOD-RAD-IND)                          
075800        MOVE MFS-RENSA-FAELT    TO MOD-RAD-RESLJUST-C2                    
075900                                   (MOD-RAD-IND)                          
076000        SET MOD-RAD-IND UP BY 1                                           
076100     END-PERFORM.                                                         
076200                                                                          
076300     EJECT                                                                
076400* IMS SEKTIONER                                                           
076500     SKIP3                                                                
076600 IMS-GET-MSG SECTION.                                                     
076700     SKIP2                                                                
076800     MOVE '  QC' TO GODK-STATUSKODER                                      
076900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
077000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077100     PERFORM IMS-STATUS-KONTROLL.                                         
077200     SKIP3                                                                
077300 IMS-INSERT-MSG SECTION.                                                  
077400     SKIP2                                                                
077500     IF ENGLISH-TEXT                                                      
077600        MOVE 'N' TO MFS-KDHUVOMR                                          
077700     END-IF                                                               
077800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077900     MOVE SPACE TO GODK-STATUSKODER                                       
078000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
078100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
078200     PERFORM IMS-STATUS-KONTROLL.                                         
078300     EJECT                                                                
078400 IMS-GET-WLXXAQ01 SECTION.                                                
078500     SKIP2                                                                
078600     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
078700             DELIMITED BY SIZE INTO SSA1                                  
078800     MOVE '  GE' TO GODK-STATUSKODER                                      
078900     CALL CBLTDLI USING GU WLXXAQ-PCB DLI-IO-AREA SSA1                    
079000     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
079100     PERFORM IMS-STATUS-KONTROLL.                                         
079200     SKIP3                                                                
079300 IMS-GET-WLXXAQ11 SECTION.                                                
079400     SKIP2                                                                
079500     STRING 'WLXXAQ11(WDGXKEY =>' W-1132KEY-X ')'                         
079600             DELIMITED BY SIZE INTO SSA1                                  
079700     MOVE '  GE' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING GNP WLXXAQ-PCB DLI-IO-AREA SSA1                   
079900     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
080000     PERFORM IMS-STATUS-KONTROLL.                                         
080100     SKIP3                                                                
080200     EJECT                                                                
080300 IMS-GET-WLXXAQ11-PROJ SECTION.                                           
080400     SKIP2                                                                
080500     STRING 'WLXXAQ11*F(IDPROJOB =' W-IDPROJOBJ                           
080600                      '&IDPROJ   =' W-IDPROJ  ')'                         
080700            DELIMITED BY SIZE INTO SSA1                                   
080800     MOVE '  GE' TO GODK-STATUSKODER                                      
080900     CALL CBLTDLI USING GHNP WLXXAQ-PCB DLI-IO-AREA SSA1                  
081000     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
081100     PERFORM IMS-STATUS-KONTROLL.                                         
081200     SKIP3                                                                
081300 IMS-GET-WLXXAQ11-FIRST SECTION.                                          
081400     SKIP2                                                                
081500     STRING 'WLXXAQ11*F(WDGXKEY =>' W-1132KEY-X ')'                       
081600             DELIMITED BY SIZE INTO SSA1                                  
081700     MOVE '  GE' TO GODK-STATUSKODER                                      
081800     CALL CBLTDLI USING GNP WLXXAQ-PCB DLI-IO-AREA SSA1                   
081900     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
082000     PERFORM IMS-STATUS-KONTROLL.                                         
082100     SKIP3                                                                
082200     EJECT                                                                
082300 IMS-REPLACE SECTION.                                                     
082400     SKIP2                                                                
082500     MOVE '  '   TO GODK-STATUSKODER                                      
082600     CALL CBLTDLI USING REPL WLXXAQ-PCB DLI-IO-AREA                       
082700     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
082800     PERFORM IMS-STATUS-KONTROLL.                                         
082900     SKIP3                                                                
083000 IMS-STATUS-KONTROLL SECTION.                                             
083100     SET STATUS-IX TO 1                                                   
083200     SEARCH GODK-STATUS                                                   
083300       AT END                                                             
083400         CALL FELLOG                                                      
083500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
083600     END-SEARCH.                                                          
