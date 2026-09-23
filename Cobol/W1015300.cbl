000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1015300.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   MAJ  1988.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800****************************************************************          
000900*                   NYPON-BILD 1153                            *          
001000*                                                              *          
001100*                   PROJEKT MENY                               *          
001200*                                                              *          
001300* 911018 ÄT MÖJLIGHET ATT REG ETT IDPROJK MOT FLERA IDPROJ GE  *          
001400*                                                              *          
001500* 940524 ÄT KDAGE TILLAGT FÖR REGISTRERING.                    *          
001600*           LARS THELL CAP PROGRAMATOR.                        *          
001700*                                                              *          
001800* 150205 CCID 10249871 - RESTRICTIONS ON PRODUCTGROUP          *          
001900*                                                              *          
002000*  BILD FÖR BEREDNINGSAVDELNINGEN SOM KAN FRÅGA PÅ OCH         *          
002100*  UPPDATERA ETT PRODUKTSLAGS PROJEKTINFORMATION. DENNA INFO   *          
002200*  BESTÅR AV IDPROJ, IDPROJOBJ, IDPROJK, KVNYART, KVNYRES,     *          
002300*  TIPRODSTA, TIFINLEV, KDAGE.                                 *          
002400*                                                              *          
002500*  FUNKTIONER: ENTER                                           *          
002600*              PF7                                             *          
002700*              PF8                                             *          
002800*              PF11                                            *          
002900*                                                              *          
003000****************************************************************          
003100     EJECT                                                                
003200*    INDATA.                                                              
003300*        TRANSAKTION: W1T153                                              
003400*                     W1T153U                                             
003500*        MID:         W1I15301                                            
003600*    UTDATA.                                                              
003700*        MOD:         W1O15301                                            
003800                                                                          
003900 ENVIRONMENT DIVISION.                                                    
004000                                                                          
004100 DATA DIVISION.                                                           
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                   PIC X(08)  VALUE 'W1015300'.                 
004700 77  JA                      PIC X(01)  VALUE 'J'.                        
004800 77  NEJ                     PIC X(01)  VALUE 'N'.                        
004900 77  MAX-MOD-LAENGD          PIC S9(4)  VALUE +785 COMP SYNC.             
005000 77  MAX-RAD-IND             PIC  9(2)  VALUE  10.                        
005100 77  LEV05-IX                PIC S9(9)  VALUE +0   COMP SYNC.             
005200                                                                          
005300 77  WS-KDPRODSL             PIC X(02)  VALUE SPACE.                      
005400                                                                          
005500 77  WS-TIPROINF-FOM         PIC S9(7) COMP-3 VALUE ZERO.                 
005600 77  WS-TIPROINF-TOM         PIC S9(7) COMP-3 VALUE ZERO.                 
005700                                                                          
005800 77  WS-TIAAVV-FOM           PIC  9(4)        VALUE ZERO.                 
005900 77  WS-TIAAVV-TOM           PIC  9(4)        VALUE ZERO.                 
006000                                                                          
006100 77  WS-IDTRANS              PIC X(04).                                   
006200     88  EGEN-BILD                          VALUE '1153'.                 
006300                                                                          
006400*01  -COPY WWPRODSL                                                       
006500     EJECT                                                                
006600*                                                                         
006700******************************************************************        
006800*                     S W I T C H A R                            *        
006900******************************************************************        
007000*                                                                         
007100 01  SWITCHAR.                                                            
007200     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
007300     05  SW-SUPPLIER-OK          PIC X(01)  VALUE 'J'.                    
007400                                                                          
007500*                                                                         
007600******************************************************************        
007700*               D I V E R S E  S P A R F Ä L T                   *        
007800******************************************************************        
007900*                                                                         
008000 01  SPAR-DIVERSE.                                                        
008100     05  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.               
008200     05  SPAR-TEXT-IND               PIC 9(01)  VALUE ZERO.               
008300     EJECT                                                                
008400*                                                                         
008500******************************************************************        
008600*           D Y N A M I S K A  S U B P R O G R A M               *        
008700******************************************************************        
008800*                                                                         
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
009100     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
009200     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
009300     05  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
009400     EJECT                                                                
009500******************************************************************        
009600*    F E L M E D D E L A N D E N                                          
009700******************************************************************        
009800*                                                                         
009900 01  MEDDELANDE.                                                          
010000     03  W-MED-1.                                                         
010100         05  FILLER              PIC X(34) VALUE                          
010200            'MER INFO PÅ NÄSTA SIDA            '.                         
010300         05  FILLER              PIC X(34) VALUE                          
010400            'MORE INFO NEXT PAGE               '.                         
010500     03  FILLER  REDEFINES  W-MED-1.                                      
010600         05  MED-1               PIC X(34) OCCURS 2.                      
010700                                                                          
010800     03  W-MED-2.                                                         
010900         05  FILLER              PIC X(34) VALUE                          
011000            'TRYCK PF11 FÖR UPPDATERING        '.                         
011100         05  FILLER              PIC X(34) VALUE                          
011200            'UPDATE WITH PF11                  '.                         
011300     03  FILLER  REDEFINES  W-MED-2.                                      
011400         05  MED-2               PIC X(34) OCCURS 2.                      
011500                                                                          
011600     03  W-MED-3.                                                         
011700         05  FILLER              PIC X(34) VALUE                          
011800            'UPPDATERING UTFÖRD                '.                         
011900         05  FILLER              PIC X(34) VALUE                          
012000            'UPDATED                           '.                         
012100     03  FILLER  REDEFINES  W-MED-3.                                      
012200         05  MED-3               PIC X(34) OCCURS 2.                      
012300                                                                          
012400     03  W-FEL-2.                                                         
012500         05  FILLER              PIC X(34) VALUE                          
012600            'UPPLYSTA FÄLT FEL                 '.                         
012700         05  FILLER              PIC X(34) VALUE                          
012800            'HIGH LIGHTED FIELD INCORRECT      '.                         
012900     03  FILLER  REDEFINES  W-FEL-2.                                      
013000         05  FEL-2               PIC X(34) OCCURS 2.                      
013100                                                                          
013200     03  W-FEL-3.                                                         
013300         05  FILLER              PIC X(34) VALUE                          
013400            'PRODUKTSLAG EJ NUMERISKT          '.                         
013500         05  FILLER              PIC X(34) VALUE                          
013600            'PROD.GROUP NOT NUMERIC            '.                         
013700     03  FILLER  REDEFINES  W-FEL-3.                                      
013800         05  FEL-3               PIC X(34) OCCURS 2.                      
013900                                                                          
014000     03  W-FEL-4.                                                         
014100         05  FILLER              PIC X(34) VALUE                          
014200            'PRODUKTSLAG SAKNAS                '.                         
014300         05  FILLER              PIC X(34) VALUE                          
014400            'PROD.GROUP IS MISSING             '.                         
014500     03  FILLER  REDEFINES  W-FEL-4.                                      
014600         05  FEL-4               PIC X(34) OCCURS 2.                      
014700                                                                          
014800     03  W-FEL-5.                                                         
014900         05  FILLER              PIC X(34) VALUE                          
015000            'PRODUKTSLAGET SAKNAR PROJEKT      '.                         
015100         05  FILLER              PIC X(34) VALUE                          
015200            'PROJECT IS MISSING FOR PROD.GROUP '.                         
015300     03  FILLER  REDEFINES  W-FEL-5.                                      
015400         05  FEL-5               PIC X(34) OCCURS 2.                      
015500                                                                          
015600     03  W-FEL-6.                                                         
015700         05  FILLER              PIC X(34) VALUE                          
015800            'PROJEKT FINNS REDAN               '.                         
015900         05  FILLER              PIC X(34) VALUE                          
016000            'PROJECT ALREADY EXISTS            '.                         
016100     03  FILLER  REDEFINES  W-FEL-6.                                      
016200         05  FEL-6               PIC X(34) OCCURS 2.                      
016300                                                                          
016400     03  W-FEL-7.                                                         
016500         05  FILLER              PIC X(34) VALUE                          
016600            'OBJEKT OCH PROJEKT FINNS REDAN    '.                         
016700         05  FILLER              PIC X(34) VALUE                          
016800            'OBJEKT AND PROJECT ALREADY EXIST  '.                         
016900     03  FILLER  REDEFINES  W-FEL-7.                                      
017000         05  FEL-7               PIC X(34) OCCURS 2.                      
017100                                                                          
017200     03  W-MED-4.                                                         
017300         05  FILLER              PIC X(34) VALUE                          
017400            'UPPDATERING EJ TILLÅTEN           '.                         
017500         05  FILLER              PIC X(34) VALUE                          
017600            'UPDATE NOT ALLOWED                '.                         
017700     03  FILLER  REDEFINES  W-MED-4.                                      
017800         05  MED-4               PIC X(34) OCCURS 2.                      
017900                                                                          
018000     EJECT                                                                
018100*                                                                         
018200******************************************************************        
018300*              N Y C K L A R  T I L L  D L I                     *        
018400******************************************************************        
018500*                                                                         
018600 01  NYCKLAR-TILL-DLI.                                                    
018700     03  W-1131KEY-X.                                                     
018800          05  FILLER           PIC X(04)  VALUE '1131'.                   
018900          05  W-KDPRODSL       PIC S9(3)  VALUE ZERO COMP-3.              
019000          05  FILLER           PIC X(24)  VALUE LOW-VALUE.                
019100                                                                          
019200     03  W-1132KEY-X.                                                     
019300          05  W-IDPROJK        PIC X(04)  VALUE SPACE.                    
019400          05  W-IDPROJOBJ      PIC X(04)  VALUE SPACE.                    
019500          05  W-IDPROJ         PIC X(04)  VALUE SPACE.                    
019600          05  FILLER           PIC X(03)  VALUE LOW-VALUE.                
019700                                                                          
019800     EJECT                                                                
019900*                                                                         
020000******************************************************************        
020100*                    C O P Y T E X T E R    (DIVERSE)            *        
020200******************************************************************        
020300*                                                                         
020400 01  IMS-WS-1.                                                            
020500     03  FILLER                  PIC X(16)   VALUE 'RDAT-AREA'.           
020600     SKIP3                                                                
020700*01  -COPY WDATAREA                                                       
020800     SKIP3                                                                
020900     03  FILLER                  PIC X(16)   VALUE 'WWLEV05  '.           
021000*01  -COPY WWLEV05                                                        
021100     EJECT                                                                
021200     03  FILLER                  PIC X(16)   VALUE 'WMSGINIT '.           
021300*01  -COPY WMSGINIT                                                       
021400     EJECT                                                                
021500*                                                                         
021600******************************************************************        
021700*                    M I D-C O P Y T E X T                       *        
021800******************************************************************        
021900*                                                                         
022000*                        ****    MFS OCH SKÄRMHANTERING                   
022100 01  IMS-WS-2.                                                            
022200     03  FILLER                  PIC X(16)   VALUE 'MID-COPY'.            
022300     SKIP3                                                                
022400*01  MID -COPY W1I15301                                                   
022500     EJECT                                                                
022600*                                                                         
022700******************************************************************        
022800*                    M S G - A R E A                             *        
022900******************************************************************        
023000*                                                                         
023100 01  IMS-WS-3.                                                            
023200     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
023300     SKIP3                                                                
023400*01  -COPY WMSGAREA                                                       
023500     EJECT                                                                
023600*                                                                         
023700******************************************************************        
023800*                    M O D-C O P Y T E X T                       *        
023900******************************************************************        
024000*                                                                         
024100*01  IMS-WS-4.                                                            
024200*    03  FILLER                  PIC X(16)   VALUE 'MOD-COPY'.            
024300     SKIP3                                                                
024400*    03  MOD -COPY W1O15301  -RED MSG-AREA.                               
024500     EJECT                                                                
024600*                                                                         
024700******************************************************************        
024800*                    M F S - A R E A                             *        
024900******************************************************************        
025000*                                                                         
025100 01  IMS-WS-5.                                                            
025200     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
025300     SKIP3                                                                
025400*01  -COPY WMFSAREA.                                                      
025500     EJECT                                                                
025600*                                                                         
025700******************************************************************        
025800*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
025900******************************************************************        
026000*                                                                         
026100 01  IMS-WS-6.                                                            
026200     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
026300     SKIP3                                                                
026400*****                    **** STATUS-KOD FRÅN IMS                         
026500     03  STATUS-WS               PIC X(2).                                
026600         88  SEGMENT-FINNS                   VALUE '  '.                  
026700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
026800     SKIP3                                                                
026900     03  GODK-STATUSKODER.                                                
027000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
027100     SKIP3                                                                
027200 01  IMS-WS-7.                                                            
027300     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
027400     SKIP3                                                                
027500 01  SSA1                        PIC X(64).                               
027600 01  SSA2                        PIC X(64).                               
027700     EJECT                                                                
027800*                                                                         
027900******************************************************************        
028000*            I M S  F U N K T I O N S K O D E R                  *        
028100******************************************************************        
028200*                                                                         
028300*                                                                         
028400 01  IMS-WS-8.                                                            
028500     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
028600     SKIP3                                                                
028700*01  -COPY W0003                                                          
028800     EJECT                                                                
028900*                                                                         
029000******************************************************************        
029100*            D L I  I N P U T-O U T P U T A R E A                *        
029200******************************************************************        
029300*                                                                         
029400 01  IMS-WS-10.                                                           
029500     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA '.           
029600     SKIP3                                                                
029700 01  DLI-IO-AREA.                                                         
029800     03  IO-AREA                   PIC X(200) VALUE SPACE.                
029900     EJECT                                                                
030000*                                                                         
030100******************************************************************        
030200*            S E G M E N T C O P Y T E X T E R                   *        
030300******************************************************************        
030400*                                                                         
030500*    03  WLXXAQ -COPY WDGX1131  -PRE WLXXAQ- -RED IO-AREA.                
030600     EJECT                                                                
030700*    03  WLXXAQ -COPY WDGX1132  -PRE WLXXAQ- -RED IO-AREA.                
030800     EJECT                                                                
030900*                                                                         
031000******************************************************************        
031100*            L I N K A G E  S E C T I O N                        *        
031200******************************************************************        
031300*                                                                         
031400 LINKAGE SECTION.                                                         
031500     SKIP2                                                                
031600*01  -COPY W0009     -PRE MSG-                                            
031700     EJECT                                                                
031800*01  -COPY W0008     -PRE USEA-                                           
031900         05  FILLER              PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008     -PRE WLXXAQ-                                         
032200         05  FILLER              PIC X.                                   
032300     EJECT                                                                
032400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WLXXAQ-PCB.                    
032500 MAIN SECTION.                                                            
032600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WLXXAQ-PCB.                   
032700                                                                          
032800     PERFORM IMS-GET-MSG                                                  
032900     IF SEGMENT-FINNS                                                     
033000        PERFORM A-INIT-SPARA-INPUT                                        
033100        IF WS-KDPRODSL NUMERIC                                            
033200           MOVE WS-KDPRODSL   TO W-KDPRODSL                               
033300           PERFORM IMS-GU-WLXXAQ01                                        
033400           IF SEGMENT-FINNS                                               
033500              IF MFS-UPDATE AND EGEN-BILD                                 
033600                 PERFORM B-KOLLA-INPUT                                    
033700                 IF SW-INPUT-RAETT = JA                                   
033800                    PERFORM C-KOLLA-MOT-BASEN                             
033900                    IF SW-INPUT-RAETT = JA                                
034000                       PERFORM D-UPPDATERA-OCH-VISA-BILD                  
034100                    ELSE                                                  
034200                       MOVE FEL-2 (SPAR-TEXT-IND) TO MOD-TEMFSFEL         
034300                    END-IF                                                
034400                 ELSE                                                     
034500                    IF SW-SUPPLIER-OK = NEJ                               
034600                      MOVE MED-4 (SPAR-TEXT-IND) TO MOD-TEMFSINF          
034700                    ELSE                                                  
034800                      MOVE FEL-2 (SPAR-TEXT-IND) TO MOD-TEMFSFEL          
034900                    END-IF                                                
035000                 END-IF                                                   
035100              ELSE                                                        
035200                 IF (NOT EGEN-BILD)  OR (MFS-IDPFK = '7')                 
035300                    MOVE SPACE                 TO W-IDPROJ                
035400                                                  W-IDPROJOBJ             
035500                                                  W-IDPROJK               
035600                 ELSE                                                     
035700                    IF MFS-IDPFK = '8'                                    
035800                       MOVE MID-IDPROJ-11      TO W-IDPROJ                
035900                       MOVE MID-IDPROJOBJ-11   TO W-IDPROJOBJ             
036000                       MOVE MID-IDPROJK-11     TO W-IDPROJK               
036100                    ELSE                                                  
036200                       MOVE MID-IDPROJ-1       TO W-IDPROJ                
036300                       MOVE MID-IDPROJOBJ-1    TO W-IDPROJOBJ             
036400                       MOVE MID-IDPROJK-1      TO W-IDPROJK               
036500                    END-IF                                                
036600                 END-IF                                                   
036700                 PERFORM S05-CHECK-SUPPLIER                               
036800                 IF SW-INPUT-RAETT = JA                                   
036900                    PERFORM E-VISA-BILD                                   
037000                 ELSE                                                     
037100                    MOVE MED-4 (SPAR-TEXT-IND) TO MOD-TEMFSINF            
037200                 END-IF                                                   
037300              END-IF                                                      
037400           ELSE                                                           
037500              MOVE FEL-4 (SPAR-TEXT-IND)      TO MOD-TEMFSFEL             
037600           END-IF                                                         
037700        ELSE                                                              
037800           MOVE FEL-3 (SPAR-TEXT-IND)         TO MOD-TEMFSFEL             
037900        END-IF                                                            
038000                                                                          
038100        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
038200        PERFORM IMS-INSERT-MSG                                            
038300     END-IF                                                               
038400                                                                          
038500     MOVE ZERO TO RETURN-CODE                                             
038600     GOBACK                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 A-INIT-SPARA-INPUT SECTION.                                              
039000     SKIP2                                                                
039100     IF MSG-DUBBLA-TRANSKODER                                             
039200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I15301               
039300         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS  WS-IDTRANS             
039400         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
039500         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
039600         MOVE MSG-IDPFK            TO MFS-IDPFK                           
039700     ELSE                                                                 
039800         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I15301                
039900         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS  WS-IDTRANS             
040000         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
040100         MOVE ' '                  TO MFS-KDTRTYP                         
040200                                      MFS-IDPFK                           
040300     END-IF                                                               
040400                                                                          
040500     IF MFS-UPDATE  AND EGEN-BILD                                         
040600        IF MID-INFALT = ALL '+'                                           
040700           MOVE SPACE              TO MFS-KDTRTYP                         
040800                                      MFS-IDPFK                           
040900                                      MID-IDPROJ-1                        
041000                                      MID-IDPROJ-11                       
041100                                      MID-IDPROJOBJ-1                     
041200                                      MID-IDPROJOBJ-11                    
041300                                      MID-IDPROJK-1                       
041400                                      MID-IDPROJK-11                      
041500        END-IF                                                            
041600     END-IF                                                               
041700                                                                          
041800     IF MID-KDPRODSL-IN = ALL '+'                                         
041900        INSPECT MID-KDPRODSL-UT REPLACING LEADING SPACE BY ZERO           
042000        MOVE MID-KDPRODSL-UT       TO WS-KDPRODSL                         
042100     ELSE                                                                 
042200        MOVE MID-KDPRODSL-IN       TO WS-KDPRODSL                         
042300        MOVE SPACE                 TO MFS-KDTRTYP                         
042400                                      MFS-IDPFK                           
042500                                      MID-IDPROJ-1                        
042600                                      MID-IDPROJ-11                       
042700                                      MID-IDPROJOBJ-1                     
042800                                      MID-IDPROJOBJ-11                    
042900                                      MID-IDPROJK-1                       
043000                                      MID-IDPROJK-11                      
043100     END-IF                                                               
043200                                                                          
043300     MOVE LOW-VALUE                TO MOD-W1O15301                        
043400     MOVE 'W1O153N1'               TO MFS-IDMOD                           
043500     MOVE '1153'                   TO MOD-IDTRANS                         
043600                                                                          
043700     MOVE WS-KDPRODSL              TO MOD-KDPRODSL-UT                     
043800     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
043900                                                                          
044000     IF MID-IDPROJ-IN = ALL '+'                                           
044100        MOVE MID-IDPROJ-UT         TO MOD-IDPROJ-UT1                      
044200     ELSE                                                                 
044300        MOVE MID-IDPROJ-IN         TO MOD-IDPROJ-UT1                      
044400     END-IF                                                               
044500                                                                          
044600     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
044700                                      MOD-TEMFSINF                        
044800                                      MOD-KDPRODSL-IN                     
044900                                      MOD-IDPROJ-IN1                      
045000                                                                          
045100     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
045200                                                                          
045300     MOVE MID-IDPROJ-1             TO MOD-IDPROJ-1                        
045400     MOVE MID-IDPROJ-11            TO MOD-IDPROJ-11                       
045500     MOVE MID-IDPROJOBJ-1          TO MOD-IDPROJOBJ-1                     
045600     MOVE MID-IDPROJOBJ-11         TO MOD-IDPROJOBJ-11                    
045700     MOVE MID-IDPROJK-1            TO MOD-IDPROJK-1                       
045800     MOVE MID-IDPROJK-11           TO MOD-IDPROJK-11                      
045900                                                                          
046000     IF SWEDISH-TEXT                                                      
046100        MOVE 1                     TO SPAR-TEXT-IND                       
046200     ELSE                                                                 
046300        MOVE 2                     TO SPAR-TEXT-IND                       
046400     END-IF.                                                              
046500                                                                          
046600     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
046700     MOVE '001'                    TO MSGI-KDCALL                         
046800     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
046900     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
047000     MOVE '1153'                   TO MSGI-IDTRANS                        
047100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047200     .                                                                    
047300     EJECT                                                                
047400 B-KOLLA-INPUT SECTION.                                                   
047500     SKIP2                                                                
047600     MOVE JA TO SW-INPUT-RAETT                                            
047700                                                                          
047800     IF MID-KDCMD = ALL '+'                                               
047900        MOVE MFS-RENSA-FAELT      TO MOD-KDCMD-IN                         
048000        MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR                    
048100        MOVE NEJ                  TO SW-INPUT-RAETT                       
048200     ELSE                                                                 
048300        IF MID-KDCMD-DELETE   OR                                          
048400           MID-KDCMD-REPLACE  OR                                          
048500           MID-KDCMD-INSERT                                               
048600           MOVE MFS-ALFA-FAELT-RAETT                                      
048700                                  TO MOD-KDCMD-IN-ATTR                    
048800        ELSE                                                              
048900           MOVE MFS-ALFA-FAELT-FEL                                        
049000                                  TO MOD-KDCMD-IN-ATTR                    
049100           MOVE NEJ               TO SW-INPUT-RAETT                       
049200        END-IF                                                            
049300        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD-IN                         
049400     END-IF                                                               
049500                                                                          
049600     IF MID-IDPROJ = ALL '+'                                              
049700        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJ-IN2                       
049800        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJ-IN2-ATTR                  
049900        MOVE NEJ                  TO SW-INPUT-RAETT                       
050000     ELSE                                                                 
050100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN2-ATTR                  
050200        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJ-IN2                       
050300     END-IF                                                               
050400                                                                          
050500     IF MID-IDPROJOBJ    = ALL '+'                                        
050600        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJOBJ-IN                     
050700        MOVE MFS-ALFA-FAELT-RAETT                                         
050800                                  TO MOD-IDPROJOBJ-IN-ATTR                
050900     ELSE                                                                 
051000        MOVE WS-KDPRODSL          TO TEST-KDPRODSL                        
051100        IF GOOD-KDPRODSL                                                  
051200*          P V - P R O D U K T S L A G                                    
051300*          CARPAC-PRODUKTSLAG                                             
051400*          LOKALA-PRODUKTSLAG                                             
051500           MOVE MFS-ALFA-FAELT-FEL                                        
051600                                  TO MOD-IDPROJOBJ-IN-ATTR                
051700           MOVE NEJ               TO SW-INPUT-RAETT                       
051800        ELSE                                                              
051900           MOVE MFS-ALFA-FAELT-RAETT                                      
052000                                  TO MOD-IDPROJOBJ-IN-ATTR                
052100        END-IF                                                            
052200        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJOBJ-IN                     
052300     END-IF                                                               
052400                                                                          
052500     IF MID-IDPROJK      = ALL '+'                                        
052600        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROJK-IN-ATTR                  
052700        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJK-IN                       
052800        MOVE NEJ                  TO SW-INPUT-RAETT                       
052900     ELSE                                                                 
053000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
053100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJK-IN                       
053200     END-IF                                                               
053300                                                                          
053400     IF MID-KDCMD-DELETE                                                  
053500        CONTINUE                                                          
053600     ELSE                                                                 
053700        IF MID-KDCMD-REPLACE             AND                              
053800           MID-KVNYART        = ALL '+'  AND                              
053900           MID-KVNYRES        = ALL '+'  AND                              
054000           MID-TIPRODSTA      = ALL '+'  AND                              
054100           MID-TIFINLEV       = ALL '+'  AND                              
054200           MID-TIAAVV-FOM     = ALL '+'  AND                              
054300           MID-TIAAVV-TOM     = ALL '+'  AND                              
054400           MID-KDAGE          = ALL '+'                                   
054500           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVNYART-IN-ATTR               
054600                                        MOD-KVNYRES-IN-ATTR               
054700                                        MOD-TIPRODSTA-IN-ATTR             
054800                                        MOD-TIFINLEV-IN-ATTR              
054900                                        MOD-TIAAVV-IN-FOM-ATTR            
055000                                        MOD-TIAAVV-IN-TOM-ATTR            
055100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDAGE-IN-ATTR                 
055200           MOVE MFS-RENSA-FAELT      TO MOD-KVNYART-IN                    
055300                                        MOD-KVNYRES-IN                    
055400                                        MOD-TIPRODSTA-IN                  
055500                                        MOD-TIFINLEV-IN                   
055600                                        MOD-TIAAVV-IN-FOM                 
055700                                        MOD-TIAAVV-IN-TOM                 
055800                                        MOD-KDAGE-IN                      
055900           MOVE NEJ                  TO SW-INPUT-RAETT                    
056000        ELSE                                                              
056100           IF MID-KVNYART = ALL '+'                                       
056200              MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVNYART-IN-ATTR            
056300              MOVE MFS-RENSA-FAELT      TO MOD-KVNYART-IN                 
056400           ELSE                                                           
056500              IF MID-KVNYART NUMERIC                                      
056600                 MOVE MFS-NUM-FAELT-RAETT                                 
056700                                        TO MOD-KVNYART-IN-ATTR            
056800              ELSE                                                        
056900                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVNYART-IN-ATTR            
057000                 MOVE NEJ               TO SW-INPUT-RAETT                 
057100              END-IF                                                      
057200              MOVE MFS-ROER-EJ-FAELT    TO MOD-KVNYART-IN                 
057300           END-IF                                                         
057400                                                                          
057500           IF MID-KVNYRES = ALL '+'                                       
057600              MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVNYRES-IN-ATTR            
057700              MOVE MFS-RENSA-FAELT      TO MOD-KVNYRES-IN                 
057800           ELSE                                                           
057900              IF MID-KVNYRES NUMERIC                                      
058000                 MOVE MFS-NUM-FAELT-RAETT                                 
058100                                        TO MOD-KVNYRES-IN-ATTR            
058200              ELSE                                                        
058300                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVNYRES-IN-ATTR            
058400                 MOVE NEJ               TO SW-INPUT-RAETT                 
058500              END-IF                                                      
058600              MOVE MFS-ROER-EJ-FAELT    TO MOD-KVNYRES-IN                 
058700           END-IF                                                         
058800                                                                          
058900           IF MID-TIPRODSTA = ALL '+'                                     
059000              MOVE MFS-RENSA-FAELT      TO MOD-TIPRODSTA-IN               
059100              IF MID-KDCMD-INSERT                                         
059200                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRODSTA-IN-ATTR          
059300                 MOVE NEJ               TO SW-INPUT-RAETT                 
059400              ELSE                                                        
059500                 MOVE MFS-NUM-FAELT-RAETT TO                              
059600                                      MOD-TIPRODSTA-IN-ATTR               
059700              END-IF                                                      
059800           ELSE                                                           
059900              MOVE MID-TIPRODSTA        TO DAT-I-TIDATUM                  
060000              MOVE 'AAMMDD'             TO DAT-KDDATFORM                  
060100                                                                          
060200              CALL WDATKONV USING DAT-KDDATFORM                           
060300                                  DAT-I-TIDATUM                           
060400                                  DAT-O-TIDATUM                           
060500                                  DAT-KDSVAR                              
060600                                                                          
060700              IF DAT-KDSVAR-OK                                            
060800                 MOVE MFS-NUM-FAELT-RAETT                                 
060900                                        TO MOD-TIPRODSTA-IN-ATTR          
061000              ELSE                                                        
061100                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRODSTA-IN-ATTR          
061200                 MOVE NEJ               TO SW-INPUT-RAETT                 
061300              END-IF                                                      
061400              MOVE MFS-ROER-EJ-FAELT    TO MOD-TIPRODSTA-IN               
061500           END-IF                                                         
061600                                                                          
061700           IF MID-TIFINLEV = ALL '+'                                      
061800              MOVE MFS-RENSA-FAELT      TO MOD-TIFINLEV-IN                
061900              IF MID-KDCMD-INSERT                                         
062000                 IF MID-TIPRODSTA = 111111                                
062100                    MOVE MFS-NUM-FAELT-RAETT                              
062200                                           TO MOD-TIFINLEV-IN-ATTR        
062300                 ELSE                                                     
062400                    MOVE MFS-NUM-FAELT-FEL TO MOD-TIFINLEV-IN-ATTR        
062500                    MOVE NEJ               TO SW-INPUT-RAETT              
062600                 END-IF                                                   
062700              ELSE                                                        
062800                 MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIFINLEV-IN-ATTR        
062900              END-IF                                                      
063000           ELSE                                                           
063100              IF MID-TIPRODSTA = 111111                                   
063200                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIFINLEV-IN-ATTR           
063300                 MOVE NEJ               TO SW-INPUT-RAETT                 
063400              ELSE                                                        
063500                 MOVE MID-TIFINLEV         TO DAT-I-TIDATUM               
063600                 MOVE 'AAMMDD'             TO DAT-KDDATFORM               
063700                                                                          
063800                 CALL WDATKONV USING DAT-KDDATFORM                        
063900                                     DAT-I-TIDATUM                        
064000                                     DAT-O-TIDATUM                        
064100                                     DAT-KDSVAR                           
064200                 IF DAT-KDSVAR-OK                                         
064300                    MOVE MFS-NUM-FAELT-RAETT                              
064400                                        TO MOD-TIFINLEV-IN-ATTR           
064500                 ELSE                                                     
064600                    MOVE MFS-NUM-FAELT-FEL                                
064700                                        TO MOD-TIFINLEV-IN-ATTR           
064800                    MOVE NEJ            TO SW-INPUT-RAETT                 
064900                 END-IF                                                   
065000              END-IF                                                      
065100              MOVE MFS-ROER-EJ-FAELT    TO MOD-TIFINLEV-IN                
065200           END-IF                                                         
065300                                                                          
065400           PERFORM BA-KOLLA-TIAAVV                                        
065500                                                                          
065600           IF MID-KDAGE = ALL '+'                                         
065700              MOVE MFS-RENSA-FAELT      TO MOD-KDAGE-IN                   
065800              IF MID-KDCMD-INSERT                                         
065900                MOVE MFS-ALFA-FAELT-FEL TO MOD-KDAGE-IN-ATTR              
066000                MOVE NEJ                TO SW-INPUT-RAETT                 
066100              ELSE                                                        
066200                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAGE-IN-ATTR            
066300              END-IF                                                      
066400           ELSE                                                           
066500              IF WS-KDPRODSL = '13'                                       
066600                 IF MID-KDAGE NUMERIC                                     
066700                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDAGE-IN-ATTR          
066800                    MOVE NEJ                TO SW-INPUT-RAETT             
066900                 ELSE                                                     
067000                    MOVE MFS-ALFA-FAELT-RAETT                             
067100                                           TO MOD-KDAGE-IN-ATTR           
067200                 END-IF                                                   
067300              ELSE                                                        
067400                 IF MID-KDAGE NUMERIC OR MID-KDAGE = SPACE                
067500                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDAGE-IN-ATTR          
067600                    MOVE NEJ                TO SW-INPUT-RAETT             
067700                 ELSE                                                     
067800                    MOVE MFS-ALFA-FAELT-RAETT                             
067900                                            TO MOD-KDAGE-IN-ATTR          
068000                 END-IF                                                   
068100              END-IF                                                      
068200              MOVE MFS-ROER-EJ-FAELT    TO MOD-KDAGE-IN                   
068300           END-IF                                                         
068400        END-IF                                                            
068500     END-IF                                                               
068600                                                                          
068700     PERFORM S05-CHECK-SUPPLIER                                           
068800                                                                          
068900     IF SW-INPUT-RAETT = NEJ                                              
069000        PERFORM S04-ROER-EJ-FAELT                                         
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400 BA-KOLLA-TIAAVV   SECTION.                                               
069500                                                                          
069600     IF MID-TIAAVV-FOM = ALL '+'                                          
069700        MOVE MFS-RENSA-FAELT  TO MOD-TIAAVV-IN-FOM                        
069800        IF MID-KDCMD-INSERT                                               
069900           MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-FOM-ATTR               
070000           MOVE NEJ               TO SW-INPUT-RAETT                       
070100        ELSE                                                              
070200           MOVE MFS-NUM-FAELT-RAETT TO                                    
070300                                MOD-TIAAVV-IN-FOM-ATTR                    
070400        END-IF                                                            
070500     ELSE                                                                 
070600        IF MID-TIAAVV-FOM = ZERO                                          
070700           MOVE MFS-NUM-FAELT-RAETT                                       
070800                    TO MOD-TIAAVV-IN-FOM-ATTR                             
070900           MOVE ZERO   TO WS-TIPROINF-FOM                                 
071000        ELSE                                                              
071100          MOVE MID-TIAAVV-FOM       TO DAT-I-TIDATUM                      
071200          MOVE 'AAVV'               TO DAT-KDDATFORM                      
071300                                                                          
071400          CALL WDATKONV USING DAT-KDDATFORM                               
071500                              DAT-I-TIDATUM                               
071600                              DAT-O-TIDATUM                               
071700                              DAT-KDSVAR                                  
071800          IF DAT-KDSVAR-OK                                                
071900             MOVE MFS-NUM-FAELT-RAETT                                     
072000                          TO MOD-TIAAVV-IN-FOM-ATTR                       
072100             MOVE DAT-TIAAMMDD  TO WS-TIPROINF-FOM                        
072200          ELSE                                                            
072300             MOVE MFS-NUM-FAELT-FEL                                       
072400                          TO MOD-TIAAVV-IN-FOM-ATTR                       
072500             MOVE NEJ               TO SW-INPUT-RAETT                     
072600          END-IF                                                          
072700        END-IF                                                            
072800        MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAAVV-IN-FOM                    
072900     END-IF                                                               
073000                                                                          
073100     EJECT                                                                
073200                                                                          
073300     IF MID-TIAAVV-TOM = ALL '+'                                          
073400        MOVE MFS-RENSA-FAELT  TO MOD-TIAAVV-IN-TOM                        
073500        IF MID-KDCMD-INSERT                                               
073600           MOVE MFS-NUM-FAELT-FEL TO MOD-TIAAVV-IN-TOM-ATTR               
073700           MOVE NEJ               TO SW-INPUT-RAETT                       
073800        ELSE                                                              
073900           MOVE MFS-NUM-FAELT-RAETT TO                                    
074000                                MOD-TIAAVV-IN-TOM-ATTR                    
074100        END-IF                                                            
074200     ELSE                                                                 
074300        IF MID-TIAAVV-TOM = ZERO                                          
074400           MOVE MFS-NUM-FAELT-RAETT                                       
074500                        TO MOD-TIAAVV-IN-TOM-ATTR                         
074600           MOVE ZERO    TO WS-TIPROINF-TOM                                
074700        ELSE                                                              
074800           MOVE MID-TIAAVV-TOM       TO DAT-I-TIDATUM                     
074900           MOVE 'AAVV'               TO DAT-KDDATFORM                     
075000           CALL WDATKONV USING DAT-KDDATFORM                              
075100                               DAT-I-TIDATUM                              
075200                               DAT-O-TIDATUM                              
075300                               DAT-KDSVAR                                 
075400           IF DAT-KDSVAR-OK                                               
075500              MOVE MFS-NUM-FAELT-RAETT                                    
075600                                TO MOD-TIAAVV-IN-TOM-ATTR                 
075700              MOVE DAT-TIAAMMDD TO WS-TIPROINF-TOM                        
075800           ELSE                                                           
075900              MOVE MFS-NUM-FAELT-FEL                                      
076000                           TO MOD-TIAAVV-IN-TOM-ATTR                      
076100              MOVE NEJ     TO SW-INPUT-RAETT                              
076200           END-IF                                                         
076300        END-IF                                                            
076400        MOVE MFS-ROER-EJ-FAELT    TO MOD-TIAAVV-IN-TOM                    
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800 C-KOLLA-MOT-BASEN SECTION.                                               
076900     SKIP2                                                                
077000     MOVE MID-IDPROJ                   TO W-IDPROJ                        
077100                                                                          
077200     IF MID-IDPROJOBJ = ALL '+'                                           
077300        MOVE SPACE                     TO W-IDPROJOBJ                     
077400     ELSE                                                                 
077500        MOVE MID-IDPROJOBJ             TO W-IDPROJOBJ                     
077600     END-IF                                                               
077700                                                                          
077800     MOVE MID-IDPROJK                  TO W-IDPROJK                       
077900     PERFORM IMS-GHNP-WLXXAQ11-UNIK                                       
078000                                                                          
078100     IF MID-KDCMD-INSERT                                                  
078200         IF SEGMENT-FINNS                                                 
078300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJK-IN-ATTR                 
078400                                      MOD-IDPROJ-IN2-ATTR                 
078500           IF MID-IDPROJOBJ = ALL '+'                                     
078600             CONTINUE                                                     
078700           ELSE                                                           
078800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJOBJ-IN-ATTR             
078900           END-IF                                                         
079000                                                                          
079100           MOVE NEJ TO SW-INPUT-RAETT                                     
079200           MOVE FEL-6 (SPAR-TEXT-IND) TO MOD-TEMFSINF                     
079300         ELSE                                                             
079400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR               
079500                                        MOD-IDPROJ-IN2-ATTR               
079600           IF MID-IDPROJOBJ = ALL '+'                                     
079700             CONTINUE                                                     
079800           ELSE                                                           
079900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJOBJ-IN-ATTR           
080000           END-IF                                                         
080100         END-IF                                                           
080200     ELSE                                                                 
080300       IF SEGMENT-FINNS                                                   
080400         IF MID-KDCMD-REPLACE                                             
080500           PERFORM CA-BEHANDLA-LOEPANDE-PROJ                              
080600         END-IF                                                           
080700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN2-ATTR                 
080800                                      MOD-IDPROJOBJ-IN-ATTR               
080900                                      MOD-IDPROJK-IN-ATTR                 
081000       ELSE                                                               
081100         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-IN2-ATTR                   
081200                                    MOD-IDPROJOBJ-IN-ATTR                 
081300                                    MOD-IDPROJK-IN-ATTR                   
081400         MOVE NEJ TO SW-INPUT-RAETT                                       
081500       END-IF                                                             
081600     END-IF                                                               
081700                                                                          
081800     IF SW-INPUT-RAETT = NEJ                                              
081900        PERFORM S04-ROER-EJ-FAELT                                         
082000     END-IF.                                                              
082100     EJECT                                                                
082200 CA-BEHANDLA-LOEPANDE-PROJ  SECTION.                                      
082300     SKIP1                                                                
082400     IF MID-TIPRODSTA = ALL '+'                                           
082500       IF WLXXAQ-1132-TIPRODSTA = +111111                                 
082600         MOVE NEJ                    TO SW-INPUT-RAETT                    
082700         MOVE MFS-NUM-FAELT-FEL      TO MOD-TIFINLEV-IN-ATTR              
082800       END-IF                                                             
082900     ELSE                                                                 
083000       IF MID-TIPRODSTA = 111111                                          
083100         MOVE ZERO                   TO MID-TIFINLEV                      
083200       END-IF                                                             
083300     END-IF                                                               
083400     IF MID-TIFINLEV = ALL '+'                                            
083500       IF WLXXAQ-1132-TIFINLEV = ZERO                                     
083600         IF MID-TIPRODSTA = 111111                                        
083700           CONTINUE                                                       
083800         ELSE                                                             
083900           MOVE NEJ                 TO SW-INPUT-RAETT                     
084000           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIFINLEV-IN-ATTR               
084100         END-IF                                                           
084200       END-IF                                                             
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600 D-UPPDATERA-OCH-VISA-BILD SECTION.                                       
084700     SKIP2                                                                
084800     IF MID-KDCMD-INSERT                                                  
084900       PERFORM DA-NYUPPLAGG                                               
085000     ELSE                                                                 
085100       IF MID-KDCMD-REPLACE                                               
085200         PERFORM DB-AENDRING                                              
085300       ELSE                                                               
085400         IF MID-KDCMD-DELETE                                              
085500           PERFORM DC-BORTTAG                                             
085600         END-IF                                                           
085700       END-IF                                                             
085800     END-IF                                                               
085900                                                                          
086000     PERFORM S02-LAGG-UT-MODDEN                                           
086100                                                                          
086200     PERFORM S03-RENSA-MOD-INMATNINGSFAELT                                
086300                                                                          
086400     MOVE MFS-FORMATETS-ATTR         TO MOD-IDPROJ-IN2-ATTR               
086500                                        MOD-IDPROJOBJ-IN-ATTR             
086600                                        MOD-IDPROJK-IN-ATTR               
086700                                        MOD-KVNYART-IN-ATTR               
086800                                        MOD-KVNYRES-IN-ATTR               
086900                                        MOD-TIPRODSTA-IN-ATTR             
087000                                        MOD-TIFINLEV-IN-ATTR              
087100                                        MOD-TIAAVV-IN-FOM-ATTR            
087200                                        MOD-TIAAVV-IN-TOM-ATTR            
087300                                        MOD-KDAGE-IN-ATTR                 
087400     MOVE MED-3 (SPAR-TEXT-IND)      TO MOD-TEMFSINF.                     
087500     EJECT                                                                
087600 DA-NYUPPLAGG SECTION.                                                    
087700     SKIP2                                                                
087800     MOVE MID-IDPROJ           TO WLXXAQ-1132-IDPROJ                      
087900     MOVE MID-IDPROJK          TO WLXXAQ-1132-IDPROJK                     
088000     MOVE MID-TIPRODSTA        TO WLXXAQ-1132-TIPRODSTA                   
088100                                                                          
088200     IF MID-IDPROJOBJ = ALL '+'                                           
088300        MOVE SPACE             TO WLXXAQ-1132-IDPROJOBJ                   
088400     ELSE                                                                 
088500        MOVE MID-IDPROJOBJ     TO WLXXAQ-1132-IDPROJOBJ                   
088600     END-IF                                                               
088700                                                                          
088800     IF MID-TIFINLEV = ALL '+'                                            
088900        MOVE ZERO              TO WLXXAQ-1132-TIFINLEV                    
089000     ELSE                                                                 
089100        MOVE MID-TIFINLEV      TO WLXXAQ-1132-TIFINLEV                    
089200     END-IF                                                               
089300                                                                          
089400     IF MID-KVNYART = ALL '+'                                             
089500        MOVE ZERO              TO WLXXAQ-1132-KVNYART                     
089600     ELSE                                                                 
089700        MOVE MID-KVNYART       TO WLXXAQ-1132-KVNYART                     
089800     END-IF                                                               
089900                                                                          
090000     IF MID-KVNYRES = ALL '+'                                             
090100        MOVE ZERO              TO WLXXAQ-1132-KVNYRES                     
090200     ELSE                                                                 
090300        MOVE MID-KVNYRES       TO WLXXAQ-1132-KVNYRES                     
090400     END-IF                                                               
090500                                                                          
090600     IF MID-TIAAVV-FOM = ALL '+'                                          
090700        MOVE ZERO         TO WLXXAQ-1132-TIPROINF-FOM                     
090800     ELSE                                                                 
090900        MOVE WS-TIPROINF-FOM   TO WLXXAQ-1132-TIPROINF-FOM                
091000     END-IF                                                               
091100                                                                          
091200     IF MID-TIAAVV-TOM = ALL '+'                                          
091300        MOVE ZERO         TO WLXXAQ-1132-TIPROINF-TOM                     
091400     ELSE                                                                 
091500        MOVE WS-TIPROINF-TOM   TO WLXXAQ-1132-TIPROINF-TOM                
091600     END-IF                                                               
091700                                                                          
091800     IF MID-KDAGE = ALL '+' OR SPACE                                      
091900        MOVE SPACE             TO WLXXAQ-1132-KDAGE                       
092000     ELSE                                                                 
092100        MOVE MID-KDAGE          TO WLXXAQ-1132-KDAGE                      
092200     END-IF                                                               
092300                                                                          
092400     MOVE SPAR-DAGENS-DATUM    TO WLXXAQ-1132-TIUPPDAT                    
092500                                                                          
092600* ÖVRIGA FÄLT I 1132-SEGMENTET FÅR DEFAULT-VÄRDEN:                        
092700                                                                          
092800     MOVE ZERO                 TO WLXXAQ-1132-IDLKTO                      
092900                                  WLXXAQ-1132-RESLJUST-C1                 
093000                                  WLXXAQ-1132-RESLJUST-C2                 
093100                                                                          
093200     MOVE LOW-VALUE            TO WLXXAQ-1132-LOWVALUE                    
093300                                                                          
093400     PERFORM  IMS-ISRT-1132                                               
093500                                                                          
093600     MOVE MID-IDPROJ           TO W-IDPROJ                                
093700     IF MID-IDPROJOBJ = ALL '+'                                           
093800        MOVE SPACE             TO W-IDPROJOBJ                             
093900     ELSE                                                                 
094000        MOVE MID-IDPROJOBJ     TO W-IDPROJOBJ                             
094100     END-IF                                                               
094200                                                                          
094300     MOVE MID-IDPROJK          TO W-IDPROJK.                              
094400     EJECT                                                                
094500 DB-AENDRING SECTION.                                                     
094600     SKIP2                                                                
094700     IF MID-IDPROJ = ALL '+'                                              
094800        CONTINUE                                                          
094900     ELSE                                                                 
095000        MOVE MID-IDPROJ     TO WLXXAQ-1132-IDPROJ                         
095100     END-IF                                                               
095200                                                                          
095300     IF MID-IDPROJOBJ = ALL '+'                                           
095400        CONTINUE                                                          
095500     ELSE                                                                 
095600        MOVE MID-IDPROJOBJ  TO WLXXAQ-1132-IDPROJOBJ                      
095700     END-IF                                                               
095800                                                                          
095900     IF MID-KVNYART = ALL '+'                                             
096000        CONTINUE                                                          
096100     ELSE                                                                 
096200        MOVE MID-KVNYART    TO WLXXAQ-1132-KVNYART                        
096300     END-IF                                                               
096400                                                                          
096500     IF MID-KVNYRES = ALL '+'                                             
096600        CONTINUE                                                          
096700     ELSE                                                                 
096800        MOVE MID-KVNYRES    TO WLXXAQ-1132-KVNYRES                        
096900     END-IF                                                               
097000                                                                          
097100     IF MID-TIPRODSTA = ALL '+'                                           
097200        CONTINUE                                                          
097300     ELSE                                                                 
097400        MOVE MID-TIPRODSTA  TO WLXXAQ-1132-TIPRODSTA                      
097500     END-IF                                                               
097600                                                                          
097700     IF MID-TIFINLEV  = ALL '+'                                           
097800        CONTINUE                                                          
097900     ELSE                                                                 
098000        MOVE MID-TIFINLEV   TO WLXXAQ-1132-TIFINLEV                       
098100     END-IF                                                               
098200                                                                          
098300     IF MID-TIAAVV-FOM = ALL '+'                                          
098400        CONTINUE                                                          
098500     ELSE                                                                 
098600        MOVE WS-TIPROINF-FOM TO WLXXAQ-1132-TIPROINF-FOM                  
098700     END-IF                                                               
098800                                                                          
098900     IF MID-TIAAVV-TOM = ALL '+'                                          
099000        CONTINUE                                                          
099100     ELSE                                                                 
099200        MOVE WS-TIPROINF-TOM TO WLXXAQ-1132-TIPROINF-TOM                  
099300     END-IF                                                               
099400                                                                          
099500     IF MID-KDAGE = ALL '+'                                               
099600        CONTINUE                                                          
099700     ELSE                                                                 
099800        MOVE MID-KDAGE         TO WLXXAQ-1132-KDAGE                       
099900     END-IF                                                               
100000                                                                          
100100     MOVE SPAR-DAGENS-DATUM TO WLXXAQ-1132-TIUPPDAT                       
100200                                                                          
100300     PERFORM IMS-REPL-1132                                                
100400                                                                          
100500     MOVE MID-IDPROJ           TO W-IDPROJ                                
100600     IF MID-IDPROJOBJ = ALL '+'                                           
100700        MOVE SPACE             TO W-IDPROJOBJ                             
100800     ELSE                                                                 
100900        MOVE MID-IDPROJOBJ     TO W-IDPROJOBJ                             
101000     END-IF                                                               
101100                                                                          
101200     MOVE MID-IDPROJK          TO W-IDPROJK                               
101300                                                                          
101400**** PERFORM DBA-SKAPA-TRANS-TILL-1109 ????????                           
101500**** PERFORM DBC-SKAPA-TRANS-TILL-NYPON ??????????                        
101600     .                                                                    
101700     EJECT                                                                
101800 DC-BORTTAG SECTION.                                                      
101900     SKIP2                                                                
102000     PERFORM IMS-DLET-1132                                                
102100                                                                          
102200     MOVE SPACE TO W-IDPROJ                                               
102300                   W-IDPROJOBJ                                            
102400                   W-IDPROJK                                              
102500     .                                                                    
102600     EJECT                                                                
102700 E-VISA-BILD SECTION.                                                     
102800     SKIP2                                                                
102900     PERFORM IMS-GNP-WLXXAQ11                                             
103000                                                                          
103100     IF SEGMENT-FINNS                                                     
103200       MOVE WLXXAQ-1132-IDPROJ TO MOD-IDPROJ-1                            
103300       MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-IDPROJOBJ-1                      
103400       MOVE WLXXAQ-1132-IDPROJK  TO MOD-IDPROJK-1                         
103500       PERFORM S01-LAGG-UT-MODDEN                                         
103600     ELSE                                                                 
103700       MOVE SPACE TO W-IDPROJ                                             
103800                     W-IDPROJOBJ                                          
103900                     W-IDPROJK                                            
104000       PERFORM IMS-GNP-WLXXAQ11-NEXT                                      
104100       IF SEGMENT-FINNS                                                   
104200         MOVE WLXXAQ-1132-IDPROJ    TO MOD-IDPROJ-1                       
104300         MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-IDPROJOBJ-1                    
104400         MOVE WLXXAQ-1132-IDPROJK   TO MOD-IDPROJK-1                      
104500         PERFORM S01-LAGG-UT-MODDEN                                       
104600       ELSE                                                               
104700         MOVE FEL-5 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                       
104800         MOVE SPACE                 TO MOD-IDPROJ-1                       
104900                                       MOD-IDPROJ-11                      
105000                                       MOD-IDPROJOBJ-1                    
105100                                       MOD-IDPROJOBJ-11                   
105200                                       MOD-IDPROJK-1                      
105300                                       MOD-IDPROJK-11                     
105400       END-IF                                                             
105500     END-IF                                                               
105600                                                                          
105700     IF MID-KDPRODSL-IN = ALL '+'  AND EGEN-BILD                          
105800       IF MID-INFALT    = ALL '+'                                         
105900         CONTINUE                                                         
106000       ELSE                                                               
106100         IF MID-KDCMD = ALL '+'                                           
106200           MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-IN                     
106300         ELSE                                                             
106400           MOVE MFS-ROER-EJ-FAELT     TO MOD-KDCMD-IN                     
106500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
106600         END-IF                                                           
106700                                                                          
106800         IF MID-IDPROJ = ALL '+'                                          
106900           MOVE MFS-RENSA-FAELT       TO MOD-IDPROJ-IN2                   
107000         ELSE                                                             
107100           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPROJ-IN2                   
107200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN2-ATTR               
107300         END-IF                                                           
107400                                                                          
107500         IF MID-IDPROJOBJ = ALL '+'                                       
107600           MOVE MFS-RENSA-FAELT       TO MOD-IDPROJOBJ-IN                 
107700         ELSE                                                             
107800           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPROJOBJ-IN                 
107900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJOBJ-IN-ATTR             
108000         END-IF                                                           
108100                                                                          
108200         IF MID-IDPROJK = ALL '+'                                         
108300           MOVE MFS-RENSA-FAELT       TO MOD-IDPROJK-IN                   
108400         ELSE                                                             
108500           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPROJK-IN                   
108600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR               
108700         END-IF                                                           
108800                                                                          
108900         IF MID-KVNYART = ALL '+'                                         
109000           MOVE MFS-RENSA-FAELT       TO MOD-KVNYART-IN                   
109100         ELSE                                                             
109200           MOVE MFS-ROER-EJ-FAELT     TO MOD-KVNYART-IN                   
109300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNYART-IN-ATTR               
109400         END-IF                                                           
109500                                                                          
109600         IF MID-KVNYRES = ALL '+'                                         
109700           MOVE MFS-RENSA-FAELT       TO MOD-KVNYRES-IN                   
109800         ELSE                                                             
109900           MOVE MFS-ROER-EJ-FAELT     TO MOD-KVNYRES-IN                   
110000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNYRES-IN-ATTR               
110100         END-IF                                                           
110200                                                                          
110300         IF MID-TIPRODSTA = ALL '+'                                       
110400           MOVE MFS-RENSA-FAELT       TO MOD-TIPRODSTA-IN                 
110500         ELSE                                                             
110600           MOVE MFS-ROER-EJ-FAELT     TO MOD-TIPRODSTA-IN                 
110700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIPRODSTA-IN-ATTR             
110800         END-IF                                                           
110900                                                                          
111000         IF MID-TIFINLEV = ALL '+'                                        
111100           MOVE MFS-RENSA-FAELT       TO MOD-TIFINLEV-IN                  
111200         ELSE                                                             
111300           MOVE MFS-ROER-EJ-FAELT     TO MOD-TIFINLEV-IN                  
111400           MOVE MFS-NUM-FAELT-RAETT   TO MOD-TIFINLEV-IN-ATTR             
111500         END-IF                                                           
111600                                                                          
111700         IF MID-TIAAVV-FOM = ALL '+'                                      
111800           MOVE MFS-RENSA-FAELT     TO MOD-TIAAVV-IN-FOM                  
111900         ELSE                                                             
112000           MOVE MFS-ROER-EJ-FAELT   TO MOD-TIAAVV-IN-FOM                  
112100           MOVE MFS-NUM-FAELT-RAETT                                       
112200                              TO MOD-TIAAVV-IN-FOM-ATTR                   
112300         END-IF                                                           
112400                                                                          
112500         IF MID-TIAAVV-TOM = ALL '+'                                      
112600           MOVE MFS-RENSA-FAELT     TO MOD-TIAAVV-IN-TOM                  
112700         ELSE                                                             
112800           MOVE MFS-ROER-EJ-FAELT   TO MOD-TIAAVV-IN-TOM                  
112900           MOVE MFS-NUM-FAELT-RAETT                                       
113000                              TO MOD-TIAAVV-IN-TOM-ATTR                   
113100         END-IF                                                           
113200                                                                          
113300         IF MID-KDAGE = ALL '+'                                           
113400           MOVE MFS-RENSA-FAELT     TO MOD-KDAGE-IN                       
113500         ELSE                                                             
113600           MOVE MFS-ROER-EJ-FAELT   TO MOD-KDAGE-IN                       
113700           MOVE MFS-ALFA-FAELT-RAETT                                      
113800                              TO MOD-KDAGE-IN-ATTR                        
113900         END-IF                                                           
114000                                                                          
114100         MOVE MED-2 (SPAR-TEXT-IND)   TO MOD-TEMFSINF                     
114200       END-IF                                                             
114300     ELSE                                                                 
114400        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
114500     END-IF.                                                              
114600     EJECT                                                                
114700 S01-LAGG-UT-MODDEN SECTION.                                              
114800     SKIP2                                                                
114900     SET MOD-RAD-IND TO 1                                                 
115000     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
115100                                                                          
115200       IF SEGMENT-FINNS                                                   
115300         MOVE WLXXAQ-1132-IDPROJ TO MOD-RAD-IDPROJ  (MOD-RAD-IND)         
115400         MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-RAD-IDPROJOBJ                  
115500                                                    (MOD-RAD-IND)         
115600         MOVE WLXXAQ-1132-IDPROJK TO MOD-RAD-IDPROJK(MOD-RAD-IND)         
115700         MOVE WLXXAQ-1132-KVNYART TO MOD-RAD-KVNYART(MOD-RAD-IND)         
115800         MOVE WLXXAQ-1132-KVNYRES TO MOD-RAD-KVNYRES(MOD-RAD-IND)         
115900         MOVE WLXXAQ-1132-TIPRODSTA TO MOD-RAD-TIPRODSTA                  
116000                                                    (MOD-RAD-IND)         
116100         MOVE WLXXAQ-1132-TIFINLEV TO MOD-RAD-TIFINLEV                    
116200                                                    (MOD-RAD-IND)         
116300***************************************************************           
116400         MOVE WLXXAQ-1132-TIPROINF-FOM TO DAT-I-TIDATUM                   
116500         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
116600                                                                          
116700         CALL WDATKONV USING DAT-KDDATFORM                                
116800                             DAT-I-TIDATUM                                
116900                             DAT-O-TIDATUM                                
117000                             DAT-KDSVAR                                   
117100                                                                          
117200         IF DAT-KDSVAR-OK                                                 
117300            MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-FOM                          
117400         ELSE                                                             
117500            MOVE ZERO          TO WS-TIAAVV-FOM                           
117600         END-IF                                                           
117700         MOVE WS-TIAAVV-FOM TO MOD-RAD-TIAAVV-FOM                         
117800                                         (MOD-RAD-IND)                    
117900***************************************************************           
118000***************************************************************           
118100         MOVE WLXXAQ-1132-TIPROINF-TOM TO DAT-I-TIDATUM                   
118200         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
118300                                                                          
118400         CALL WDATKONV USING DAT-KDDATFORM                                
118500                             DAT-I-TIDATUM                                
118600                             DAT-O-TIDATUM                                
118700                             DAT-KDSVAR                                   
118800                                                                          
118900         IF DAT-KDSVAR-OK                                                 
119000            MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-TOM                          
119100         ELSE                                                             
119200            MOVE ZERO          TO WS-TIAAVV-TOM                           
119300         END-IF                                                           
119400         MOVE WS-TIAAVV-TOM        TO MOD-RAD-TIAAVV-TOM                  
119500                                            (MOD-RAD-IND)                 
119600***************************************************************           
119700                                                                          
119800         MOVE WLXXAQ-1132-KDAGE    TO MOD-RAD-KDAGE (MOD-RAD-IND)         
119900         PERFORM IMS-GNP-WLXXAQ11                                         
120000       ELSE                                                               
120100         MOVE MFS-RENSA-FAELT TO MOD-RAD-IDPROJ   (MOD-RAD-IND)           
120200                                 MOD-RAD-IDPROJOBJ(MOD-RAD-IND)           
120300                                 MOD-RAD-IDPROJK (MOD-RAD-IND)            
120400                                 MOD-RAD-KVNYART (MOD-RAD-IND)            
120500                                 MOD-RAD-KVNYRES (MOD-RAD-IND)            
120600                                 MOD-RAD-TIPRODSTA(MOD-RAD-IND)           
120700                                 MOD-RAD-TIFINLEV (MOD-RAD-IND)           
120800                                MOD-RAD-TIAAVV-FOM(MOD-RAD-IND)           
120900                                MOD-RAD-TIAAVV-TOM(MOD-RAD-IND)           
121000                                 MOD-RAD-KDAGE(MOD-RAD-IND)               
121100       END-IF                                                             
121200       SET MOD-RAD-IND UP BY 1                                            
121300     END-PERFORM                                                          
121400                                                                          
121500     IF SEGMENT-FINNS                                                     
121600        MOVE WLXXAQ-1132-IDPROJ     TO MOD-IDPROJ-11                      
121700        MOVE WLXXAQ-1132-IDPROJOBJ  TO MOD-IDPROJOBJ-11                   
121800        MOVE WLXXAQ-1132-IDPROJK    TO MOD-IDPROJK-11                     
121900        MOVE MED-1 (SPAR-TEXT-IND)  TO MOD-TEMFSINF                       
122000     ELSE                                                                 
122100        MOVE SPACE                  TO MOD-IDPROJ-11                      
122200                                       MOD-IDPROJOBJ-11                   
122300                                       MOD-IDPROJK-11                     
122400     END-IF.                                                              
122500     EJECT                                                                
122600 S02-LAGG-UT-MODDEN SECTION.                                              
122700                                                                          
122800     PERFORM IMS-GNP-WLXXAQ11-NEXT                                        
122900                                                                          
123000     SET MOD-RAD-IND TO 1                                                 
123100     IF SEGMENT-FINNS                                                     
123200       MOVE WLXXAQ-1132-IDPROJ       TO MOD-IDPROJ-1                      
123300       MOVE WLXXAQ-1132-IDPROJOBJ    TO MOD-IDPROJOBJ-1                   
123400       MOVE WLXXAQ-1132-IDPROJK      TO MOD-IDPROJK-1                     
123500     END-IF                                                               
123600                                                                          
123700     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
123800       IF SEGMENT-FINNS                                                   
123900         MOVE WLXXAQ-1132-IDPROJ   TO MOD-RAD-IDPROJ(MOD-RAD-IND)         
124000         MOVE WLXXAQ-1132-IDPROJOBJ TO MOD-RAD-IDPROJOBJ                  
124100                                                    (MOD-RAD-IND)         
124200         MOVE WLXXAQ-1132-IDPROJK TO MOD-RAD-IDPROJK(MOD-RAD-IND)         
124300         MOVE WLXXAQ-1132-KVNYART TO MOD-RAD-KVNYART(MOD-RAD-IND)         
124400         MOVE WLXXAQ-1132-KVNYRES TO MOD-RAD-KVNYRES(MOD-RAD-IND)         
124500         MOVE WLXXAQ-1132-TIPRODSTA TO MOD-RAD-TIPRODSTA                  
124600                                                    (MOD-RAD-IND)         
124700         MOVE WLXXAQ-1132-TIFINLEV TO MOD-RAD-TIFINLEV                    
124800                                                    (MOD-RAD-IND)         
124900         MOVE WLXXAQ-1132-TIFINLEV TO MOD-RAD-TIFINLEV                    
125000                                                    (MOD-RAD-IND)         
125100***************************************************************           
125200         MOVE WLXXAQ-1132-TIPROINF-FOM TO DAT-I-TIDATUM                   
125300         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
125400                                                                          
125500         CALL WDATKONV USING DAT-KDDATFORM                                
125600                             DAT-I-TIDATUM                                
125700                             DAT-O-TIDATUM                                
125800                             DAT-KDSVAR                                   
125900                                                                          
126000         IF DAT-KDSVAR-OK                                                 
126100            MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-FOM                          
126200         ELSE                                                             
126300            MOVE ZERO          TO WS-TIAAVV-FOM                           
126400         END-IF                                                           
126500         MOVE WS-TIAAVV-FOM TO MOD-RAD-TIAAVV-FOM                         
126600                                         (MOD-RAD-IND)                    
126700***************************************************************           
126800***************************************************************           
126900         MOVE WLXXAQ-1132-TIPROINF-TOM TO DAT-I-TIDATUM                   
127000         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
127100                                                                          
127200         CALL WDATKONV USING DAT-KDDATFORM                                
127300                             DAT-I-TIDATUM                                
127400                             DAT-O-TIDATUM                                
127500                             DAT-KDSVAR                                   
127600                                                                          
127700         IF DAT-KDSVAR-OK                                                 
127800            MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-TOM                          
127900         ELSE                                                             
128000            MOVE ZERO          TO WS-TIAAVV-TOM                           
128100         END-IF                                                           
128200         MOVE WS-TIAAVV-TOM        TO MOD-RAD-TIAAVV-TOM                  
128300                                            (MOD-RAD-IND)                 
128400***************************************************************           
128500         MOVE WLXXAQ-1132-KDAGE    TO MOD-RAD-KDAGE(MOD-RAD-IND)          
128600         PERFORM IMS-GNP-WLXXAQ11                                         
128700       ELSE                                                               
128800         MOVE MFS-RENSA-FAELT TO MOD-RAD-IDPROJ   (MOD-RAD-IND)           
128900                                 MOD-RAD-IDPROJOBJ(MOD-RAD-IND)           
129000                                 MOD-RAD-IDPROJK  (MOD-RAD-IND)           
129100                                 MOD-RAD-KVNYART  (MOD-RAD-IND)           
129200                                 MOD-RAD-KVNYRES  (MOD-RAD-IND)           
129300                                 MOD-RAD-TIPRODSTA(MOD-RAD-IND)           
129400                                 MOD-RAD-TIFINLEV (MOD-RAD-IND)           
129500                             MOD-RAD-TIAAVV-FOM(MOD-RAD-IND)              
129600                             MOD-RAD-TIAAVV-TOM(MOD-RAD-IND)              
129700                                 MOD-RAD-KDAGE (MOD-RAD-IND)              
129800       END-IF                                                             
129900       IF MOD-RAD-IND = 1                                                 
130000         IF MID-KDCMD-REPLACE OR  MID-KDCMD-INSERT                        
130100*          PERFORM S021-LYS-UPP-FALT                                      
130200           CONTINUE                                                       
130300         END-IF                                                           
130400       END-IF                                                             
130500                                                                          
130600       SET MOD-RAD-IND UP BY 1                                            
130700     END-PERFORM                                                          
130800                                                                          
130900     IF SEGMENT-FINNS                                                     
131000        MOVE WLXXAQ-1132-IDPROJ     TO MOD-IDPROJ-11                      
131100        MOVE WLXXAQ-1132-IDPROJOBJ  TO MOD-IDPROJOBJ-11                   
131200        MOVE WLXXAQ-1132-IDPROJK    TO MOD-IDPROJK-11                     
131300        MOVE MED-1 (SPAR-TEXT-IND)  TO MOD-TEMFSINF                       
131400     ELSE                                                                 
131500        MOVE SPACE                  TO MOD-IDPROJ-11                      
131600                                       MOD-IDPROJOBJ-11                   
131700                                       MOD-IDPROJK-11                     
131800     END-IF.                                                              
131900     EJECT                                                                
132000 S021-LYS-UPP-FALT SECTION.                                               
132100     SKIP3                                                                
132200     IF MID-KDCMD-INSERT                                                  
132300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-IDPROJ-ATTR (1)              
132400                                     MOD-RAD-IDPROJK-ATTR (1)             
132500     ELSE                                                                 
132600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-IDPROJ-ATTR (1)              
132700                                     MOD-RAD-IDPROJK-ATTR (1)             
132800                                                                          
132900       IF MID-KVNYART = ALL '+'                                           
133000         CONTINUE                                                         
133100       ELSE                                                               
133200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-KVNYART-ATTR(1)            
133300       END-IF                                                             
133400                                                                          
133500       IF MID-KVNYRES = ALL '+'                                           
133600         CONTINUE                                                         
133700       ELSE                                                               
133800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-KVNYRES-ATTR(1)            
133900       END-IF                                                             
134000                                                                          
134100       IF MID-TIPRODSTA = ALL '+'                                         
134200         CONTINUE                                                         
134300       ELSE                                                               
134400         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-TIPRODSTA-ATTR(1)          
134500       END-IF                                                             
134600                                                                          
134700       IF MID-TIFINLEV = ALL '+'                                          
134800         CONTINUE                                                         
134900       ELSE                                                               
135000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RAD-TIFINLEV-ATTR (1)          
135100       END-IF                                                             
135200       IF MID-TIAAVV-FOM = ALL '+'                                        
135300         CONTINUE                                                         
135400       ELSE                                                               
135500         MOVE MFS-ADD-LYS-UPP-FAELT                                       
135600                      TO MOD-RAD-TIAAVV-FOM-ATTR (1)                      
135700       END-IF                                                             
135800                                                                          
135900       IF MID-TIAAVV-TOM = ALL '+'                                        
136000         CONTINUE                                                         
136100       ELSE                                                               
136200         MOVE MFS-ADD-LYS-UPP-FAELT                                       
136300                      TO MOD-RAD-TIAAVV-TOM-ATTR (1)                      
136400       END-IF                                                             
136500                                                                          
136600       IF MID-KDAGE = ALL '+'                                             
136700         CONTINUE                                                         
136800       ELSE                                                               
136900         MOVE MFS-ADD-LYS-UPP-FAELT                                       
137000                      TO MOD-RAD-KDAGE-ATTR (1)                           
137100       END-IF                                                             
137200     END-IF                                                               
137300     .                                                                    
137400     EJECT                                                                
137500 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
137600     SKIP3                                                                
137700     MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-IN                           
137800                                   MOD-IDPROJ-IN2                         
137900                                   MOD-IDPROJOBJ-IN                       
138000                                   MOD-IDPROJK-IN                         
138100                                   MOD-KVNYART-IN                         
138200                                   MOD-KVNYRES-IN                         
138300                                   MOD-TIPRODSTA-IN                       
138400                                   MOD-TIFINLEV-IN                        
138500                                   MOD-TIAAVV-IN-FOM                      
138600                                   MOD-TIAAVV-IN-TOM                      
138700                                   MOD-KDAGE-IN                           
138800     .                                                                    
138900     EJECT                                                                
139000 S04-ROER-EJ-FAELT SECTION.                                               
139100     SKIP3                                                                
139200     SET MOD-RAD-IND                 TO 1                                 
139300                                                                          
139400     PERFORM UNTIL MOD-RAD-IND > MAX-RAD-IND                              
139500       MOVE MFS-ROER-EJ-FAELT TO                                          
139600                             MOD-RAD-IDPROJ   (MOD-RAD-IND)               
139700                             MOD-RAD-IDPROJOBJ(MOD-RAD-IND)               
139800                             MOD-RAD-IDPROJK  (MOD-RAD-IND)               
139900                             MOD-RAD-KVNYART  (MOD-RAD-IND)               
140000                             MOD-RAD-KVNYRES  (MOD-RAD-IND)               
140100                             MOD-RAD-TIPRODSTA(MOD-RAD-IND)               
140200                             MOD-RAD-TIFINLEV (MOD-RAD-IND)               
140300                          MOD-RAD-TIAAVV-FOM (MOD-RAD-IND)                
140400                          MOD-RAD-TIAAVV-TOM (MOD-RAD-IND)                
140500                             MOD-RAD-KDAGE    (MOD-RAD-IND)               
140600        SET MOD-RAD-IND UP BY 1                                           
140700     END-PERFORM                                                          
140800     .                                                                    
140900     EJECT                                                                
141000 S05-CHECK-SUPPLIER SECTION.                                              
141100     SKIP2                                                                
141200     MOVE JA        TO SW-SUPPLIER-OK                                     
141300                                                                          
141400     MOVE 1         TO LEV05-IX                                           
141500     PERFORM UNTIL LEV05-IX > 2                                           
141600       IF MSGI-KDARBTYP-SEC-IDLEV = TAB-KDARBTYP-LEV (LEV05-IX)           
141700         MOVE NEJ   TO SW-SUPPLIER-OK                                     
141800         IF WS-KDPRODSL = TAB-KDPRODSL (LEV05-IX)                         
141900           MOVE JA  TO SW-SUPPLIER-OK                                     
142000           MOVE 2   TO LEV05-IX                                           
142100         END-IF                                                           
142200       END-IF                                                             
142300       ADD 1        TO LEV05-IX                                           
142400     END-PERFORM                                                          
142500                                                                          
142600     IF SW-SUPPLIER-OK = NEJ                                              
142700       MOVE NEJ TO SW-INPUT-RAETT                                         
142800     END-IF                                                               
142900     .                                                                    
143000     EJECT                                                                
143100                                                                          
143200* IMS SEKTIONER                                                           
143300     SKIP3                                                                
143400 IMS-GET-MSG SECTION.                                                     
143500     SKIP2                                                                
143600     MOVE '  QC' TO GODK-STATUSKODER                                      
143700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
143800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143900     PERFORM IMS-STATUS-KONTROLL.                                         
144000     SKIP3                                                                
144100 IMS-INSERT-MSG SECTION.                                                  
144200     SKIP2                                                                
144300                                                                          
144400     IF NOT ENGLISH-TEXT                                                  
144500        MOVE '0'          TO MFS-KDHUVOMR                                 
144600     END-IF                                                               
144700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
144800     MOVE SPACE TO GODK-STATUSKODER                                       
144900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
145000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145100     PERFORM IMS-STATUS-KONTROLL.                                         
145200     EJECT                                                                
145300 IMS-GU-WLXXAQ01 SECTION.                                                 
145400     SKIP2                                                                
145500     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
145600             DELIMITED BY SIZE INTO SSA1                                  
145700     MOVE '  GE' TO GODK-STATUSKODER                                      
145800     CALL CBLTDLI USING GU WLXXAQ-PCB DLI-IO-AREA SSA1                    
145900     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
146000     PERFORM IMS-STATUS-KONTROLL.                                         
146100     SKIP3                                                                
146200 IMS-GNP-WLXXAQ11 SECTION.                                                
146300     SKIP2                                                                
146400     STRING 'WLXXAQ11(WDGXKEY =>' W-1132KEY-X ')'                         
146500             DELIMITED BY SIZE INTO SSA1                                  
146600     MOVE '  GE' TO GODK-STATUSKODER                                      
146700     CALL CBLTDLI USING GNP WLXXAQ-PCB DLI-IO-AREA SSA1                   
146800     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
146900     PERFORM IMS-STATUS-KONTROLL.                                         
147000     SKIP3                                                                
147100     EJECT                                                                
147200 IMS-GHNP-WLXXAQ11-UNIK SECTION.                                          
147300     SKIP2                                                                
147400     STRING 'WLXXAQ11(WDGXKEY  =' W-1132KEY-X ')'                         
147500             DELIMITED BY SIZE INTO SSA1                                  
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GHNP WLXXAQ-PCB DLI-IO-AREA SSA1                  
147800     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
147900     PERFORM IMS-STATUS-KONTROLL.                                         
148000     SKIP3                                                                
148100 IMS-GNP-WLXXAQ11-NEXT  SECTION.                                          
148200     SKIP2                                                                
148300     STRING 'WLXXAQ11(WDGXKEY =>' W-1132KEY-X ')'                         
148400             DELIMITED BY SIZE INTO SSA1                                  
148500     MOVE '  GE' TO GODK-STATUSKODER                                      
148600     CALL CBLTDLI USING GNP WLXXAQ-PCB DLI-IO-AREA SSA1                   
148700     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
148800     PERFORM IMS-STATUS-KONTROLL.                                         
148900     SKIP3                                                                
149000     EJECT                                                                
149100 IMS-REPL-1132 SECTION.                                                   
149200     SKIP2                                                                
149300     MOVE '  '   TO GODK-STATUSKODER                                      
149400     CALL CBLTDLI USING REPL WLXXAQ-PCB DLI-IO-AREA                       
149500     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
149600     PERFORM IMS-STATUS-KONTROLL.                                         
149700     SKIP3                                                                
149800 IMS-ISRT-1132 SECTION.                                                   
149900     SKIP2                                                                
150000     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
150100             DELIMITED BY SIZE INTO SSA1                                  
150200     MOVE   'WLXXAQ11 ' TO SSA2                                           
150300     MOVE '  '   TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING ISRT WLXXAQ-PCB DLI-IO-AREA SSA1 SSA2             
150500     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
150600     PERFORM IMS-STATUS-KONTROLL.                                         
150700     SKIP3                                                                
150800 IMS-DLET-1132 SECTION.                                                   
150900     SKIP2                                                                
151000     MOVE '  '   TO GODK-STATUSKODER                                      
151100     CALL  CBLTDLI  USING DLET WLXXAQ-PCB DLI-IO-AREA                     
151200     MOVE WLXXAQ-STATUS-CODE TO STATUS-WS                                 
151300     PERFORM IMS-STATUS-KONTROLL.                                         
151400     SKIP3                                                                
151500 IMS-STATUS-KONTROLL SECTION.                                             
151600     SET STATUS-IX TO 1                                                   
151700     SEARCH GODK-STATUS                                                   
151800       AT END                                                             
151900         CALL FELLOG                                                      
152000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
152100         CONTINUE                                                         
152200     END-SEARCH                                                           
152300     .                                                                    
