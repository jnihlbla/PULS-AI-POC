000100 ID DIVISION.                                                             
000200 PROGRAM-ID.          W0060500.                                           
000300 AUTHOR.              MATS.                                               
000400 DATE-WRITTEN.        JANUARI 1986.                                       
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*       VISAR, UPPDATERAR, TAR BORT SAMT STARTAR PACKNINGSTRANSAR         
000800*       HÄNDELSETYP 4316, SOM VANLIG BILD, RESP                           
000900*       STARTAR OCH RENSAR DITO SOM BAKGRUNDSTRANS.                       
001000*                                                                         
001100*       FÖR ATT SÄTTA EN DATARAD TILL ALL '+': MATA IN HELA RADEN         
001200*                                              MED '-'                    
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION:  W0T605                                             
001600*        MID:          W0I60501                                           
001700*                      W0I60502                                           
001800*    UTDATA.                                                              
001900*        MOD:          W0O60501    (EGEN-BILD)                            
002000*        TRANSAKTION:  W4T314, W4T315, W4T317 ELLER W4T398X               
002100*    SUBPROGRAM.                                                          
002200*        FELLOG                                                           
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP3                                                                
002801                                                                          
002810*    -- CHECKED BY WY2000                                                 
002900 77  FELTEXT                     PIC X(8)    VALUE SPACE.                 
002910 77  JA                          PIC X(1)    VALUE 'J'.                   
003000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003100 77  ISRT-MSG                    PIC X(1)    VALUE 'N'.                   
003200 77  ISRT-ALT                    PIC X(1)    VALUE 'J'.                   
003300 77  WS-IDPRODNR                 PIC X(7).                                
003400 77  WS-IDPTYP                   PIC X(3).                                
003500 77  WS-IDKOLLI                  PIC X(5).                                
003600 77  WS-KDTRSTAT                 PIC X(1).                                
003700 77  RIX                         PIC S9(9)   VALUE ZERO COMP SYNC.        
003800 77  MAX-MODLAENGD               PIC S9(4)   VALUE +887 COMP SYNC.        
003900 77  MAX-RADER-PLUS-1            PIC S9(3)   VALUE +6.                    
003910 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
003920 77  PGMPOS                      PIC X(16)   VALUE SPACE.                 
004000     SKIP2                                                                
004100 77  SW-ALLT-OK                  PIC X(1).                                
004200     88  ALLT-OK                             VALUE 'J'.                   
004300     SKIP2                                                                
004400 77  SW-NYA-NYCKLAR              PIC X(1).                                
004500     88  NYA-NYCKLAR                         VALUE 'J'.                   
004600     SKIP2                                                                
004700 77  W-IDTRANS                   PIC X(4).                                
004800     88  EGEN-BILD                           VALUE '0605'.                
004900     88  KOLLIVIS                            VALUE '4314' '4315'          
005000                                                   '4317' '4398'.         
005100     88  ORDERVIS                            VALUE '4301' '4302'          
005200                                                   '4303' '4306'          
005300                                                   '4391' '4392'          
005400                                                   '4393' '4394'          
005500                                                   '4396'.                
005600     EJECT                                                                
005650                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800     03  WVARIF                  PIC X(8)    VALUE 'WVARIF  '.            
005830     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005840     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     SKIP3                                                                
005910 01  WVARIFAREA                  PIC X(16)   VALUE 'WVARIFAREA'.          
006000 01  PARM-TILL-WVARIF.                                                    
006100     03  CALL0-LANGD1            PIC S9(9)   COMP.                        
006200     03  CALL0-LANGD2            PIC S9(9)   COMP.                        
006300     03  CALL0-KDSVAR            PIC X(1).                                
006400         88  CALL0-KDSVAR-OK                 VALUE SPACE.                 
006500         88  CALL0-KDSVAR-FEL                VALUE 'F'.                   
006600     SKIP2                                                                
006610 01  TRANS-VISAD              PIC X(16) VALUE 'TRANSVISAD-AREA'.          
006700 01  W-TRANS-VISAD.                                                       
006800     03  W-IDPRODNR-VISAD        PIC S9(7)   COMP-3.                      
006900     03  W-IDPTYP-VISAD          PIC X(3).                                
007000     03  W-IDKOLLI-VISAD         PIC S9(5)   COMP-3.                      
007100     03  FILLER                  PIC X(10)   VALUE LOW-VALUE.             
007200     03  W-KDTRSTAT-VISAD        PIC S9(1)   COMP-3.                      
007300     03  W-LL-VISAD              PIC S9(4)   COMP.                        
007400     03  FILLER                  PIC X(2)    VALUE LOW-VALUE.             
007500     03  W-DATA                  PIC X(345).                              
007600     03  FILLER REDEFINES W-DATA.                                         
007700         05  W-KDTRANS-VISAD     PIC X(8).                                
007800         05  W-IDTRANS-VISAD     PIC X(4).                                
007900         05  W-KDMFSFOR-VISAD    PIC X(1).                                
008000         05  W-DATA-VISAD        PIC X(332).                              
008100     EJECT                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008300     03  W-4315-WDGXKEY-X.                                                
008400         05  FILLER              PIC X(4)    VALUE '4315'.                
008500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008600     SKIP2                                                                
008700     03  W-4316-WDGXKEY-X.                                                
008800         05  W-4316-IDPRODNR     PIC S9(7)   COMP-3.                      
008900         05  W-4316-IDPTYP       PIC X(3).                                
009000         05  W-4316-IDKOLLI      PIC S9(5)   COMP-3.                      
009100         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
009200     SKIP2                                                                
009300     03  W-4316-KDTRSTAT-X.                                               
009400         05  W-4316-KDTRSTAT     PIC S9(1)   COMP-3.                      
009500     EJECT                                                                
009600 01  MEDDELANDE.                                                          
009700*                                                                         
009800     03  FEL-1.                                                           
009900         05  FILLER              PIC X(40)                                
010000             VALUE 'UPPLYSTA FÄLT FEL         '.                          
010100                                                                          
010200     03  FEL-2.                                                           
010300         05  FILLER              PIC X(40)                                
010400             VALUE 'NYCKLAR FEL IFYLLDA       '.                          
010500                                                                          
010600     03  FEL-3-1.                                                         
010700         05  FILLER              PIC X(40)                                
010800             VALUE 'TRANSAKTION SAKNAS A      '.                          
010900                                                                          
010910     03  FEL-3-2.                                                         
010920         05  FILLER              PIC X(40)                                
010930             VALUE 'TRANSAKTION SAKNAS B      '.                          
010940                                                                          
010950     03  FEL-3-3.                                                         
010960         05  FILLER              PIC X(40)                                
010970             VALUE 'TRANSAKTION SAKNAS C      '.                          
010980                                                                          
010990     03  FEL-3-4.                                                         
010991         05  FILLER              PIC X(40)                                
010992             VALUE 'TRANSAKTION SAKNAS D      '.                          
010993                                                                          
011000     03  FEL-4.                                                           
011100         05  FILLER              PIC X(40)                                
011200             VALUE 'TRANS MED STATUS 2 FINNS  '.                          
011300                                                                          
011400     03  FEL-5.                                                           
011500         05  FILLER              PIC X(40)                                
011600             VALUE 'DETTA ÄR FÖRSTA SIDAN     '.                          
011700                                                                          
011800     03  FEL-6.                                                           
011900         05  FILLER              PIC X(40)                                
012000             VALUE 'DETTA ÄR SISTA SIDAN      '.                          
012100                                                                          
012200     03  FEL-7.                                                           
012300         05  FILLER              PIC X(40)                                
012400             VALUE 'STARTFÖRSÖK MED FEL STATUS'.                          
012500                                                                          
012600     03  INFO-1.                                                          
012700         05  FILLER              PIC X(61)                                
012800             VALUE 'TRANSAKTION STARTAD       '.                          
012900                                                                          
013000     03  INFO-2.                                                          
013100         05  FILLER              PIC X(61)                                
013200             VALUE 'TRANSAKTION UPPDATERAD    '.                          
013300                                                                          
013400     03  INFO-3.                                                          
013500         05  FILLER              PIC X(61)                                
013600             VALUE 'TRANSAKTION BORTTAGEN     '.                          
013601******END INFO TEXTS********************************                      
013610*                                                                         
013700     SKIP2                                                                
013910 01  FILLER                 PIC X(16) VALUE 'MID W0I60501 MID'.           
014000*    -COPY W0I60501                                                       
014200     SKIP2                                                                
014210 01  FILLER                 PIC X(16) VALUE 'MID W0I60502 MID'.           
014300*    -COPY W0I60502                                                       
014500     SKIP2                                                                
014510 01  FILLER                 PIC X(16) VALUE 'MOD WMSGAREA MOD'.           
014600*01  -COPY WMSGAREA                                                       
014800     SKIP2                                                                
014900*    03  MOD -COPY W0O60501 -RED MSG-AREA                                 
015100     SKIP2                                                                
015110 01  FILLER                 PIC X(16) VALUE 'ALT-IO-AREA     '.           
015200 01  ALT-IO-AREA.                                                         
015300     03  ALT-LL                  PIC S9(4)   COMP.                        
015400     03  ALT-Z1-Z2               PIC X(2)    VALUE LOW-VALUE.             
015500     03  ALT-KDTRANS             PIC X(8).                                
015600     03  ALT-IDTRANS             PIC X(4).                                
015700     03  ALT-KDMFSFOR            PIC X(1).                                
015800     03  ALT-DATA                PIC X(332).                              
015900     EJECT                                                                
015910 01  FILLER                 PIC X(8) VALUE 'WMFSAREA'.                    
016000*01  -COPY WMFSAREA                                                       
016200     EJECT                                                                
016300 01  IMS-WS.                                                              
016400     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
016500     SKIP3                                                                
016600*                            *** STATUSKOD FRÅN IMS                       
016700     03  STATUS-WS               PIC XX.                                  
016800         88  SEGMENT-FINNS                   VALUE '  '.                  
016900         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
017000     SKIP3                                                                
017100     03  GODK-STATUSKODER.                                                
017200         05  GODK-STATUS OCCURS 5  INDEXED BY STATUS-IX PIC XX.           
017300     SKIP3                                                                
017400 01  SSA1                        PIC X(64).                               
017500 01  SSA2                        PIC X(64).                               
017600     EJECT                                                                
017700*                            *** IMS FUNKTIONSKODER                       
017800*01  -COPY W0003                                                          
018000     EJECT                                                                
018100*                            *** DLI INPUT-OUTPUT AREA                    
018110 01  FILLER             PIC X(12) VALUE 'DLI-IO-AREA'.                    
018200 01  DLI-IO-AREA.                                                         
018300     03  IO-AREA                 PIC X(400)  VALUE SPACE.                 
018400     SKIP3                                                                
018500*    03  WLXXDL11 -COPY WDGX4316 -RED IO-AREA.                            
018700     EJECT                                                                
018800     03  FILLER REDEFINES IO-AREA.                                        
018900         05  FILLER              PIC X(25).                               
019000         05  W-DATA-RAD          OCCURS 5                                 
019100                                 PIC X(70).                               
019200     EJECT                                                                
019300 LINKAGE SECTION.                                                         
019400     SKIP2                                                                
019500*01  -COPY W0009             -PRE MSG-                                    
019700     EJECT                                                                
019800*01  -COPY W0009             -PRE W4T314-                                 
020000     EJECT                                                                
020100*01  -COPY W0009             -PRE W4T315-                                 
020300     EJECT                                                                
020400*01  -COPY W0009             -PRE W4T317-                                 
020600     EJECT                                                                
020700*01  -COPY W0009             -PRE W4T398U-                                
020900     EJECT                                                                
021000*01  -COPY W0008             -PRE 4315-                                   
021200         05  FILLER          PIC X.                                       
021300     EJECT                                                                
021400 PROCEDURE DIVISION USING MSG-PCB W4T314-PCB W4T315-PCB                   
021500                          W4T317-PCB W4T398U-PCB 4315-PCB.                
021600     ENTRY 'DLITCBL' USING MSG-PCB W4T314-PCB W4T315-PCB                  
021700                           W4T317-PCB W4T398U-PCB 4315-PCB.               
021800     SKIP2                                                                
021900     PERFORM IMS-GET-MSG                                                  
022000                                                                          
022100     IF SEGMENT-FINNS                                                     
022200                                                                          
022300         PERFORM A-INIT-SPARA-INPUT                                       
022400         IF ORDERVIS OR KOLLIVIS                                          
022500             MOVE NEJ TO ISRT-MSG                                         
022600             IF MFS-UPDATE                                                
022700                 IF KOLLIVIS AND MID-FLSVAR = NEJ                         
022710                     MOVE  'POS1'        TO FELTEXT                       
022800                     CALL FELLOG                                          
022900                 END-IF                                                   
023000                 PERFORM B-FORBERED-TRANS                                 
023100             END-IF                                                       
023200         ELSE                                                             
023300             IF EGEN-BILD                                                 
023400                 PERFORM C-FIXA-NYCKLAR                                   
023500                 IF MFS-UPDATE                                            
023600                     IF NOT NYA-NYCKLAR                                   
023700                         PERFORM D-KOLLA-INFAELT                          
023800                         IF ALLT-OK                                       
023900                             IF MID-KDCMDVAL = 'DEL'                      
024000                                 PERFORM E-TAG-BORT-TRANS                 
024100                             ELSE                                         
024200                                 IF MID-KDCMDVAL = 'STA'                  
024300                                   IF MID-KDTRSTAT-GL = 1                 
024400                                     PERFORM F-STARTA-GIVEN-TRANS         
024500                                   ELSE                                   
024600                                     MOVE FEL-7 TO MOD-TEMFSFEL           
024700                                   END-IF                                 
024800                                 ELSE                                     
024900                                     IF MID-KDCMDVAL = 'INS'              
025000                                        PERFORM G-INSERTA-TRANS           
025100                                     ELSE                                 
025200                                        PERFORM H-UPPDATERA-TRANS         
025300                                     END-IF                               
025400                                 END-IF                                   
025500                             END-IF                                       
025600                         ELSE                                             
025700                             MOVE FEL-1 TO MOD-TEMFSFEL                   
025800                         END-IF                                           
025900                     ELSE                                                 
026000                         MOVE FEL-2 TO MOD-TEMFSFEL                       
026100                     END-IF                                               
026200                 ELSE                                                     
026300                     PERFORM I-VISA-TRANS                                 
026400                 END-IF                                                   
026500             ELSE                                                         
026600                 PERFORM S03-RENSA-ALLT                                   
026700             END-IF                                                       
026800         END-IF                                                           
026900                                                                          
027000         IF ISRT-ALT = JA                                                 
027100             EVALUATE ALT-KDTRANS                                         
027200               WHEN 'W4T314  ' PERFORM IMS-INSERT-ALT-W4T314              
027300               WHEN 'W4T315  ' PERFORM IMS-INSERT-ALT-W4T315              
027400               WHEN 'W4T317  ' PERFORM IMS-INSERT-ALT-W4T317              
027500               WHEN 'W4T398U ' PERFORM IMS-INSERT-ALT-W4T398U             
027510               WHEN 'W4T398X ' PERFORM IMS-INSERT-ALT-W4T398U             
027520               MOVE  'POS2'        TO FELTEXT                             
027600               WHEN OTHER CALL FELLOG                                     
027700             END-EVALUATE                                                 
027800         END-IF                                                           
027900                                                                          
028000         IF ISRT-MSG = JA                                                 
028100             PERFORM IMS-INSERT-MSG                                       
028200         END-IF                                                           
028300     END-IF                                                               
028400                                                                          
028500     MOVE ZERO TO RETURN-CODE                                             
028600     GOBACK                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 A-INIT-SPARA-INPUT SECTION.                                              
029000     SKIP2                                                                
029010     MOVE 'STA A-INIT SEC'              TO PGMPOS                         
029100     IF MSG-DUBBLA-TRANSKODER                                             
029200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I60501               
029300                                               MID-W0I60502               
029400         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS W-IDTRANS                      
029500         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
029600     ELSE                                                                 
029700         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W0I60501                 
029800                                             MID-W0I60502                 
029900         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS W-IDTRANS                      
030000         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
030100     END-IF                                                               
030200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
030300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
030400                                                                          
030500     MOVE LOW-VALUE TO MSG-AREA                                           
030600     MOVE 'W0O60501' TO MFS-IDMOD                                         
030700     MOVE '0605' TO MOD-IDTRANS                                           
030800     MOVE MAX-MODLAENGD TO MSG-KVLL                                       
030900                                                                          
031000     MOVE NEJ TO ISRT-ALT                                                 
031100     MOVE JA TO ISRT-MSG                                                  
031200                SW-ALLT-OK                                                
031300     .                                                                    
031400     EJECT                                                                
031500 B-FORBERED-TRANS SECTION.                                                
031600     SKIP2                                                                
031610     MOVE 'STA B-FORB SEC'              TO PGMPOS                         
031700     MOVE NEJ TO ISRT-MSG                                                 
031800     PERFORM IMS-GU-4315                                                  
031900                                                                          
032000     MOVE +2 TO W-4316-KDTRSTAT                                           
032100     PERFORM IMS-GHNP-4316-STATUS-FIRST                                   
032200                                                                          
032300     IF (SEGMENT-FINNS AND KOLLIVIS) OR                                   
032400             (SEGMENT-SAKNAS AND ORDERVIS)                                
032500         IF KOLLIVIS                                                      
032600             PERFORM IMS-DELETE                                           
032700         END-IF                                                           
032800         MOVE +1 TO W-4316-KDTRSTAT                                       
032900         PERFORM IMS-GHNP-4316-STATUS-FIRST                               
033000         IF SEGMENT-FINNS                                                 
033100             MOVE +2 TO 4316-KDTRSTAT                                     
033200             PERFORM IMS-REPLACE                                          
033300             MOVE 4316-INTRANS TO ALT-IO-AREA                             
033400             MOVE '0605' TO ALT-IDTRANS                                   
033500             MOVE JA TO ISRT-ALT                                          
033600         END-IF                                                           
033700     ELSE                                                                 
033800         IF KOLLIVIS                                                      
033810             MOVE  'POS3'        TO FELTEXT                               
033900             CALL FELLOG                                                  
034000         END-IF                                                           
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 C-FIXA-NYCKLAR SECTION.                                                  
034410     MOVE 'STA C-FIXA SEC'              TO PGMPOS                         
034500     SKIP2                                                                
034600     MOVE NEJ TO SW-NYA-NYCKLAR                                           
034700                                                                          
034800     IF MID-KDTRSTAT-IN NUMERIC                                           
034900        MOVE JA TO SW-NYA-NYCKLAR                                         
035000        MOVE MID-KDTRSTAT-IN TO WS-KDTRSTAT MOD-KDTRSTAT-UT               
035100        MOVE SPACE TO WS-IDPRODNR                                         
035200                      WS-IDPTYP                                           
035300                      WS-IDKOLLI                                          
035400        MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                           
035500                                MOD-IDPTYP-UT                             
035600                                MOD-IDKOLLI-UT                            
035700     ELSE                                                                 
035800        IF MID-IDPRODNR-IN = ALL '+'                                      
035900                AND MID-IDPTYP-IN = ALL '+'                               
036000                AND MID-IDKOLLI-IN = ALL '+'                              
036100                AND MID-KDTRSTAT-UT NUMERIC                               
036200           MOVE MID-KDTRSTAT-UT TO WS-KDTRSTAT MOD-KDTRSTAT-UT            
036300           MOVE SPACE TO WS-IDPRODNR                                      
036400                         WS-IDPTYP                                        
036500                         WS-IDKOLLI                                       
036600           MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                        
036700                                   MOD-IDPTYP-UT                          
036800                                   MOD-IDKOLLI-UT                         
036900        ELSE                                                              
037000           IF MID-IDPRODNR-IN NOT = ALL '+'                               
037100              MOVE JA TO SW-NYA-NYCKLAR                                   
037200              MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                         
037300           ELSE                                                           
037400              MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                         
037500           END-IF                                                         
037600           IF WS-IDPRODNR NOT = SPACE                                     
037700              INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO         
037800           END-IF                                                         
037900                                                                          
038000           IF MID-IDPTYP-IN NOT = ALL '+'                                 
038100              MOVE JA TO SW-NYA-NYCKLAR                                   
038200              MOVE MID-IDPTYP-IN TO WS-IDPTYP                             
038300           ELSE                                                           
038400              MOVE MID-IDPTYP-UT TO WS-IDPTYP                             
038500           END-IF                                                         
038600           IF WS-IDPTYP NOT = SPACE                                       
038700              INSPECT WS-IDPTYP REPLACING LEADING SPACE BY ZERO           
038800           END-IF                                                         
038900                                                                          
039000           IF MID-IDKOLLI-IN NOT = ALL '+'                                
039100              MOVE JA TO SW-NYA-NYCKLAR                                   
039200              MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                           
039300           ELSE                                                           
039400              MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                           
039500           END-IF                                                         
039600           IF WS-IDKOLLI NOT = SPACE                                      
039700              INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO          
039800           END-IF                                                         
039900                                                                          
040000           MOVE WS-IDPRODNR TO MOD-IDPRODNR-UT                            
040100           INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE        
040200                                                                          
040300           MOVE WS-IDPTYP TO MOD-IDPTYP-UT                                
040400           INSPECT MOD-IDPTYP-UT REPLACING LEADING ZERO BY SPACE          
040500                                                                          
040600           MOVE WS-IDKOLLI TO MOD-IDKOLLI-UT                              
040700           INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE         
040800                                                                          
040900           MOVE SPACE TO WS-KDTRSTAT                                      
041000           MOVE MFS-RENSA-FAELT TO MOD-KDTRSTAT-UT                        
041100                                                                          
041200        END-IF                                                            
041300     END-IF                                                               
041400                                                                          
041500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
041600                             MOD-IDPRODNR-IN                              
041700                             MOD-IDPTYP-IN                                
041800                             MOD-IDKOLLI-IN                               
041900                             MOD-KDTRSTAT-IN                              
042000                             MOD-KDCMDVAL                                 
042100                             MOD-IDPRODNR-NY                              
042200                             MOD-IDPTYP-NY                                
042300                             MOD-IDKOLLI-NY                               
042400                             MOD-KDTRSTAT-NY                              
042500                             MOD-LL-NY                                    
042600     .                                                                    
042700     EJECT                                                                
042800 D-KOLLA-INFAELT SECTION.                                                 
042810     MOVE 'STA D-KOLLA SEC'              TO PGMPOS                        
042900     SKIP2                                                                
043000     MOVE JA TO SW-ALLT-OK                                                
043100                                                                          
043200     IF MID-KDCMDVAL = 'DEL' OR 'STA'                                     
043300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR                   
043400         PERFORM DA-KOLLA-ATT-INGET-AER-IFYLLT                            
043500     ELSE                                                                 
043600         IF MID-KDCMDVAL = 'INS' OR ALL '+'                               
043700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR               
043800             PERFORM DB-KOLLA-INDATA                                      
043900         ELSE                                                             
044000             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                 
044100             MOVE NEJ TO SW-ALLT-OK                                       
044200         END-IF                                                           
044300     END-IF                                                               
044400                                                                          
044500     IF NOT ALLT-OK                                                       
044600         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                           
044700                                   MOD-IDPRODNR-GL                        
044800                                   MOD-IDPRODNR-NY                        
044900                                   MOD-IDPTYP-GL                          
045000                                   MOD-IDPTYP-NY                          
045100                                   MOD-IDKOLLI-GL                         
045200                                   MOD-IDKOLLI-NY                         
045300                                   MOD-KDTRSTAT-GL                        
045400                                   MOD-KDTRSTAT-NY                        
045500                                   MOD-LL-GL                              
045600                                   MOD-LL-NY                              
045700         MOVE +1 TO RIX                                                   
045800         PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                         
045900             MOVE MFS-ROER-EJ-FAELT TO MOD-TEDATA-GL (RIX)                
046000                                       MOD-TEDATA-NY (RIX)                
046100             ADD +1 TO RIX                                                
046200         END-PERFORM                                                      
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 DA-KOLLA-ATT-INGET-AER-IFYLLT SECTION.                                   
046610     MOVE 'STA DA-KOLLA SEC'              TO PGMPOS                       
046700     SKIP2                                                                
046800     IF MID-IDPRODNR-NY NOT = ALL '+'                                     
046900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-NY-ATTR                   
047000         MOVE NEJ TO SW-ALLT-OK                                           
047100     END-IF                                                               
047200                                                                          
047300     IF MID-IDPTYP-NY NOT = ALL '+'                                       
047400         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPTYP-NY-ATTR                     
047500         MOVE NEJ TO SW-ALLT-OK                                           
047600     END-IF                                                               
047700                                                                          
047800     IF MID-IDKOLLI-NY NOT = ALL '+'                                      
047900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-NY-ATTR                    
048000         MOVE NEJ TO SW-ALLT-OK                                           
048100     END-IF                                                               
048200                                                                          
048300     IF MID-KDTRSTAT-NY NOT = ALL '+'                                     
048400         MOVE MFS-NUM-FAELT-FEL TO MOD-KDTRSTAT-NY-ATTR                   
048500         MOVE NEJ TO SW-ALLT-OK                                           
048600     END-IF                                                               
048700                                                                          
048800     IF MID-LL-NY NOT = ALL '+'                                           
048900         MOVE MFS-NUM-FAELT-FEL TO MOD-LL-NY-ATTR                         
049000         MOVE NEJ TO SW-ALLT-OK                                           
049100     END-IF                                                               
049200                                                                          
049300     MOVE +1 TO RIX                                                       
049400     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
049500         IF MID-TEDATA-NY (RIX) NOT = ALL '+'                             
049600           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEDATA-NY-ATTR (RIX)            
049700           MOVE NEJ TO SW-ALLT-OK                                         
049800         END-IF                                                           
049900         ADD +1 TO RIX                                                    
050000     END-PERFORM                                                          
050100     .                                                                    
050200     EJECT                                                                
050300 DB-KOLLA-INDATA SECTION.                                                 
050310     MOVE 'STA DB-KOLLA SEC'              TO PGMPOS                       
050400     SKIP2                                                                
050500     IF MID-IDPRODNR-NY NOT = ALL '+'                                     
050600         IF MID-IDPRODNR-NY NUMERIC                                       
050700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRODNR-NY-ATTR             
050800         ELSE                                                             
050900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-NY-ATTR               
051000             MOVE NEJ TO SW-ALLT-OK                                       
051100         END-IF                                                           
051200     ELSE                                                                 
051300         IF MID-KDCMDVAL = 'INS'                                          
051400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-NY-ATTR               
051500             MOVE NEJ TO SW-ALLT-OK                                       
051600         END-IF                                                           
051700     END-IF                                                               
051800                                                                          
051900     IF MID-IDPTYP-NY NOT = ALL '+'                                       
052000         IF MID-IDPTYP-NY NUMERIC                                         
052100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPTYP-NY-ATTR               
052200         ELSE                                                             
052300             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPTYP-NY-ATTR                 
052400             MOVE NEJ TO SW-ALLT-OK                                       
052500         END-IF                                                           
052600     ELSE                                                                 
052700         IF MID-KDCMDVAL = 'INS'                                          
052800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPTYP-NY-ATTR                 
052900             MOVE NEJ TO SW-ALLT-OK                                       
053000         END-IF                                                           
053100     END-IF                                                               
053200                                                                          
053300     IF MID-IDKOLLI-NY NOT = ALL '+'                                      
053400         IF MID-IDKOLLI-NY NUMERIC                                        
053500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-NY-ATTR              
053600         ELSE                                                             
053700             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-NY-ATTR                
053800             MOVE NEJ TO SW-ALLT-OK                                       
053900         END-IF                                                           
054000     ELSE                                                                 
054100         IF MID-KDCMDVAL = 'INS'                                          
054200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-NY-ATTR                
054300             MOVE NEJ TO SW-ALLT-OK                                       
054400         END-IF                                                           
054500     END-IF                                                               
054600                                                                          
054700     IF MID-KDTRSTAT-NY NOT = ALL '+'                                     
054800         IF MID-KDTRSTAT-NY NUMERIC                                       
054900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTRSTAT-NY-ATTR             
055000         ELSE                                                             
055100             MOVE MFS-NUM-FAELT-FEL TO MOD-KDTRSTAT-NY-ATTR               
055200             MOVE NEJ TO SW-ALLT-OK                                       
055300         END-IF                                                           
055400     ELSE                                                                 
055500         IF MID-KDCMDVAL = 'INS'                                          
055600             MOVE MFS-NUM-FAELT-FEL TO MOD-KDTRSTAT-NY-ATTR               
055700             MOVE NEJ TO SW-ALLT-OK                                       
055800         END-IF                                                           
055900     END-IF                                                               
056000                                                                          
056100     IF MID-LL-NY NOT = ALL '+'                                           
056200         IF MID-LL-NY NUMERIC                                             
056300             IF MID-LL-NY < +350                                          
056400                 MOVE MFS-NUM-FAELT-RAETT TO MOD-LL-NY-ATTR               
056500             ELSE                                                         
056600                 MOVE MFS-NUM-FAELT-FEL TO MOD-LL-NY-ATTR                 
056700                 MOVE NEJ TO SW-ALLT-OK                                   
056800             END-IF                                                       
056900         ELSE                                                             
057000             MOVE MFS-NUM-FAELT-FEL TO MOD-LL-NY-ATTR                     
057100             MOVE NEJ TO SW-ALLT-OK                                       
057200         END-IF                                                           
057300     ELSE                                                                 
057400         IF MID-KDCMDVAL = 'INS'                                          
057500             MOVE MFS-NUM-FAELT-FEL TO MOD-LL-NY-ATTR                     
057600             MOVE NEJ TO SW-ALLT-OK                                       
057700         END-IF                                                           
057800     END-IF                                                               
057900                                                                          
058000     IF MID-TEDATA-NY (1) = ALL '+' AND MID-KDCMDVAL = 'INS'              
058100         MOVE MFS-ALFA-FAELT-FEL TO MOD-TEDATA-NY-ATTR (1)                
058200         MOVE NEJ TO SW-ALLT-OK                                           
058300     END-IF                                                               
058400                                                                          
058500     MOVE +2 TO RIX                                                       
058600     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
058700         IF MID-TEDATA-NY (RIX) NOT = ALL '+'                             
058800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEDATA-NY-ATTR (RIX)        
058900         END-IF                                                           
059000         ADD +1 TO RIX                                                    
059100     END-PERFORM                                                          
059200     .                                                                    
059300     EJECT                                                                
059400 E-TAG-BORT-TRANS SECTION.                                                
059410     MOVE 'STA E-TAG   SEC'              TO PGMPOS                        
059500     SKIP2                                                                
059600     PERFORM IMS-GU-4315                                                  
059700                                                                          
059800     PERFORM S01-LAS-FRAM-TILL-RETT-SEGMENT                               
059900                                                                          
060000     IF CALL0-KDSVAR-OK                                                   
060100         PERFORM IMS-DELETE                                               
060200         MOVE INFO-3 TO MOD-TEMFSINF                                      
060300         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-GL                        
060400                                   MOD-IDPTYP-GL                          
060500                                   MOD-IDKOLLI-GL                         
060600         MOVE MFS-RENSA-FAELT TO MOD-KDTRSTAT-GL                          
060700                                 MOD-LL-GL                                
060800     ELSE                                                                 
060900         PERFORM S02-ROER-EJ-FAELT                                        
061000         MOVE FEL-3-1 TO MOD-TEMFSFEL                                     
061100     END-IF                                                               
061200                                                                          
061300     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
061400                                MOD-IDPRODNR-NY-ATTR                      
061500                                MOD-IDPTYP-NY-ATTR                        
061600                                MOD-IDKOLLI-NY-ATTR                       
061700                                MOD-KDTRSTAT-NY-ATTR                      
061800                                MOD-LL-NY-ATTR                            
061900     .                                                                    
062000     EJECT                                                                
062100 F-STARTA-GIVEN-TRANS SECTION.                                            
062110     MOVE 'STA F-START SEC'              TO PGMPOS                        
062200     SKIP2                                                                
062300     PERFORM IMS-GU-4315                                                  
062400                                                                          
062500     MOVE +2 TO W-4316-KDTRSTAT                                           
062600     PERFORM IMS-GHNP-4316-STATUS-FIRST                                   
062700                                                                          
062800     IF SEGMENT-SAKNAS                                                    
062900                                                                          
063000         PERFORM IMS-GU-4315                                              
063100         PERFORM S01-LAS-FRAM-TILL-RETT-SEGMENT                           
063200         IF CALL0-KDSVAR-OK                                               
063300             PERFORM S02-ROER-EJ-FAELT                                    
063400             MOVE +2 TO 4316-KDTRSTAT MOD-KDTRSTAT-GL                     
063500             PERFORM IMS-REPLACE                                          
063600             MOVE 4316-INTRANS TO ALT-IO-AREA                             
063700             MOVE '0605' TO ALT-IDTRANS                                   
063800             MOVE JA TO ISRT-ALT                                          
063900             MOVE INFO-1 TO MOD-TEMFSINF                                  
064000         ELSE                                                             
064100             MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-GL                    
064200                                       MOD-IDPTYP-GL                      
064300                                       MOD-IDKOLLI-GL                     
064400             MOVE MFS-RENSA-FAELT TO MOD-KDTRSTAT-GL                      
064500                                     MOD-LL-GL                            
064600             MOVE FEL-3-2 TO MOD-TEMFSFEL                                 
064700             MOVE NEJ TO SW-ALLT-OK                                       
064800         END-IF                                                           
064900     ELSE                                                                 
065000         PERFORM S04-FYLL-I-DATA-I-MOD                                    
065100         MOVE FEL-4 TO MOD-TEMFSFEL                                       
065200         MOVE NEJ TO SW-ALLT-OK                                           
065300     END-IF                                                               
065400                                                                          
065500     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
065600                                MOD-IDPRODNR-NY-ATTR                      
065700                                MOD-IDPTYP-NY-ATTR                        
065800                                MOD-IDKOLLI-NY-ATTR                       
065900                                MOD-KDTRSTAT-NY-ATTR                      
066000                                MOD-LL-NY-ATTR                            
066100     .                                                                    
066200     EJECT                                                                
066300 G-INSERTA-TRANS SECTION.                                                 
066310     MOVE 'STA G-INSER SEC'              TO PGMPOS                        
066400     SKIP2                                                                
066500     MOVE MID-IDPRODNR-NY TO 4316-IDPRODNR MOD-IDPRODNR-GL                
066600     MOVE MID-IDPTYP-NY TO 4316-IDPTYP MOD-IDPTYP-GL                      
066700     MOVE MID-IDKOLLI-NY TO 4316-IDKOLLI MOD-IDKOLLI-GL                   
066800     MOVE LOW-VALUE TO 4316-LOWVALUE 4316-Z1 4316-Z2                      
066900     MOVE MID-KDTRSTAT-NY TO 4316-KDTRSTAT MOD-KDTRSTAT-GL                
067000     MOVE MID-LL-NY TO 4316-LL MOD-LL-GL                                  
067100                                                                          
067200     MOVE +1 TO RIX                                                       
067300     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
067400         MOVE MID-TEDATA-NY (RIX) TO W-DATA-RAD (RIX)                     
067500                                     MOD-TEDATA-GL (RIX)                  
067600                                     MOD-TEDATA-NY (RIX)                  
067700         MOVE MFS-FORMATETS-ATTR TO MOD-TEDATA-NY-ATTR (RIX)              
067800         ADD +1 TO RIX                                                    
067900     END-PERFORM                                                          
068000                                                                          
068100     PERFORM IMS-ISRT-4316                                                
068200                                                                          
068300     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
068400                                MOD-IDPRODNR-NY-ATTR                      
068500                                MOD-IDPTYP-NY-ATTR                        
068600                                MOD-IDKOLLI-NY-ATTR                       
068700                                MOD-KDTRSTAT-NY-ATTR                      
068800                                MOD-LL-NY-ATTR                            
068900                                                                          
069000     MOVE INFO-2 TO MOD-TEMFSINF                                          
069100     .                                                                    
069200     EJECT                                                                
069300 H-UPPDATERA-TRANS SECTION.                                               
069310     MOVE 'STA H-UPPDA SEC'              TO PGMPOS                        
069400     SKIP2                                                                
069500     PERFORM IMS-GU-4315                                                  
069600                                                                          
069700     PERFORM S01-LAS-FRAM-TILL-RETT-SEGMENT                               
069800                                                                          
069900     IF CALL0-KDSVAR-OK                                                   
070000        PERFORM S02-ROER-EJ-FAELT                                         
070100        IF MID-IDPRODNR-NY = ALL '+' AND MID-IDPTYP-NY = ALL '+'          
070200                 AND MID-IDKOLLI-NY = ALL '+'                             
070300                                                                          
070400           IF MID-KDTRSTAT-NY NOT = ALL '+'                               
070500              MOVE MID-KDTRSTAT-NY TO 4316-KDTRSTAT                       
070600                                      MOD-KDTRSTAT-GL                     
070700           END-IF                                                         
070800           IF MID-LL-NY NOT = ALL '+'                                     
070900              MOVE MID-LL-NY TO 4316-LL                                   
071000                                MOD-LL-GL                                 
071100           END-IF                                                         
071200           MOVE +1 TO RIX                                                 
071300           PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                       
071400              IF MID-TEDATA-NY (RIX) NOT = ALL '+'                        
071500                 IF MID-TEDATA-NY (RIX) = ALL '-'                         
071600                    MOVE ALL '+' TO W-DATA-RAD (RIX)                      
071700                                    MOD-TEDATA-GL (RIX)                   
071800                                    MOD-TEDATA-NY (RIX)                   
071900                 ELSE                                                     
072000                    MOVE MID-TEDATA-NY (RIX) TO W-DATA-RAD (RIX)          
072100                                              MOD-TEDATA-GL (RIX)         
072200                                              MOD-TEDATA-NY (RIX)         
072300                 END-IF                                                   
072400                                                                          
072500              END-IF                                                      
072600              MOVE MFS-FORMATETS-ATTR TO MOD-TEDATA-NY-ATTR (RIX)         
072700              ADD +1 TO RIX                                               
072800           END-PERFORM                                                    
072900                                                                          
073000           PERFORM IMS-REPLACE                                            
073100                                                                          
073200        ELSE                                                              
073300           PERFORM IMS-DELETE                                             
073400           IF MID-IDPRODNR-NY NOT = ALL '+'                               
073500              MOVE MID-IDPRODNR-NY TO 4316-IDPRODNR                       
073600                                      MOD-IDPRODNR-GL                     
073700           END-IF                                                         
073800           IF MID-IDPTYP-NY NOT = ALL '+'                                 
073900              MOVE MID-IDPTYP-NY TO 4316-IDPTYP                           
074000                                    MOD-IDPTYP-GL                         
074100           END-IF                                                         
074200           IF MID-IDKOLLI-NY NOT = ALL '+'                                
074300              MOVE MID-IDKOLLI-NY TO 4316-IDKOLLI                         
074400                                     MOD-IDKOLLI-GL                       
074500           END-IF                                                         
074600           IF MID-KDTRSTAT-NY NOT = ALL '+'                               
074700              MOVE MID-KDTRSTAT-NY TO 4316-KDTRSTAT                       
074800                                      MOD-KDTRSTAT-GL                     
074900           END-IF                                                         
075000           IF MID-LL-NY NOT = ALL '+'                                     
075100              MOVE MID-LL-NY TO 4316-LL                                   
075200                                MOD-LL-GL                                 
075300           END-IF                                                         
075400           MOVE +1 TO RIX                                                 
075500           PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                       
075600              IF MID-TEDATA-NY (RIX) NOT = ALL '+'                        
075700                 IF MID-TEDATA-NY (RIX) = ALL '-'                         
075800                    MOVE ALL '+' TO W-DATA-RAD (RIX)                      
075900                                 MOD-TEDATA-GL (RIX)                      
076000                                 MOD-TEDATA-NY (RIX)                      
076100                 ELSE                                                     
076200                    MOVE MID-TEDATA-NY (RIX) TO W-DATA-RAD (RIX)          
076300                                             MOD-TEDATA-GL (RIX)          
076400                                             MOD-TEDATA-NY (RIX)          
076500                 END-IF                                                   
076600              END-IF                                                      
076700              MOVE MFS-FORMATETS-ATTR TO MOD-TEDATA-NY-ATTR (RIX)         
076800              ADD +1 TO RIX                                               
076900           END-PERFORM                                                    
077000                                                                          
077100           PERFORM IMS-ISRT-4316                                          
077200                                                                          
077300        END-IF                                                            
077400        MOVE INFO-2 TO MOD-TEMFSINF                                       
077500     ELSE                                                                 
077600         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-GL                        
077700                                   MOD-IDPTYP-GL                          
077800                                   MOD-IDKOLLI-GL                         
077900         MOVE MFS-RENSA-FAELT TO MOD-KDTRSTAT-GL                          
078000                                 MOD-LL-GL                                
078100         MOVE FEL-3-3 TO MOD-TEMFSFEL                                     
078200     END-IF                                                               
078300                                                                          
078400     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
078500                                MOD-IDPRODNR-NY-ATTR                      
078600                                MOD-IDPTYP-NY-ATTR                        
078700                                MOD-IDKOLLI-NY-ATTR                       
078800                                MOD-KDTRSTAT-NY-ATTR                      
078900                                MOD-LL-NY-ATTR                            
079000     .                                                                    
079100     EJECT                                                                
079200 I-VISA-TRANS SECTION.                                                    
079210     MOVE 'STA I-VISA  SEC'              TO PGMPOS                        
079300     SKIP2                                                                
079400     PERFORM IMS-GU-4315                                                  
079500     MOVE NEJ TO SW-ALLT-OK                                               
079600                                                                          
079700     IF NYA-NYCKLAR                                                       
079800         IF MID-KDTRSTAT-IN NOT = ALL '+'                                 
079900             MOVE WS-KDTRSTAT TO W-4316-KDTRSTAT                          
080000             PERFORM IMS-GNP-4316-STATUS                                  
080100         ELSE                                                             
080200             MOVE LOW-VALUE TO W-4316-WDGXKEY-X                           
080300             IF WS-IDPRODNR NUMERIC                                       
080400                 MOVE WS-IDPRODNR TO W-4316-IDPRODNR                      
080500                 IF WS-IDPTYP NUMERIC                                     
080600                     MOVE WS-IDPTYP TO W-4316-IDPTYP                      
080700                     IF WS-IDKOLLI NUMERIC                                
080800                         MOVE WS-IDKOLLI TO W-4316-IDKOLLI                
080900                     END-IF                                               
081000                 END-IF                                                   
081100             END-IF                                                       
081200             PERFORM IMS-GNP-4316-ORDER                                   
081300         END-IF                                                           
081400         MOVE FEL-5 TO MOD-TEMFSFEL                                       
081500     ELSE                                                                 
081600         IF MFS-IDPFK = '8'                                               
081700             PERFORM S01-LAS-FRAM-TILL-RETT-SEGMENT                       
081800             IF SEGMENT-FINNS                                             
081900                 MOVE JA TO SW-ALLT-OK                                    
082000                 IF WS-KDTRSTAT NUMERIC                                   
082100                     MOVE WS-KDTRSTAT TO W-4316-KDTRSTAT                  
082200                     PERFORM IMS-GNP-4316-STATUS                          
082300                 ELSE                                                     
082400                     PERFORM IMS-GNP-4316-ORDER                           
082500                 END-IF                                                   
082600                 IF SEGMENT-SAKNAS                                        
082700                     MOVE FEL-6 TO MOD-TEMFSFEL                           
082800                 END-IF                                                   
082900             ELSE                                                         
083000                 PERFORM IMS-GNP-4316-FIRST                               
083100                 MOVE FEL-5 TO MOD-TEMFSFEL                               
083200             END-IF                                                       
083300         ELSE                                                             
083400             IF MFS-IDPFK = '7'                                           
083500                 PERFORM IMS-GNP-4316-FIRST                               
083600                 MOVE FEL-5 TO MOD-TEMFSFEL                               
083700                 MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                  
083800                                         MOD-IDPTYP-UT                    
083900                                         MOD-IDKOLLI-UT                   
084000                                         MOD-KDTRSTAT-UT                  
084100             ELSE                                                         
084200                 PERFORM S01-LAS-FRAM-TILL-RETT-SEGMENT                   
084300             END-IF                                                       
084400         END-IF                                                           
084500     END-IF                                                               
084600                                                                          
084700     IF SEGMENT-FINNS OR (ALLT-OK AND SEGMENT-SAKNAS)                     
084800         PERFORM S04-FYLL-I-DATA-I-MOD                                    
084900     ELSE                                                                 
085000         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-GL                        
085100                                   MOD-IDPTYP-GL                          
085200                                   MOD-IDKOLLI-GL                         
085300         MOVE MFS-RENSA-FAELT TO MOD-KDTRSTAT-GL                          
085400                                 MOD-LL-GL                                
085500         MOVE FEL-3-4  TO MOD-TEMFSFEL                                    
085600     END-IF                                                               
085700                                                                          
085800     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-NY                              
085900                             MOD-IDPTYP-NY                                
086000                             MOD-IDKOLLI-NY                               
086100                             MOD-KDTRSTAT-NY                              
086200                             MOD-LL-NY                                    
086300     .                                                                    
086400     EJECT                                                                
086500 S01-LAS-FRAM-TILL-RETT-SEGMENT SECTION.                                  
086510     MOVE 'STA S01-LAS SEC'              TO PGMPOS                        
086600     SKIP2                                                                
086700     MOVE MID-IDPRODNR-GL TO W-4316-IDPRODNR                              
086800                             W-IDPRODNR-VISAD                             
086900     MOVE MID-IDPTYP-GL TO W-4316-IDPTYP                                  
087000                           W-IDPTYP-VISAD                                 
087100     MOVE MID-IDKOLLI-GL TO W-4316-IDKOLLI                                
087200                            W-IDKOLLI-VISAD                               
087300     MOVE MID-KDTRSTAT-GL TO W-KDTRSTAT-VISAD                             
087400     MOVE MID-LL-GL TO W-LL-VISAD                                         
087500                                                                          
087600     PERFORM IMS-GHNP-4316-ORDER                                          
087700                                                                          
087800     STRING MID-TEDATA-GL (1) MID-TEDATA-GL (2) MID-TEDATA-GL (3)         
087900                              MID-TEDATA-GL (4) MID-TEDATA-GL (5)         
088000         DELIMITED BY SIZE INTO W-DATA                                    
088100                                                                          
088200     COMPUTE CALL0-LANGD1 = W-LL-VISAD + 21                               
088300     COMPUTE CALL0-LANGD2 = 4316-LL + 21                                  
088400     CALL WVARIF USING W-TRANS-VISAD CALL0-LANGD1                         
088500                       4316-WDGX4316 CALL0-LANGD2 CALL0-KDSVAR            
088600                                                                          
088700     PERFORM UNTIL NOT(SEGMENT-FINNS AND CALL0-KDSVAR-FEL)                
088800         PERFORM IMS-GHNP-4316-ORDER                                      
088900         COMPUTE CALL0-LANGD2 = 4316-LL + 21                              
089000         CALL WVARIF USING W-TRANS-VISAD CALL0-LANGD1                     
089100                           4316-WDGX4316 CALL0-LANGD2 CALL0-KDSVAR        
089200     END-PERFORM                                                          
089300     .                                                                    
089400     EJECT                                                                
089500 S02-ROER-EJ-FAELT SECTION.                                               
089510     MOVE 'STA S02-ROR SEC'              TO PGMPOS                        
089600     SKIP2                                                                
089700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR-GL                            
089800                               MOD-IDPTYP-GL                              
089900                               MOD-IDKOLLI-GL                             
090000                               MOD-KDTRSTAT-GL                            
090100                               MOD-LL-GL                                  
090200                                                                          
090300     MOVE +1 TO RIX                                                       
090400     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
090500                                                                          
090600         MOVE MFS-ROER-EJ-FAELT TO MOD-TEDATA-GL (RIX)                    
090700                                   MOD-TEDATA-NY (RIX)                    
090800         ADD +1 TO RIX                                                    
090900     END-PERFORM                                                          
091000     .                                                                    
091100     EJECT                                                                
091200 S03-RENSA-ALLT SECTION.                                                  
091210     MOVE 'STA S03-REN SEC'              TO PGMPOS                        
091300     SKIP2                                                                
091400     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-UT                              
091500                             MOD-IDPTYP-UT                                
091600                             MOD-IDKOLLI-UT                               
091700                             MOD-KDTRSTAT-UT                              
091800                             MOD-IDPRODNR-NY                              
091900                             MOD-IDPTYP-NY                                
092000                             MOD-IDKOLLI-NY                               
092100                             MOD-KDTRSTAT-NY                              
092200                             MOD-LL-NY                                    
092300     MOVE ZERO TO  MOD-IDPRODNR-GL                                        
092400                   MOD-IDPTYP-GL                                          
092500                   MOD-IDKOLLI-GL                                         
092600                   MOD-KDTRSTAT-GL                                        
092700                   MOD-LL-GL                                              
092800                                                                          
092900     MOVE +1 TO RIX                                                       
093000     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
093100         MOVE MFS-RENSA-FAELT TO MOD-TEDATA-GL (RIX)                      
093200                                 MOD-TEDATA-NY (RIX)                      
093300         ADD +1 TO RIX                                                    
093400     END-PERFORM                                                          
093500     .                                                                    
093600     EJECT                                                                
093700 S04-FYLL-I-DATA-I-MOD SECTION.                                           
093710     MOVE 'STA S04-FYLL SEC'              TO PGMPOS                       
093800     SKIP2                                                                
093900     MOVE 4316-IDPRODNR TO MOD-IDPRODNR-GL                                
094000     MOVE 4316-IDPTYP TO MOD-IDPTYP-GL                                    
094100     MOVE 4316-IDKOLLI TO MOD-IDKOLLI-GL                                  
094200     MOVE 4316-KDTRSTAT TO MOD-KDTRSTAT-GL                                
094300     MOVE 4316-LL TO MOD-LL-GL                                            
094400                                                                          
094500     MOVE +1 TO RIX                                                       
094600     PERFORM UNTIL NOT RIX < MAX-RADER-PLUS-1                             
094700         MOVE W-DATA-RAD (RIX) TO MOD-TEDATA-GL (RIX)                     
094800                                     MOD-TEDATA-NY (RIX)                  
094900         ADD +1 TO RIX                                                    
095000     END-PERFORM                                                          
095100     EJECT                                                                
095200* IMS SECTIONER                                                           
095300     SKIP3                                                                
095400     .                                                                    
095500 IMS-GET-MSG SECTION.                                                     
095600     SKIP2                                                                
095700     MOVE '  QC' TO GODK-STATUSKODER                                      
095800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
095900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096000     PERFORM IMS-STATUS-KONTROLL                                          
096100     SKIP3                                                                
096200     .                                                                    
096300 IMS-INSERT-MSG SECTION.                                                  
096400     SKIP2                                                                
096500     IF ENGLISH-TEXT                                                      
096600         MOVE 'N' TO MFS-KDHUVOMR                                         
096700     END-IF                                                               
096800     MOVE SPACE TO GODK-STATUSKODER                                       
096900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
097000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097100     PERFORM IMS-STATUS-KONTROLL                                          
097200     .                                                                    
097300     EJECT                                                                
097400 IMS-INSERT-ALT-W4T314 SECTION.                                           
097500     SKIP2                                                                
097600     MOVE SPACE TO GODK-STATUSKODER                                       
097700     CALL CBLTDLI USING ISRT W4T314-PCB ALT-IO-AREA                       
097800     MOVE W4T314-STATUS-CODE TO STATUS-WS                                 
097900     PERFORM IMS-STATUS-KONTROLL                                          
098000     SKIP3                                                                
098100     .                                                                    
098200 IMS-INSERT-ALT-W4T315 SECTION.                                           
098300     SKIP2                                                                
098400     MOVE SPACE TO GODK-STATUSKODER                                       
098500     CALL CBLTDLI USING ISRT W4T315-PCB ALT-IO-AREA                       
098600     MOVE W4T315-STATUS-CODE TO STATUS-WS                                 
098700     PERFORM IMS-STATUS-KONTROLL                                          
098800     SKIP3                                                                
098900     .                                                                    
099000 IMS-INSERT-ALT-W4T317 SECTION.                                           
099100     SKIP2                                                                
099200     MOVE SPACE TO GODK-STATUSKODER                                       
099300     CALL CBLTDLI USING ISRT W4T317-PCB ALT-IO-AREA                       
099400     MOVE W4T317-STATUS-CODE TO STATUS-WS                                 
099500     PERFORM IMS-STATUS-KONTROLL                                          
099600     SKIP3                                                                
099700     .                                                                    
099800 IMS-INSERT-ALT-W4T398U SECTION.                                          
099900     SKIP2                                                                
100000     MOVE SPACE TO GODK-STATUSKODER                                       
100100     CALL CBLTDLI USING ISRT W4T398U-PCB ALT-IO-AREA                      
100200     MOVE W4T398U-STATUS-CODE TO STATUS-WS                                
100300     PERFORM IMS-STATUS-KONTROLL                                          
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GU-4315 SECTION.                                                     
100700     SKIP2                                                                
100800     STRING 'WLXXDL01(WDGXKEY  =' W-4315-WDGXKEY-X ')'                    
100900             DELIMITED BY SIZE INTO SSA1                                  
101000     MOVE '  ' TO GODK-STATUSKODER                                        
101100     CALL CBLTDLI USING GU 4315-PCB DLI-IO-AREA SSA1                      
101200     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUS-KONTROLL                                          
101400     SKIP3                                                                
101500     .                                                                    
101600 IMS-GHNP-4316-ORDER SECTION.                                             
101700     SKIP2                                                                
101800     STRING 'WLXXDL11(WDGXKEY  =' W-4316-WDGXKEY-X ')'                    
101900             DELIMITED BY SIZE INTO SSA1                                  
102000     MOVE '  GE' TO GODK-STATUSKODER                                      
102100     CALL CBLTDLI USING GHNP 4315-PCB DLI-IO-AREA SSA1                    
102200     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
102300     PERFORM IMS-STATUS-KONTROLL                                          
102400     SKIP3                                                                
102500     .                                                                    
102600 IMS-GHNP-4316-STATUS-FIRST SECTION.                                      
102700     SKIP2                                                                
102800     STRING 'WLXXDL11*F(KDTRSTAT =' W-4316-KDTRSTAT-X ')'                 
102900             DELIMITED BY SIZE INTO SSA1                                  
103000     MOVE '  GE' TO GODK-STATUSKODER                                      
103100     CALL CBLTDLI USING GHNP 4315-PCB DLI-IO-AREA SSA1                    
103200     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
103300     PERFORM IMS-STATUS-KONTROLL                                          
103400     .                                                                    
103500     EJECT                                                                
103600 IMS-GNP-4316-FIRST SECTION.                                              
103700     SKIP2                                                                
103800     MOVE 'WLXXDL11*F' TO SSA1                                            
103900     MOVE '  GE' TO GODK-STATUSKODER                                      
104000     CALL CBLTDLI USING GNP 4315-PCB DLI-IO-AREA SSA1                     
104100     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
104200     PERFORM IMS-STATUS-KONTROLL                                          
104300     SKIP2                                                                
104400     .                                                                    
104500 IMS-GNP-4316-ORDER SECTION.                                              
104600     SKIP2                                                                
104700     STRING 'WLXXDL11(WDGXKEY >=' W-4316-WDGXKEY-X ')'                    
104800             DELIMITED BY SIZE INTO SSA1                                  
104900     MOVE '  GE' TO GODK-STATUSKODER                                      
105000     CALL CBLTDLI USING GNP 4315-PCB DLI-IO-AREA SSA1                     
105100     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
105200     PERFORM IMS-STATUS-KONTROLL                                          
105300     SKIP2                                                                
105400     .                                                                    
105500 IMS-GNP-4316-STATUS SECTION.                                             
105600     SKIP2                                                                
105700     STRING 'WLXXDL11(KDTRSTAT =' W-4316-KDTRSTAT-X ')'                   
105800             DELIMITED BY SIZE INTO SSA1                                  
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GNP 4315-PCB DLI-IO-AREA SSA1                     
106100     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUS-KONTROLL                                          
106300     .                                                                    
106400     EJECT                                                                
106500 IMS-ISRT-4316 SECTION.                                                   
106600     SKIP2                                                                
106700     STRING 'WLXXDL01(WDGXKEY  =' W-4315-WDGXKEY-X ')'                    
106800             DELIMITED BY SIZE INTO SSA1                                  
106900     MOVE 'WLXXDL11 ' TO SSA2                                             
107000     MOVE '  ' TO GODK-STATUSKODER                                        
107100     CALL CBLTDLI USING ISRT 4315-PCB DLI-IO-AREA SSA1 SSA2               
107200     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUS-KONTROLL                                          
107400     SKIP3                                                                
107500     .                                                                    
107600 IMS-REPLACE SECTION.                                                     
107700     SKIP2                                                                
107800     MOVE '  ' TO GODK-STATUSKODER                                        
107900     CALL CBLTDLI USING REPL 4315-PCB DLI-IO-AREA                         
108000     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
108100     PERFORM IMS-STATUS-KONTROLL                                          
108200     SKIP3                                                                
108300     .                                                                    
108400 IMS-DELETE SECTION.                                                      
108500     SKIP2                                                                
108600     MOVE '  ' TO GODK-STATUSKODER                                        
108700     CALL CBLTDLI USING DLET 4315-PCB DLI-IO-AREA                         
108800     MOVE 4315-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUS-KONTROLL                                          
109000     .                                                                    
109100     EJECT                                                                
109200 IMS-STATUS-KONTROLL SECTION.                                             
109300     SET STATUS-IX TO 1                                                   
109400     SEARCH GODK-STATUS                                                   
109410       AT END                                                             
109420         CALL FELLOG                                                      
109500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
109600     END-SEARCH                                                           
109700     CONTINUE                                                             
109800     .                                                                    
