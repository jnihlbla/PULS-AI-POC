000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4033400.                                                
000400 AUTHOR.         SUSANNE ENEGARD.                                         
000500     DATE-WRITTEN.   MAJ  -86.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        PROGRAMMETS FRÅGEDEL LISTAR ETT/FLERA KOLLI  INOM                
001000*                    ETT UNIKT DISTRIKT/KUNDNR/ORDERNR.                   
001100*        PROGRAMMETS UPPDATERINGSDEL GODKÄNNER BARA                       
001200*                    Ä = ÄNDRING                                          
001300*                        MASKINELL FLYTTNING GÖRS OM MAN INTE             
001400*                        FYLLER I NYA VÄRDEN ANNARS GÖRS DEN              
001500*                        IFYLLDA FLYTTNINGEN.                             
001600*                        ENDAST 1 ÄNDRING GÖRS VARJE GÅNG.                
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T334                                              
001900*        MID:         W4I33401                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O33401                                            
002300*    SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP3                                                                
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900*    -- CHECKED BY WY2000                                                 
003000     SKIP3                                                                
003100 77   PROGRAM-NAMN           VALUE 'W4033400'                             
003200                                 PIC X(8).                                
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  SPRAK-IX                    PIC S9(4)   VALUE +0   COMP SYNC.        
003600 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
003700 77  TAB-IX                      PIC S9(4)   VALUE +0   COMP SYNC.        
003800 77  TAB-MAX                     PIC S9(4)   VALUE +100 COMP SYNC.        
003900 77  MAX-IX-PLUS-1               PIC S9(4)   VALUE +15  COMP SYNC.        
004000 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +819 COMP SYNC.        
004100 77  INMATN-RAEKNARE             PIC S9(1)   VALUE +0   COMP-3.           
004200 77  FOERSTA-UPDATE              PIC X       VALUE 'J'.                   
004300 77  IDKOLLI-HITTAT              PIC X       VALUE 'N'.                   
004400 77  WS-SLINGA-KLAR              PIC X.                                   
004500     88  SLINGA-KLAR                         VALUE 'J'.                   
004600 77  INDATA-FEL                  PIC X.                                   
004700     88  INDATA-OK                           VALUE 'N'.                   
004800 77  KEY-SW                      PIC X.                                   
004900     88  KEY-OK                              VALUE 'J'.                   
005000 77  WS-IDTRANS                  PIC X(4).                                
005100     88  WS-GODKAEND-BILD                    VALUE '4331' '4332'          
005200                                     '4333' '4334' '4335' '4336'          
005300                                                      '4338'.             
005400     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005900     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16)  VALUE 'W005INIT '.            
006200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006300*01 -COPY WMSGINIT                                                        
006400     EJECT                                                                
006500*                                                                         
006600 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
006700*01 -COPY W403PLAT                                                        
006800     EJECT                                                                
006900*                                                                         
007000 01  IDDISTR-WS                  PIC X(4)    VALUE SPACE.                 
007100 01  FILLER     REDEFINES IDDISTR-WS.                                     
007200     03  KEY-IDDISTR             PIC 9(4).                                
007300 01  IDKUNDNR-WS                 PIC X(6)    VALUE SPACE.                 
007400 01  FILLER     REDEFINES IDKUNDNR-WS.                                    
007500     03  KEY-IDKUNDNR            PIC 9(6).                                
007600 01  IDORDNR-WS                  PIC X(5)    VALUE SPACE.                 
007700 01  FILLER     REDEFINES IDORDNR-WS.                                     
007800     03  KEY-IDORDNR             PIC 9(5).                                
007900 01  IDKOLLI-WS                  PIC X(5)    VALUE SPACE.                 
008000 01  FILLER     REDEFINES IDKOLLI-WS.                                     
008100     03  KEY-IDKOLLI             PIC 9(5).                                
008200 01  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
008300                                                                          
008400 01  WS-IDKUNDRF.                                                         
008500     03  WS-IDORDNR              PIC 9(5).                                
008600     03  FILLER                  PIC X(5)    VALUE SPACE.                 
008700     EJECT                                                                
008800 01  SPAR-AREA.                                                           
008900     03  SPAR-KOLLI-IDTRPTNR     PIC S9(3)   COMP-3.                      
009000     03  SPAR-KOLLI-DARFS        PIC 9(12).                               
009100     03  SPAR-KOLLI-ADCLGEO.                                              
009200         05  SPAR-KOLLI-WS-IDDC  PIC X(2).                                
009300         05  SPAR-KOLLI-ADFLGEO  PIC X(3).                                
009400     03  SPAR-KOLLI-ADFLOMR      PIC S9(3)   COMP-3.                      
009500     03  SPAR-KOLLI-ADRUTNIV     PIC S9(3)   COMP-3.                      
009600     03  SPAR-KOLLI-DIDMODUL     PIC S9(3)   COMP-3.                      
009700     03  SPAR-KOLLI-DIHMODUL     PIC S9(3)   COMP-3.                      
009800     03  SPAR-KOLLI-ADVMODUL     PIC S9(3)   COMP-3.                      
009900     03  SPAR-KOLLI-ADHMODUL     PIC S9(3)   COMP-3.                      
010000     03  SPAR-EMB-KDKOLLID       PIC X(1).                                
010100     03  SPAR-VORD-KDFRAKT       PIC S9(3)   COMP-3.                      
010200     03  SPAR-VORD-KDORDKL       PIC S9(1)   COMP-3.                      
010300     03  SPAR-KVORDRAD-LEVPL     PIC S9(5)   COMP-3.                      
010400     EJECT                                                                
010500 01  FEL-MEDDELANDEN.                                                     
010600   03  FEL1.                                                              
010700     05 FILLER                   PIC X(40)                                
010800          VALUE '749  FEL NYCKEL               '.                         
010900     05 FILLER                   PIC X(40)                                
011000          VALUE '749  WRONG KEY                   '.                      
011100   03  FILLER REDEFINES FEL1.                                             
011200     05  FEL-1                   PIC X(40)   OCCURS 2.                    
011300                                                                          
011400   03  FEL2.                                                              
011500     05 FILLER                   PIC X(40)                                
011600          VALUE '748  UPPLYSTA FÄLT FEL        '.                         
011700     05 FILLER                   PIC X(40)                                
011800          VALUE '748  HIGH LIGHTED FIELD FALSE '.                         
011900   03  FILLER REDEFINES FEL2.                                             
012000     05  FEL-2                   PIC X(40)   OCCURS 2.                    
012100                                                                          
012200   03  FEL3.                                                              
012300     05 FILLER                   PIC X(40)                                
012400          VALUE '701  ORDERN SAKNAS            '.                         
012500     05 FILLER                   PIC X(40)                                
012600          VALUE '701  ORDER MISSING            '.                         
012700   03  FILLER REDEFINES FEL3.                                             
012800     05  FEL-3                   PIC X(40)   OCCURS 2.                    
012900                                                                          
013000   03  FEL4.                                                              
013100     05 FILLER                   PIC X(40)                                
013200          VALUE '764  PLATS FINNS EJ           '.                         
013300     05 FILLER                   PIC X(40)                                
013400          VALUE '764  LOCATION MISSING         '.                         
013500   03  FILLER REDEFINES FEL4.                                             
013600     05  FEL-4                   PIC X(40)   OCCURS 2.                    
013700                                                                          
013800   03  FEL5.                                                              
013900     05 FILLER                   PIC X(40)                                
014000          VALUE '758  KOLLI SAKNAS             '.                         
014100     05 FILLER                   PIC X(40)                                
014200          VALUE '758  CASE MISSING             '.                         
014300   03  FILLER REDEFINES FEL5.                                             
014400     05  FEL-5                   PIC X(40)   OCCURS 2.                    
014500                                                                          
014600   03  FEL6.                                                              
014700     05 FILLER                   PIC X(40)                                
014800          VALUE '079  FEL STATUS               '.                         
014900     05 FILLER                   PIC X(40)                                
015000          VALUE '079  WRONG STATUS             '.                         
015100   03  FILLER REDEFINES FEL6.                                             
015200     05  FEL-6                   PIC X(40)   OCCURS 2.                    
015300*                                                                         
015400   03    FEL9.                                                            
015500     05  FILLER                  PIC X(40)   VALUE                        
015600        '    KOLLIT TILLHÖR ETT SAMLINGSKOLLI    '.                       
015700     05  FILLER                  PIC X(40)   VALUE                        
015800        '    CASE IS PART OF CONSOLIDATED CASE   '.                       
015900   03    FILLER  REDEFINES  FEL9.                                         
016000     05  FEL-9       OCCURS 2    PIC X(40).                               
016100*                                                                         
016200     SKIP2                                                                
016300 01  MEDDELANDEN.                                                         
016400   03  MED1.                                                              
016500     05 FILLER                   PIC X(40)                                
016600          VALUE 'UPPDATERING UTFÖRD       '.                              
016700     05 FILLER                   PIC X(40)                                
016800          VALUE 'UPDATING DONE            '.                              
016900   03  FILLER REDEFINES MED1.                                             
017000     05  MED-1                   PIC X(40)   OCCURS 2.                    
017100                                                                          
017200   03  MED2.                                                              
017300     05 FILLER                   PIC X(40)                                
017400          VALUE 'FLER KOLLI FINNS         '.                              
017500     05 FILLER                   PIC X(40)                                
017600          VALUE 'MORE CASSES             '.                               
017700   03  FILLER REDEFINES MED2.                                             
017800     05  MED-2                   PIC X(40)   OCCURS 2.                    
017900     EJECT                                                                
018000 01  NYCKLAR-TILL-DLI.                                                    
018100                                                                          
018200     03  W-KDKOLLI-X.                                                     
018300         05  W-KDKOLLI             PIC X(8).                              
018400     03  W-WDE4A1KEY-X.                                                   
018500         05  W-4A1-IDDISTR         PIC S9(5)   COMP-3.                    
018600         05  W-4A1-IDKUNDNR        PIC S9(7)   COMP-3.                    
018700         05  W-4A1-IDKUNDRF.                                              
018800             07  W-4A1-IDORDNR     PIC X(5).                              
018900             07  FILLER            PIC X(5)    VALUE SPACE.               
019000     03  W-WDE401KEY-X.                                                   
019100         05  W-401-IDDISTR         PIC S9(5)   COMP-3.                    
019200         05  W-401-IDKUNDNR        PIC S9(7)   COMP-3.                    
019300         05  W-401-IDKUNDRF.                                              
019400             07  W-401-IDORDNR     PIC X(5).                              
019500             07  FILLER            PIC X(5)    VALUE SPACE.               
019600         05  W-401-IDPRODNR        PIC S9(7)   COMP-3.                    
019700         05  W-401-IDPLKLST        PIC S9(3)   COMP-3.                    
019800     03  W-IDPRODNR-X.                                                    
019900         05  W-IDPRODNR            PIC S9(7)   COMP-3.                    
020000     03  W-IDKOLLI-X.                                                     
020100         05  W-IDKOLLI             PIC S9(5)   COMP-3.                    
020200     03  W-IDKOLLI-FLER-X.                                                
020300         05  W-IDKOLLI-FLER        PIC S9(5)   COMP-3.                    
020400     EJECT                                                                
020500******************************************************************        
020600*                                                                         
020700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
021000     SKIP3                                                                
021100*01  MID -COPY W4I33401                                                   
021200     EJECT                                                                
021300*01  -COPY WMSGAREA                                                       
021400     EJECT                                                                
021500*  03  MOD -COPY W4O33401           -RED MSG-AREA.                        
021600     EJECT                                                                
021700*01  -COPY WMFSAREA                                                       
021800     EJECT                                                                
021900******************************************************************        
022000*                                                                         
022100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022200*                                                                         
022300 01  IMS-WS.                                                              
022400   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
022500     SKIP3                                                                
022600*                        **** STATUS-KOD FRÅN IMS                         
022700   03  STATUS-KUNDORDER-SEK-WS   PIC XX.                                  
022800     88  KUNDORDER-SEK-FINNS                 VALUE '  '.                  
022900     88  KUNDORDER-SEK-SAKNAS                VALUE 'GE'.                  
023000   03  STATUS-WS                 PIC XX.                                  
023100     88  SEGMENT-FINNS                       VALUE '  '.                  
023200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023300     SKIP3                                                                
023400   03  GODK-STATUSKODER.                                                  
023500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023600     SKIP3                                                                
023700 01    SSA1                      PIC X(192).                              
023800 01    SSA2                      PIC X(64).                               
023900 01    SSA3                      PIC X(64).                               
024000     EJECT                                                                
024100*                            IMS FUNKTIONSKODER                           
024200*01    -COPY W0003                                                        
024300     EJECT                                                                
024400* - - - - - - - - - - - - - - - - - - - - DLI-IO-AREA                     
024500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
024600 01  DLI-IO-AREA.                                                         
024700     03  IO-AREA-1               PIC X(400) VALUE SPACE.                  
024800     SKIP3                                                                
024900*    03  WDE401    -COPY WDE401       -RED IO-AREA-1.                     
025000     EJECT                                                                
025100*    03  WDE601    -COPY WDE601       -RED IO-AREA-1.                     
025200     EJECT                                                                
025300*    03  WDE611    -COPY WDE611       -RED IO-AREA-1.                     
025400     EJECT                                                                
025500*    03  WLEMBB01  -COPY WDK501       -RED IO-AREA-1.                     
025600     EJECT                                                                
025700 LINKAGE SECTION.                                                         
025800*01  -COPY W0009     -PRE MSG-                                            
025900     EJECT                                                                
026000*01  -COPY W0008     -PRE USEA-                                           
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0009     -PRE WDE4-                                           
026400     EJECT                                                                
026500*01  -COPY W0009     -PRE WDE4A-                                          
026600     EJECT                                                                
026700*01  -COPY W0009     -PRE WDE6-                                           
026800     EJECT                                                                
026900*01  -COPY W0009     -PRE WDE62-                                          
027000     EJECT                                                                
027100*01  -COPY W0009     -PRE EMB-                                            
027200     EJECT                                                                
027300*01  -COPY W0008     -PRE PLATS-DM-                                       
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600*01  -COPY W0008     -PRE PLATS-DN-                                       
027700     05  FILLER                  PIC X.                                   
027800     EJECT                                                                
027900*01  -COPY W0008     -PRE PLATS-DP-                                       
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200*01  -COPY W0008     -PRE PLATS-DO-                                       
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028500*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
028600     05  FILLER                  PIC X.                                   
028700     EJECT                                                                
028800*01  -COPY W0008     -PRE PLATS-GMTC-                                     
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008     -PRE PLATS-WDB6-                                     
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDE4-PCB                       
029500                          WDE4A-PCB WDE6-PCB WDE62-PCB EMB-PCB            
029600                          PLATS-DM-PCB PLATS-DN-PCB                       
029700                          PLATS-DP-PCB PLATS-DO-PCB                       
029800                          PLATS-WDE6C-PCB PLATS-GMTC-PCB                  
029900                          PLATS-WDB6-PCB.                                 
030000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE4-PCB                      
030100                          WDE4A-PCB WDE6-PCB WDE62-PCB EMB-PCB            
030200                          PLATS-DM-PCB PLATS-DN-PCB                       
030300                          PLATS-DP-PCB PLATS-DO-PCB                       
030400                          PLATS-WDE6C-PCB PLATS-GMTC-PCB                  
030500                          PLATS-WDB6-PCB.                                 
030600                                                                          
030700     PERFORM IMS-GET-MSG                                                  
030800     IF SEGMENT-FINNS                                                     
030900        PERFORM A-INIT                                                    
031000        IF WS-GODKAEND-BILD                                               
031100                                                                          
031200          IF KEY-IDDISTR NUMERIC                                          
031300             AND KEY-IDKUNDNR NUMERIC                                     
031400             AND KEY-IDORDNR NUMERIC                                      
031500             AND KEY-IDKOLLI NUMERIC                                      
031600             AND KEY-OK                                                   
031700             IF MFS-UPDATE  AND                                           
031800                (MID-IDDISTR-IN = ALL '+' AND                             
031900                MID-IDKUNDNR-IN = ALL '+' AND                             
032000                MID-IDORDNR-IN = ALL '+' AND                              
032100                MID-IDKOLLI-IN = ALL '+')                                 
032200                PERFORM B-KOLLA-INDATA                                    
032300                IF INDATA-OK                                              
032400                   PERFORM C-UPPDATERA                                    
032500                   IF INDATA-OK                                           
032600                      PERFORM MFS-RENSA-BILD                              
032700                   ELSE                                                   
032800                      PERFORM MFS-ROER-EJ-BILD                            
032900                   END-IF                                                 
033000                ELSE                                                      
033100                   MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                  
033200                   MOVE JA TO INDATA-FEL                                  
033300                   PERFORM MFS-ROER-EJ-BILD                               
033400                END-IF                                                    
033500             END-IF                                                       
033600             IF INDATA-OK                                                 
033700                PERFORM D-FRAGA                                           
033800             END-IF                                                       
033900          ELSE                                                            
034000             MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                        
034100             PERFORM MFS-RENSA-BILD                                       
034200             PERFORM MFS-GRUND-FORMAT                                     
034300          END-IF                                                          
034400        ELSE                                                              
034500          PERFORM E-RENSA-NYCKLAR                                         
034600        END-IF                                                            
034700                                                                          
034800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
034900        PERFORM IMS-INSERT-MSG                                            
035000     END-IF                                                               
035100                                                                          
035200     MOVE ZERO TO RETURN-CODE                                             
035300     GOBACK                                                               
035400     .                                                                    
035500     EJECT                                                                
035600 A-INIT SECTION.                                                          
035700                                                                          
035800     IF MSG-DUBBLA-TRANSKODER                                             
035900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33401                 
036000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
036300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
036400     ELSE                                                                 
036500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33401                  
036600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
036700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036800       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
036900     END-IF                                                               
037000     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
037100                                                                          
037200     MOVE LOW-VALUE TO MSG-AREA                                           
037300     MOVE 'W4O334N1' TO MFS-IDMOD                                         
037400     MOVE '4334' TO MOD-IDTRANS                                           
037500                                                                          
037600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037700     MOVE '001'             TO MSGI-KDCALL                                
037800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037900     MOVE '4334'            TO MSGI-IDTRANS                               
038000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
038100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038200                                                                          
038300     IF MSGI-IDLAND-SPR = 'GB'                                            
038400       MOVE +2 TO SPRAK-IX                                                
038500     ELSE                                                                 
038600       MOVE +1 TO SPRAK-IX                                                
038700     END-IF                                                               
038800                                                                          
038900     IF MFS-IDTRANS NOT = '4334'                                          
039000       MOVE SPACE TO MFS-KDTRTYP                                          
039100       MOVE '7' TO MFS-IDPFK                                              
039200     END-IF                                                               
039300                                                                          
039400     MOVE JA                   TO KEY-SW                                  
039500                                                                          
039600     IF MID-IDDISTR-IN = ALL '+'                                          
039700        MOVE MID-IDDISTR-UT TO IDDISTR-WS                                 
039800        INSPECT IDDISTR-WS REPLACING LEADING SPACE BY ZERO                
039900     ELSE                                                                 
040000        MOVE MID-IDDISTR-IN TO IDDISTR-WS                                 
040100     END-IF                                                               
040200                                                                          
040300     IF MID-IDKUNDNR-IN = ALL '+'                                         
040400        MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                               
040500        INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO               
040600     ELSE                                                                 
040700        MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                               
040800     END-IF                                                               
040900                                                                          
041000     IF MID-IDORDNR-IN = ALL '+'                                          
041100        MOVE MID-IDORDNR-UT TO IDORDNR-WS                                 
041200        INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO                
041300     ELSE                                                                 
041400        MOVE MID-IDORDNR-IN TO IDORDNR-WS                                 
041500     END-IF                                                               
041600                                                                          
041700     IF MID-IDKOLLI-IN = ALL '+'                                          
041800        MOVE MID-IDKOLLI-UT TO IDKOLLI-WS                                 
041900        INSPECT IDKOLLI-WS REPLACING LEADING SPACE BY ZERO                
042000     ELSE                                                                 
042100        MOVE MID-IDKOLLI-IN TO IDKOLLI-WS                                 
042200     END-IF                                                               
042300                                                                          
042400     MOVE MSGI-IDDC                       TO WS-IDDC                      
042500                                                                          
042600     IF WS-IDDC IS > SPACE                                                
042700       CONTINUE                                                           
042800     ELSE                                                                 
042900       MOVE NEJ                TO KEY-SW                                  
043000     END-IF                                                               
043100                                                                          
043200     MOVE IDDISTR-WS      TO MOD-IDDISTR-UT                               
043300     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
043400                                                                          
043500     MOVE IDKUNDNR-WS     TO MOD-IDKUNDNR-UT                              
043600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
043700                                                                          
043800     MOVE IDORDNR-WS      TO MOD-IDORDNR-UT                               
043900     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
044000                                                                          
044100     MOVE IDKOLLI-WS      TO MOD-IDKOLLI-UT                               
044200     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
044300                                                                          
044400     MOVE WS-IDDC         TO MOD-IDDC-UT                                  
044500                                                                          
044600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
044700                             MOD-IDKUNDNR-IN                              
044800                             MOD-IDORDNR-IN                               
044900                             MOD-IDKOLLI-IN                               
045000                             MOD-TEMFSFEL                                 
045100                             MOD-TEMFSINF                                 
045200                                                                          
045300     MOVE NEJ TO INDATA-FEL                                               
045400     .                                                                    
045500     EJECT                                                                
045600 B-KOLLA-INDATA SECTION.                                                  
045700                                                                          
045800     MOVE +1 TO INDX                                                      
045900                                                                          
046000     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1                               
046100       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
046200          IF MID-KDCMD (INDX) = 'Ä' OR 'A' OR 'C' OR ' '                  
046300             MOVE MFS-ALFA-FAELT-RAETT TO                                 
046400                  MOD-KDCMD-ATTR (INDX)                                   
046500          ELSE                                                            
046600             MOVE MFS-ALFA-FAELT-FEL TO                                   
046700                  MOD-KDCMD-ATTR (INDX)                                   
046800             MOVE JA TO INDATA-FEL                                        
046900          END-IF                                                          
047000       END-IF                                                             
047100                                                                          
047200       MOVE ZERO TO INMATN-RAEKNARE                                       
047300                                                                          
047400       IF MID-ADFLGEO-NY(INDX) NOT = ALL '+'                              
047500          MOVE MFS-NUM-FAELT-RAETT TO                                     
047600               MOD-ADFLGEO-NY-ATTR(INDX)                                  
047700          ADD +1 TO INMATN-RAEKNARE                                       
047800       END-IF                                                             
047900                                                                          
048000       IF MID-ADFLOMR-NY(INDX) NOT = ALL '+'                              
048100          IF MID-ADFLOMR-NY(INDX) NUMERIC                                 
048200             IF MID-ADFLOMR-NY(INDX) < 500 OR > 899                       
048300                MOVE MFS-NUM-FAELT-RAETT TO                               
048400                     MOD-ADFLOMR-NY-ATTR(INDX)                            
048500                ADD +1 TO INMATN-RAEKNARE                                 
048600             ELSE                                                         
048700                MOVE MFS-NUM-FAELT-FEL TO                                 
048800                     MOD-ADFLOMR-NY-ATTR(INDX)                            
048900                MOVE JA TO INDATA-FEL                                     
049000             END-IF                                                       
049100          ELSE                                                            
049200             MOVE MFS-NUM-FAELT-FEL TO                                    
049300                  MOD-ADFLOMR-NY-ATTR(INDX)                               
049400             MOVE JA TO INDATA-FEL                                        
049500          END-IF                                                          
049600       END-IF                                                             
049700       IF MID-ADRUTNIV-NY(INDX) NOT = ALL '+'                             
049800          IF MID-ADRUTNIV-NY(INDX) NUMERIC                                
049900             MOVE MFS-NUM-FAELT-RAETT TO                                  
050000                  MOD-ADRUTNIV-NY-ATTR(INDX)                              
050100             ADD +1 TO INMATN-RAEKNARE                                    
050200          ELSE                                                            
050300             MOVE MFS-NUM-FAELT-FEL TO                                    
050400                  MOD-ADRUTNIV-NY-ATTR(INDX)                              
050500             MOVE JA TO INDATA-FEL                                        
050600          END-IF                                                          
050700       END-IF                                                             
050800       IF INDATA-OK                                                       
050900         IF INMATN-RAEKNARE = +0 OR +3                                    
051000         CONTINUE                                                         
051100         ELSE                                                             
051200           MOVE MFS-NUM-FAELT-FEL TO                                      
051300                MOD-ADFLGEO-NY-ATTR(INDX)                                 
051400                MOD-ADFLOMR-NY-ATTR(INDX)                                 
051500                MOD-ADRUTNIV-NY-ATTR(INDX)                                
051600           MOVE JA TO INDATA-FEL                                          
051700         END-IF                                                           
051800       END-IF                                                             
051900       ADD +1 TO INDX                                                     
052000     END-PERFORM                                                          
052100     .                                                                    
052200     EJECT                                                                
052300 C-UPPDATERA SECTION.                                                     
052400                                                                          
052500     MOVE +1 TO INDX                                                      
052600     MOVE JA TO FOERSTA-UPDATE                                            
052700                                                                          
052800     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1  OR                           
052900             FOERSTA-UPDATE NOT = JA                                      
053000        IF MID-KDCMD(INDX) = ALL '+'                                      
053100        CONTINUE                                                          
053200        ELSE                                                              
053300           IF MID-KDCMD-INGENTING(INDX)                                   
053400           CONTINUE                                                       
053500           ELSE                                                           
053600              PERFORM CA-AENDRING                                         
053700              MOVE NEJ TO FOERSTA-UPDATE                                  
053800           END-IF                                                         
053900        END-IF                                                            
054000        ADD +1 TO INDX                                                    
054100     END-PERFORM                                                          
054200     .                                                                    
054300     EJECT                                                                
054400 CA-AENDRING SECTION.                                                     
054500                                                                          
054600     MOVE MID-KDKOLLI(INDX) TO W-KDKOLLI                                  
054700     PERFORM IMS-GET-EMB                                                  
054800     IF SEGMENT-SAKNAS                                                    
054900        MOVE 'L' TO SPAR-EMB-KDKOLLID                                     
055000     ELSE                                                                 
055100        MOVE EMB-KDKOLLID TO SPAR-EMB-KDKOLLID                            
055200     END-IF                                                               
055300                                                                          
055400     MOVE 'N'          TO WS-SLINGA-KLAR                                  
055500     MOVE KEY-IDDISTR  TO W-4A1-IDDISTR                                   
055600     MOVE KEY-IDKUNDNR TO W-4A1-IDKUNDNR                                  
055700     MOVE KEY-IDORDNR  TO WS-IDORDNR                                      
055800     MOVE WS-IDKUNDRF  TO W-4A1-IDKUNDRF                                  
055900     PERFORM IMS-GU-KUNDORDER-SEK                                         
056000*                                                                         
056100     IF KUNDORDER-SEK-FINNS                                               
056200        PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                
056300        MOVE KORD-IDDISTR         TO W-401-IDDISTR                        
056400        MOVE KORD-IDKUNDNR        TO W-401-IDKUNDNR                       
056500        MOVE KORD-IDORDNR5        TO W-401-IDORDNR                        
056600        MOVE KORD-IDPRODNR        TO W-401-IDPRODNR                       
056700        MOVE KORD-IDPLKLST        TO W-401-IDPLKLST                       
056800        PERFORM IMS-GU-KUNDORDER                                          
056900        MOVE KORD-KVORDRAD-LEVPL   TO SPAR-KVORDRAD-LEVPL                 
057000*                                                                         
057100        MOVE KORD-IDPRODNR TO W-IDPRODNR                                  
057200        PERFORM IMS-GET-VORD                                              
057300        IF SEGMENT-SAKNAS                                                 
057400           MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                          
057500           MOVE JA TO INDATA-FEL                                          
057600        ELSE                                                              
057700           IF            (VORD-IDDC = WS-IDDC    AND                      
057800                          SPAR-KVORDRAD-LEVPL   = ZERO)                   
057900              IF VORD-IDDC = WS-IDDC                                      
058000                 MOVE VORD-IDPRODNR TO W-IDPRODNR                         
058100                 MOVE VORD-KDFRAKT TO SPAR-VORD-KDFRAKT                   
058200                 MOVE VORD-KDORDKL TO SPAR-VORD-KDORDKL                   
058300                 MOVE 'J'          TO WS-SLINGA-KLAR                      
058400                 PERFORM CAA-LAES-UPD-KOLLI                               
058500              END-IF                                                      
058600           END-IF                                                         
058700        END-IF                                                            
058800        PERFORM IMS-GN-KUNDORDER-SEK                                      
058900        END-PERFORM                                                       
059000     ELSE                                                                 
059100        MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                             
059200        MOVE JA TO INDATA-FEL                                             
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 CAA-LAES-UPD-KOLLI SECTION.                                              
059700                                                                          
059800     MOVE MID-IDKOLLI(INDX) TO W-IDKOLLI                                  
059900     PERFORM IMS-GHU-WDE601-11                                            
060000     IF SEGMENT-SAKNAS                                                    
060100       MOVE FEL-5 (SPRAK-IX) TO MOD-TEMFSFEL                              
060200       MOVE JA TO INDATA-FEL                                              
060300     ELSE                                                                 
060400       IF KOLLI-KDKOLSTA NOT = 1                                          
060500         MOVE FEL-6 (SPRAK-IX) TO MOD-TEMFSFEL                            
060600         MOVE JA TO INDATA-FEL                                            
060700       ELSE                                                               
060800         IF KOLLI-IDKOLLI-SAMP = ZERO                                     
060900           MOVE KOLLI-IDTRPTNR TO SPAR-KOLLI-IDTRPTNR                     
061000           MOVE KOLLI-DARFS  TO SPAR-KOLLI-DARFS                          
061100           MOVE KOLLI-ADCLGEO TO SPAR-KOLLI-ADCLGEO                       
061200           MOVE KOLLI-ADFLOMR TO SPAR-KOLLI-ADFLOMR                       
061300           MOVE KOLLI-ADRUTNIV TO SPAR-KOLLI-ADRUTNIV                     
061400           MOVE KOLLI-DIHMODUL TO SPAR-KOLLI-DIHMODUL                     
061500           MOVE KOLLI-DIDMODUL TO SPAR-KOLLI-DIDMODUL                     
061600           MOVE KOLLI-ADVMODUL TO SPAR-KOLLI-ADVMODUL                     
061700           MOVE KOLLI-ADHMODUL TO SPAR-KOLLI-ADHMODUL                     
061800                                                                          
061900           IF KOLLI-IDKOLLI-FLER < ZERO                                   
062000             MOVE KOLLI-IDKOLLI-FLER TO W-IDKOLLI                         
062100                                        W-IDKOLLI-FLER                    
062200             PERFORM IMS-GET-KOLLI-FLER-FIRST                             
062300           END-IF                                                         
062400           IF MID-ADFLGEO-NY(INDX) = ALL '+' OR                           
062500              MID-ADFLOMR-NY(INDX) = ALL '+' OR                           
062600              MID-ADRUTNIV-NY(INDX) = ALL '+'                             
062700              PERFORM CAAA-MASKINELL-PLATSSOEK                            
062800           ELSE                                                           
062900             PERFORM CAAB-MANUELL-PLATSSOEK                               
063000           END-IF                                                         
063100         ELSE                                                             
063200*---------------------------------------------INGÅR KOLLIT I ETT          
063300*---------------------------------------------SAMLINGSKOLLI               
063400           MOVE FEL-9 (SPRAK-IX) TO  MOD-TEMFSFEL                         
063500           MOVE JA TO INDATA-FEL                                          
063600         END-IF                                                           
063700       END-IF                                                             
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100 CAAA-MASKINELL-PLATSSOEK SECTION.                                        
064200                                                                          
064300     IF KOLLI-KDFARLIG-KOLLI = +4                                         
064400     OR KOLLI-KDFARLIG-KOLLI = +7                                         
064500       MOVE +6                  TO PLATS-KDCALL                           
064600     ELSE                                                                 
064700       MOVE +0                  TO PLATS-KDCALL                           
064800     END-IF                                                               
064900                                                                          
065000                                                                          
065100                                                                          
065200     MOVE WS-IDDC               TO PLATS-IDDC                             
065300     MOVE KEY-IDDISTR           TO PLATS-IDDISTR                          
065400     MOVE KEY-IDKUNDNR          TO PLATS-IDKUNDNR                         
065500     MOVE SPAR-VORD-KDFRAKT     TO PLATS-KDFRAKT                          
065600     MOVE SPAR-VORD-KDORDKL     TO PLATS-KDORDKLX                         
065700     MOVE KEY-IDORDNR           TO PLATS-IDORDNR                          
065800     MOVE KOLLI-DIKOLLIH        TO PLATS-DIKOLLIH                         
065900     MOVE KOLLI-DIKOLLIL        TO PLATS-DIKOLLIL                         
066000     MOVE KOLLI-DIKOLLIB        TO PLATS-DIKOLLIB                         
066100     MOVE KOLLI-VKORDNTO-KOLLI  TO PLATS-VKORDNTO-KOLLI                   
066200     MOVE SPAR-EMB-KDKOLLID     TO PLATS-KDKOLLID                         
066300                                                                          
066400     MOVE SPACE                 TO PLATS-ADFLGEO                          
066500                                   PLATS-FLUTLAST                         
066600                                   PLATS-IDDC-CROSS                       
066700                                                                          
066800     MOVE ZERO                  TO PLATS-IDTRPTNR                         
066900                                   PLATS-ADFLOMR                          
067000                                   PLATS-ADRUTNIV                         
067100                                   PLATS-DIHMODUL                         
067200                                   PLATS-DIDMODUL                         
067300                                   PLATS-ADVMODUL                         
067400                                   PLATS-ADHMODUL                         
067500                                                                          
067600     PERFORM S03-ANROP-PLATS                                              
067700     IF PLATS-KDSVAR = SPACE                                              
067800                                                                          
067900        IF KOLLI-IDKOLLI-FLER = ZERO                                      
068000          PERFORM S01-REPLACE-KOLLI                                       
068100        ELSE                                                              
068200          PERFORM IMS-GET-KOLLI-FIRST                                     
068300          PERFORM UNTIL SEGMENT-SAKNAS                                    
068400            PERFORM S01-REPLACE-KOLLI                                     
068500            PERFORM IMS-GET-KOLLI-NEXT                                    
068600          END-PERFORM                                                     
068700        END-IF                                                            
068800                                                                          
068900        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
069000        PERFORM S02-AVBOKA-KOLLI                                          
069100     ELSE                                                                 
069200        MOVE JA TO INDATA-FEL                                             
069300        MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                             
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700 CAAB-MANUELL-PLATSSOEK SECTION.                                          
069800                                                                          
069900     IF SPAR-VORD-KDORDKL = 4                                             
070000        MOVE +1                 TO PLATS-KDCALL                           
070100     ELSE                                                                 
070200        MOVE +2                 TO PLATS-KDCALL                           
070300     END-IF                                                               
070400                                                                          
070500     MOVE WS-IDDC               TO PLATS-IDDC                             
070600     MOVE KEY-IDDISTR           TO PLATS-IDDISTR                          
070700     MOVE KEY-IDKUNDNR          TO PLATS-IDKUNDNR                         
070800     MOVE SPAR-VORD-KDFRAKT     TO PLATS-KDFRAKT                          
070900     MOVE SPAR-VORD-KDORDKL     TO PLATS-KDORDKLX                         
071000     MOVE KEY-IDORDNR           TO PLATS-IDORDNR                          
071100     MOVE KOLLI-DIKOLLIH        TO PLATS-DIKOLLIH                         
071200     MOVE KOLLI-DIKOLLIL        TO PLATS-DIKOLLIL                         
071300     MOVE KOLLI-DIKOLLIB        TO PLATS-DIKOLLIB                         
071400     MOVE KOLLI-VKORDNTO-KOLLI  TO PLATS-VKORDNTO-KOLLI                   
071500     MOVE SPAR-EMB-KDKOLLID     TO PLATS-KDKOLLID                         
071600     MOVE MID-ADFLGEO-NY(INDX)  TO PLATS-ADFLGEO                          
071700     MOVE MID-ADFLOMR-NY(INDX)  TO PLATS-ADFLOMR                          
071800     MOVE MID-ADRUTNIV-NY(INDX) TO PLATS-ADRUTNIV                         
071900                                                                          
072000     MOVE ZERO                  TO PLATS-IDTRPTNR                         
072100                                   PLATS-DIHMODUL                         
072200                                   PLATS-DIDMODUL                         
072300                                   PLATS-ADVMODUL                         
072400                                   PLATS-ADHMODUL                         
072500                                                                          
072600     MOVE SPACE                 TO PLATS-FLUTLAST                         
072700     MOVE SPACE                 TO PLATS-IDDC-CROSS                       
072800                                                                          
072900     PERFORM S03-ANROP-PLATS                                              
073000     IF PLATS-KDSVAR = SPACE                                              
073100                                                                          
073200        IF KOLLI-IDKOLLI-FLER = ZERO                                      
073300          PERFORM S01-REPLACE-KOLLI                                       
073400        ELSE                                                              
073500          PERFORM IMS-GET-KOLLI-FIRST                                     
073600          PERFORM UNTIL SEGMENT-SAKNAS                                    
073700            PERFORM S01-REPLACE-KOLLI                                     
073800            PERFORM IMS-GET-KOLLI-NEXT                                    
073900          END-PERFORM                                                     
074000        END-IF                                                            
074100                                                                          
074200        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
074300        PERFORM S02-AVBOKA-KOLLI                                          
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 D-FRAGA SECTION.                                                         
074800                                                                          
074900     MOVE NEJ TO IDKOLLI-HITTAT                                           
075000                                                                          
075100     IF MFS-IDPFK = '7'                                                   
075200        MOVE ZERO TO KEY-IDKOLLI                                          
075300     ELSE                                                                 
075400        IF MFS-IDPFK = '8'                                                
075500           MOVE MID-IDKOLLI-NEXT     TO KEY-IDKOLLI                       
075600        ELSE                                                              
075700           IF MID-IDKOLLI-IN NUMERIC                                      
075800              MOVE MID-IDKOLLI-IN    TO KEY-IDKOLLI                       
075900           ELSE                                                           
076000              MOVE MID-IDKOLLI-FIRST TO KEY-IDKOLLI                       
076100           END-IF                                                         
076200        END-IF                                                            
076300     END-IF                                                               
076400                                                                          
076500     PERFORM DA-LAES-FRAGA                                                
076600                                                                          
076700     IF INDX < MAX-IX-PLUS-1                                              
076800        PERFORM DB-BLANKA-REST-RADER                                      
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 DA-LAES-FRAGA SECTION.                                                   
077300                                                                          
077400     MOVE +1 TO INDX                                                      
077500     MOVE 'N'             TO WS-SLINGA-KLAR                               
077600     MOVE KEY-IDDISTR     TO W-4A1-IDDISTR                                
077700     MOVE KEY-IDKUNDNR    TO W-4A1-IDKUNDNR                               
077800     MOVE KEY-IDORDNR     TO WS-IDORDNR                                   
077900     MOVE WS-IDKUNDRF     TO W-4A1-IDKUNDRF                               
078000     PERFORM IMS-GU-KUNDORDER-SEK                                         
078100*                                                                         
078200     IF KUNDORDER-SEK-FINNS                                               
078300        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
078400                      SLINGA-KLAR                                         
078500        MOVE KORD-IDDISTR   TO W-401-IDDISTR                              
078600        MOVE KORD-IDKUNDNR  TO W-401-IDKUNDNR                             
078700        MOVE KORD-IDORDNR5  TO W-401-IDORDNR                              
078800        MOVE KORD-IDPRODNR  TO W-401-IDPRODNR                             
078900        MOVE KORD-IDPLKLST  TO W-401-IDPLKLST                             
079000        PERFORM IMS-GU-KUNDORDER                                          
079100        MOVE KORD-KVORDRAD-LEVPL   TO SPAR-KVORDRAD-LEVPL                 
079200        MOVE KORD-IDPRODNR TO W-IDPRODNR                                  
079300        PERFORM IMS-GET-VORD                                              
079400        IF SEGMENT-SAKNAS                                                 
079500           MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                          
079600        ELSE                                                              
079700           IF            (VORD-IDDC = WS-IDDC    AND                      
079800                          SPAR-KVORDRAD-LEVPL   = ZERO)                   
079900              IF VORD-IDDC = WS-IDDC                                      
080000                 MOVE 'J'           TO WS-SLINGA-KLAR                     
080100                 MOVE VORD-IDPRODNR TO W-IDPRODNR                         
080200                 PERFORM DAA-LAES-KOLLI                                   
080300              END-IF                                                      
080400           END-IF                                                         
080500        END-IF                                                            
080600        PERFORM IMS-GN-KUNDORDER-SEK                                      
080700        END-PERFORM                                                       
080800     ELSE                                                                 
080900        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 DAA-LAES-KOLLI SECTION.                                                  
081400                                                                          
081500     PERFORM IMS-GU-WDE601                                                
081600     MOVE KEY-IDKOLLI TO W-IDKOLLI                                        
081700     PERFORM IMS-GNP-WDE611                                               
081800     IF SEGMENT-FINNS                                                     
081900        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
082000                INDX NOT < MAX-IX-PLUS-1                                  
082100        IF (KOLLI-KDKOLSTA = 1) AND                                       
082200           (KOLLI-IDKOLLI NOT < ZERO)     AND                             
082300           (KOLLI-IDKOLLI NOT < W-IDKOLLI)                                
082400           MOVE KOLLI-IDKOLLI   TO MOD-IDKOLLI(INDX)                      
082500           IF KOLLI-IDKOLLI-FLER < ZERO                                   
082600               MOVE '*'         TO MOD-KDFLERK(INDX)                      
082700           ELSE                                                           
082800               MOVE SPACE       TO MOD-KDFLERK(INDX)                      
082900           END-IF                                                         
083000           MOVE KOLLI-ADFLGEO   TO MOD-ADFLGEO(INDX)                      
083100           MOVE KOLLI-ADFLOMR   TO MOD-ADFLOMR(INDX)                      
083200           MOVE KOLLI-ADRUTNIV  TO MOD-ADRUTNIV(INDX)                     
083300           MOVE KOLLI-ADVMODUL  TO MOD-ADVMODUL(INDX)                     
083400           MOVE KOLLI-ADHMODUL  TO MOD-ADHMODUL(INDX)                     
083500           MOVE MFS-RENSA-FAELT TO MOD-ADFLGEO-NY(INDX)                   
083600                                   MOD-ADFLOMR-NY(INDX)                   
083700                                   MOD-ADRUTNIV-NY(INDX)                  
083800                                   MOD-KDCMD(INDX)                        
083900           MOVE KOLLI-KDKOLLI   TO MOD-KDKOLLI(INDX)                      
084000           ADD +1 TO INDX                                                 
084100        END-IF                                                            
084200        PERFORM IMS-GNP-WDE611                                            
084300        END-PERFORM                                                       
084400     ELSE                                                                 
084500        MOVE FEL-5(SPRAK-IX) TO MOD-TEMFSFEL                              
084600     END-IF                                                               
084700                                                                          
084800     IF SEGMENT-FINNS                                                     
084900        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
085000                IDKOLLI-HITTAT NOT = NEJ                                  
085100           IF (KOLLI-KDKOLSTA = 1 OR 6 OR 7) AND                          
085200              (KOLLI-IDKOLLI NOT < ZERO) AND                              
085300              (KOLLI-IDKOLLI NOT < W-IDKOLLI)                             
085400              MOVE KOLLI-IDKOLLI TO MOD-IDKOLLI-NEXT                      
085500              MOVE JA TO IDKOLLI-HITTAT                                   
085600           END-IF                                                         
085700           PERFORM IMS-GNP-WDE611                                         
085800        END-PERFORM                                                       
085900        IF IDKOLLI-HITTAT = JA                                            
086000           MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                          
086100        ELSE                                                              
086200           MOVE ZERO TO MOD-IDKOLLI-NEXT                                  
086300        END-IF                                                            
086400     ELSE                                                                 
086500        MOVE ZERO             TO MOD-IDKOLLI-NEXT                         
086600     END-IF                                                               
086700     MOVE KEY-IDKOLLI         TO MOD-IDKOLLI-FIRST                        
086800     .                                                                    
086900     EJECT                                                                
087000 DB-BLANKA-REST-RADER SECTION.                                            
087100                                                                          
087200     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1                               
087300        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                     
087400                                 MOD-ADFLGEO-NY-ATTR(INDX)                
087500                                 MOD-ADFLOMR-NY-ATTR(INDX)                
087600                                 MOD-ADRUTNIV-NY-ATTR(INDX)               
087700        MOVE MFS-RENSA-FAELT  TO MOD-KDCMD(INDX)                          
087800                                 MOD-IDKOLLI(INDX)                        
087900                                 MOD-ADFLGEO(INDX)                        
088000                                 MOD-ADFLOMR(INDX)                        
088100                                 MOD-ADRUTNIV(INDX)                       
088200                                 MOD-ADVMODUL(INDX)                       
088300                                 MOD-ADHMODUL(INDX)                       
088400                                 MOD-ADFLGEO-NY(INDX)                     
088500                                 MOD-ADFLOMR-NY(INDX)                     
088600                                 MOD-ADRUTNIV-NY(INDX)                    
088700                                 MOD-KDKOLLI(INDX)                        
088800        ADD +1 TO INDX                                                    
088900     END-PERFORM                                                          
089000     .                                                                    
089100     EJECT                                                                
089200 E-RENSA-NYCKLAR SECTION.                                                 
089300     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                          
089400                                  MOD-IDKUNDNR-UT                         
089500                                  MOD-IDORDNR-UT                          
089600                                  MOD-IDKOLLI-UT                          
089700     .                                                                    
089800     EJECT                                                                
089900 MFS-ROER-EJ-BILD SECTION.                                                
090000                                                                          
090100     MOVE +1 TO INDX                                                      
090200                                                                          
090300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-FIRST                          
090400                               MOD-IDKOLLI-NEXT                           
090500                                                                          
090600     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1                               
090700                                                                          
090800       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD(INDX)                         
090900                                  MOD-IDKOLLI(INDX)                       
091000                                  MOD-ADFLGEO(INDX)                       
091100                                  MOD-ADFLOMR(INDX)                       
091200                                  MOD-ADRUTNIV(INDX)                      
091300                                  MOD-ADVMODUL(INDX)                      
091400                                  MOD-ADHMODUL(INDX)                      
091500                                  MOD-ADFLGEO-NY(INDX)                    
091600                                  MOD-ADFLOMR-NY(INDX)                    
091700                                  MOD-ADRUTNIV-NY(INDX)                   
091800                                  MOD-KDKOLLI(INDX)                       
091900       ADD +1 TO INDX                                                     
092000     END-PERFORM                                                          
092100     .                                                                    
092200     EJECT                                                                
092300 MFS-RENSA-BILD SECTION.                                                  
092400                                                                          
092500     MOVE +1 TO INDX                                                      
092600     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1                               
092700        MOVE MFS-RENSA-FAELT TO MOD-KDCMD(INDX)                           
092800                                MOD-IDKOLLI(INDX)                         
092900                                MOD-ADFLGEO(INDX)                         
093000                                MOD-ADFLOMR(INDX)                         
093100                                MOD-ADRUTNIV(INDX)                        
093200                                MOD-ADVMODUL(INDX)                        
093300                                MOD-ADHMODUL(INDX)                        
093400                                MOD-ADFLGEO-NY(INDX)                      
093500                                MOD-ADFLOMR-NY(INDX)                      
093600                                MOD-ADRUTNIV-NY(INDX)                     
093700                                MOD-KDKOLLI(INDX)                         
093800        ADD +1 TO INDX                                                    
093900     END-PERFORM                                                          
094000     .                                                                    
094100     EJECT                                                                
094200 MFS-GRUND-FORMAT SECTION.                                                
094300                                                                          
094400     MOVE +1 TO INDX                                                      
094500     PERFORM UNTIL INDX NOT < MAX-IX-PLUS-1                               
094600        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR(INDX)                   
094700                                   MOD-ADFLGEO-NY-ATTR(INDX)              
094800                                   MOD-ADFLOMR-NY-ATTR(INDX)              
094900                                   MOD-ADRUTNIV-NY-ATTR(INDX)             
095000        ADD +1 TO INDX                                                    
095100     END-PERFORM                                                          
095200     .                                                                    
095300     EJECT                                                                
095400 S01-REPLACE-KOLLI SECTION.                                               
095500                                                                          
095600     MOVE PLATS-IDTRPTNR   TO KOLLI-IDTRPTNR                              
095700     MOVE PLATS-ADCLGEO    TO KOLLI-ADCLGEO                               
095800     MOVE PLATS-ADFLOMR    TO KOLLI-ADFLOMR                               
095900     MOVE PLATS-ADRUTNIV   TO KOLLI-ADRUTNIV                              
096000     MOVE PLATS-DIHMODUL   TO KOLLI-DIHMODUL                              
096100     MOVE PLATS-DIDMODUL   TO KOLLI-DIDMODUL                              
096200     MOVE PLATS-ADVMODUL   TO KOLLI-ADVMODUL                              
096300     MOVE PLATS-ADHMODUL   TO KOLLI-ADHMODUL                              
096400     MOVE PLATS-FLUTLAST   TO KOLLI-FLUTLAST                              
096600     PERFORM IMS-REPLACE-KOLLI                                            
096700     .                                                                    
096800     EJECT                                                                
096900 S02-AVBOKA-KOLLI SECTION.                                                
097000                                                                          
097100     MOVE +3                  TO PLATS-KDCALL                             
097200     MOVE SPAR-KOLLI-IDTRPTNR TO PLATS-IDTRPTNR                           
097300     MOVE SPAR-KOLLI-DARFS (3:10)   TO PLATS-TIRFS                        
097400     MOVE SPAR-KOLLI-ADCLGEO  TO PLATS-ADCLGEO                            
097500     MOVE SPAR-KOLLI-ADFLOMR  TO PLATS-ADFLOMR                            
097600     MOVE SPAR-KOLLI-ADRUTNIV TO PLATS-ADRUTNIV                           
097700     MOVE SPAR-KOLLI-DIHMODUL TO PLATS-DIHMODUL                           
097800     MOVE SPAR-KOLLI-DIDMODUL TO PLATS-DIDMODUL                           
097900     MOVE SPAR-KOLLI-ADVMODUL TO PLATS-ADVMODUL                           
098000     MOVE SPAR-KOLLI-ADHMODUL TO PLATS-ADHMODUL                           
098100     PERFORM S03-ANROP-PLATS                                              
098200     .                                                                    
098300     EJECT                                                                
098400 S03-ANROP-PLATS SECTION.                                                 
098500                                                                          
098600     CALL W403PLAT USING PLATS-W403PLAT                                   
098700                         PLATS-DM-PCB                                     
098800                         PLATS-DN-PCB                                     
098900                         PLATS-DP-PCB                                     
099000                         PLATS-DO-PCB                                     
099100                         PLATS-WDE6C-PCB                                  
099200                         PLATS-GMTC-PCB                                   
099300                         PLATS-WDB6-PCB                                   
099400     EJECT                                                                
099500     .                                                                    
099600* IMS SEKTIONER                                                           
099700     SKIP3                                                                
099800 IMS-GET-MSG SECTION.                                                     
099900                                                                          
100000     MOVE '  QC' TO GODK-STATUSKODER                                      
100100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     SKIP3                                                                
100500     .                                                                    
100600 IMS-INSERT-MSG SECTION.                                                  
100700                                                                          
100800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
100900       MOVE '0' TO MFS-KDHUVOMR                                           
101000     END-IF                                                               
101100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101200     MOVE SPACE TO GODK-STATUSKODER                                       
101300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     EJECT                                                                
101800 IMS-GU-KUNDORDER-SEK SECTION.                                            
101900                                                                          
102000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1KEY-X ')'                       
102100            DELIMITED BY SIZE INTO SSA1                                   
102200     MOVE '  GE' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
102400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
102500                              STATUS-KUNDORDER-SEK-WS                     
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     SKIP3                                                                
102800     .                                                                    
102900 IMS-GN-KUNDORDER-SEK SECTION.                                            
103000                                                                          
103100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1KEY-X ')'                       
103200            DELIMITED BY SIZE INTO SSA1                                   
103300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
103400     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
103500     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
103600                              STATUS-KUNDORDER-SEK-WS                     
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     SKIP3                                                                
103900     .                                                                    
104000 IMS-GU-KUNDORDER SECTION.                                                
104100                                                                          
104200     STRING 'WDE401  (WDE401KY =' W-WDE401KEY-X ')'                       
104300            DELIMITED BY SIZE INTO SSA1                                   
104400     MOVE '    ' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
104600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     SKIP3                                                                
104900     .                                                                    
105000 IMS-GET-VORD SECTION.                                                    
105100                                                                          
105200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
105300            DELIMITED BY SIZE INTO SSA1                                   
105400     MOVE '  GE' TO GODK-STATUSKODER                                      
105500     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
105600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     SKIP3                                                                
105900     .                                                                    
106000     EJECT                                                                
106100 IMS-GHU-WDE601-11 SECTION.                                               
106200                                                                          
106300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
106400            DELIMITED BY SIZE INTO SSA1                                   
106500     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
106600            DELIMITED BY SIZE INTO SSA2                                   
106700     MOVE '  GE' TO GODK-STATUSKODER                                      
106800     CALL CBLTDLI USING GHU WDE6-PCB                                      
106900           DLI-IO-AREA SSA1 SSA2                                          
107000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
107100     PERFORM IMS-STATUSKONTROLL                                           
107200     SKIP2                                                                
107300     .                                                                    
107400 IMS-GET-KOLLI-FLER-FIRST SECTION.                                        
107500                                                                          
107600     STRING 'WDE611  *F(IDKOLLI  =' W-IDKOLLI-X ')'                       
107700            DELIMITED BY SIZE INTO SSA1                                   
107800     MOVE '  GE' TO GODK-STATUSKODER                                      
107900     CALL CBLTDLI USING GHNP WDE6-PCB                                     
108000           DLI-IO-AREA SSA1                                               
108100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     SKIP2                                                                
108400     .                                                                    
108500 IMS-GET-KOLLI-FIRST SECTION.                                             
108600                                                                          
108700     STRING 'WDE611  *F(IDKOLLIF =' W-IDKOLLI-FLER-X ')'                  
108800            DELIMITED BY SIZE INTO SSA1                                   
108900     MOVE '  GE' TO GODK-STATUSKODER                                      
109000     CALL CBLTDLI USING GHNP WDE6-PCB                                     
109100           DLI-IO-AREA SSA1                                               
109200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
109300     PERFORM IMS-STATUSKONTROLL                                           
109400     SKIP2                                                                
109500     .                                                                    
109600 IMS-GET-KOLLI-NEXT SECTION.                                              
109700                                                                          
109800     STRING 'WDE611  (IDKOLLIF =' W-IDKOLLI-FLER-X ')'                    
109900            DELIMITED BY SIZE INTO SSA1                                   
110000     MOVE '  GE' TO GODK-STATUSKODER                                      
110100     CALL CBLTDLI USING GHNP WDE6-PCB                                     
110200           DLI-IO-AREA SSA1                                               
110300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
110400     PERFORM IMS-STATUSKONTROLL                                           
110500     SKIP2                                                                
110600     .                                                                    
110700 IMS-REPLACE-KOLLI SECTION.                                               
110800                                                                          
110900     MOVE '  ' TO GODK-STATUSKODER                                        
111000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA                         
111100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
111200     PERFORM IMS-STATUSKONTROLL                                           
111300     .                                                                    
111400     EJECT                                                                
111500 IMS-GU-WDE601 SECTION.                                                   
111600                                                                          
111700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
111800            DELIMITED BY SIZE INTO SSA1                                   
111900     MOVE '    ' TO GODK-STATUSKODER                                      
112000     CALL CBLTDLI USING GU WDE62-PCB                                      
112100           DLI-IO-AREA SSA1                                               
112200     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
112300     PERFORM IMS-STATUSKONTROLL                                           
112400     .                                                                    
112500     SKIP2                                                                
112600 IMS-GNP-WDE611 SECTION.                                                  
112700                                                                          
112800     MOVE 'WDE611   ' TO SSA1                                             
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GNP WDE62-PCB                                     
113100           DLI-IO-AREA SSA1                                               
113200     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     EJECT                                                                
113600 IMS-GET-EMB SECTION.                                                     
113700                                                                          
113800     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-X ')'                         
113900            DELIMITED BY SIZE INTO SSA1                                   
114000     MOVE '  GE' TO GODK-STATUSKODER                                      
114100     CALL CBLTDLI USING GU EMB-PCB DLI-IO-AREA SSA1                       
114200     MOVE EMB-STATUS-CODE TO STATUS-WS                                    
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     .                                                                    
114500     EJECT                                                                
114600 IMS-STATUSKONTROLL SECTION.                                              
114700                                                                          
114800     SET STATUS-IX TO 1                                                   
114900     SEARCH GODK-STATUS AT END CALL FELLOG                                
115000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
115100     END-SEARCH                                                           
115200     SKIP2                                                                
115300                                                                          
115400     .                                                                    
