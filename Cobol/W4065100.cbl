000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4065100.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   MARS -86.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMETS FRÅGEDEL LISTAR ETT/FLERA NIVÅER INOM                
001000*                    ETT UNIKT GEOGR OMR - LAST OMR/STÄLLAGE.             
001100*        PROGRAMMETS UPPDATERINGSDEL ÄR INDELAT I 3 FUNKTIONER            
001200*                    Ä = ÄNDRING FÅR BARA GÖRAS AV VOLYM OCH              
001300*                        SPÄRRTEXT                                        
001400*                    B = GÖRS AV HELA NIVÅN/RUTAN                         
001500*    RUTA FULL AVBOKAS OCH FÖRSVINNER NÄR MAN LASTAR UT SISTA             
001600*    KOLLIT PÅ DEN RUTAN. (AVBOKAS AV W403PLAT.)                          
001700*    DET GÅR INTE ATT BOKA AV MANUELLT VIA NÅGON BILD.                    
001800*    NÄR EN RUTA ÄR FULL SÅ SKAPAS DET EN NY SOM ÄR AKTIV.                
001900*                                                                         
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T651                                              
002400*        MID:         W4I65101                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W4O65101                                            
002800*    SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(8)    VALUE 'W4065100'.            
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  SPRAK-IX                    PIC S9(4)   VALUE +0   COMP SYNC.        
004000 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004100 77  MAX-IX                      PIC S9(4)   VALUE +13  COMP SYNC.        
004200 77  MAX-IX-PLUS-1               PIC S9(4)   VALUE +14  COMP SYNC.        
004300 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +807 COMP SYNC.        
004400 77  UPDATE-DONE                 PIC X       VALUE 'N'.                   
004500 77  NYUPDATE                    PIC X       VALUE 'N'.                   
004600 77  INDATA-FEL                  PIC X       VALUE 'N'.                   
004700 77  FEL-BORTTAG                 PIC X       VALUE 'N'.                   
004800 77  PF11-TEST                   PIC X       VALUE 'N'.                   
004900 77  WS-IDTRANS                  PIC X(4).                                
005000     88  WS-GODKAEND-BILD                    VALUE '4651' '4652'.         
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005500     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
005600     EJECT                                                                
005700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
005800*                                                                         
005900 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
006000*01 -COPY W403PLAT                                                        
006100     EJECT                                                                
006200*                                                                         
006300 01  KEY-ADFLGEO                 PIC X(3)    VALUE SPACE.                 
006400 01  ADFLOMR-WS                  PIC X(3)    VALUE SPACE.                 
006500 01  FILLER     REDEFINES ADFLOMR-WS.                                     
006600     03  KEY-ADFLOMR             PIC 9(3).                                
006700 01  ADRUTNIV-WS                 PIC X(3)    VALUE SPACE.                 
006800 01  FILLER     REDEFINES ADRUTNIV-WS.                                    
006900     03  KEY-ADRUTNIV            PIC 9(3).                                
007000     SKIP2                                                                
007100 01  WS-IDDC                     PIC X(2).                                
007200 01  DIDMODUL-WS                 PIC X(3)    VALUE SPACE.                 
007300 01  FILLER     REDEFINES DIDMODUL-WS.                                    
007400     03  KEY-DIDMODUL            PIC 9(3).                                
007500 01  DIHMODUL-WS                 PIC X(3)    VALUE SPACE.                 
007600 01  FILLER     REDEFINES DIHMODUL-WS.                                    
007700     03  KEY-DIHMODUL            PIC 9(3).                                
007800 01  WS-STAELL-SLUT              PIC X(11)   VALUE 'STAELL SLUT'.         
007900 01  WS-SLUT-TESPAERR.                                                    
008000     03  SLUT-TESPAERR           PIC X(11).                               
008100     03  FILLER                  PIC X(9).                                
008200                                                                          
008300 01  SPAR-ADRUTNIV               PIC S9(3)   COMP-3.                      
008400 01  SPAR-DIDMODUL               PIC S9(3)   COMP-3.                      
008500 01  SPAR-DIHMODUL               PIC S9(3)   COMP-3.                      
008600 01  SPAR-VLRUTNIV               PIC S9(3)   COMP-3.                      
008700 01  SPAR-ADHMODUL               PIC S9(3)   COMP-3.                      
008800     SKIP2                                                                
008900 01  W-RUTA-TESPAERR.                                                     
009000     03  W-TRPT               PIC X(4).                                   
009100     03  FILLER                  PIC X.                                   
009200     03  W-TRPTNR             PIC X(3).                                   
009300     03  FILLER                  PIC X.                                   
009400     03  W-TRPTDATUM          PIC X(6).                                   
009500     03  FILLER                  PIC X.                                   
009600     03  W-FULL               PIC X(4).                                   
009700     EJECT                                                                
009800 01  WS-FIXA-ZAETA-FAELT.                                                 
009900     03  FIXA-ADVMODUL        PIC X(3).                                   
010000     03  FIXA-ADHMODUL        PIC X(3).                                   
010100     EJECT                                                                
010200 01  FEL-MEDDELANDEN.                                                     
010300   03  FEL1.                                                              
010400     05 FILLER                   PIC X(40)                                
010500          VALUE '749  FEL NYCKEL               '.                         
010600     05 FILLER                   PIC X(40)                                
010700          VALUE '749  WRONG KEY                   '.                      
010800   03  FILLER REDEFINES FEL1.                                             
010900     05  FEL-1                   PIC X(40)   OCCURS 2.                    
011000                                                                          
011100   03  FEL2.                                                              
011200     05 FILLER                   PIC X(40)                                
011300          VALUE '748  UPPLYSTA FÄLT FEL        '.                         
011400     05 FILLER                   PIC X(40)                                
011500          VALUE '748  HIGHLIGHT FIELDS  WRONG    '.                       
011600   03  FILLER REDEFINES FEL2.                                             
011700     05  FEL-2                   PIC X(40)   OCCURS 2.                    
011800                                                                          
011900   03  FEL3.                                                              
012000     05 FILLER                   PIC X(40)                                
012100          VALUE '706  OMRÅDET SAKNAS           '.                         
012200     05 FILLER                   PIC X(40)                                
012300          VALUE '706  AREA MISSING             '.                         
012400   03  FILLER REDEFINES FEL3.                                             
012500     05  FEL-3                   PIC X(40)   OCCURS 2.                    
012600                                                                          
012700   03  FEL4.                                                              
012800     05 FILLER                   PIC X(40)                                
012900          VALUE '832  BORTTAG EJ TILLÅTET      '.                         
013000     05 FILLER                   PIC X(40)                                
013100          VALUE '832  DELETE NOT ALLOWED       '.                         
013200   03  FILLER REDEFINES FEL4.                                             
013300     05  FEL-4                   PIC X(40)   OCCURS 2.                    
013400                                                                          
013500   03  FEL5.                                                              
013600     05 FILLER                   PIC X(40)                                
013700          VALUE '812  TRYCK PF11 FÖR UPPDATERING '.                       
013800     05 FILLER                   PIC X(40)                                
013900          VALUE '812  PRESS PF11 TO UPDATING     '.                       
014000   03  FILLER REDEFINES FEL5.                                             
014100     05  FEL-5                   PIC X(40)   OCCURS 2.                    
014200     EJECT                                                                
014300 01  MEDDELANDEN.                                                         
014400   03  MED1.                                                              
014500     05 FILLER                   PIC X(40)                                
014600          VALUE 'UPPDATERING UTFÖRD       '.                              
014700     05 FILLER                   PIC X(40)                                
014800          VALUE 'UPDATING OK              '.                              
014900   03  FILLER REDEFINES MED1.                                             
015000     05  MED-1                   PIC X(40)   OCCURS 2.                    
015100                                                                          
015200   03  MED2.                                                              
015300     05 FILLER                   PIC X(40)                                
015400          VALUE 'FLER RADER FINNS         '.                              
015500     05 FILLER                   PIC X(40)                                
015600          VALUE 'MORE LINES              '.                               
015700   03  FILLER REDEFINES MED2.                                             
015800     05  MED-2                   PIC X(40)   OCCURS 2.                    
015900     EJECT                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-4411-WDGXKEY-X.                                                
016200         05  W-4411-IDHTYP         PIC X(4)    VALUE '4411'.              
016300         05  W-4411-ADCLGEO.                                              
016400             07  W-4411-IDDC       PIC X(02).                             
016500             07  W-4411-ADFLGEO    PIC X(3).                              
016600         05  W-4411-LOWVALUE       PIC X(21)   VALUE LOW-VALUE.           
016700     03  W-4412-WDGXKEY-X.                                                
016800         05  W-4412-ADFLOMR        PIC S9(3)   COMP-3.                    
016900         05  W-4412-LOWVALUE       PIC X(8)    VALUE LOW-VALUE.           
017000     03  W-4414-WDGXKEY-X.                                                
017100         05  W-4414-ADRUTNIV       PIC S9(3)   COMP-3.                    
017200         05  W-4414-LOWVALUE       PIC X(8)    VALUE LOW-VALUE.           
017300                                                                          
017400     03  W-4417-WDGXKEY-X.                                                
017500         05  W-4417-IDHTYP         PIC X(4)    VALUE '4417'.              
017600         05  W-4417-ADCLGEO.                                              
017700             07  W-4417-IDDC       PIC X(02).                             
017800             07  W-4417-ADFLGEO    PIC X(3).                              
017900         05  W-4417-LOWVALUE       PIC X(21)   VALUE LOW-VALUE.           
018000     03  W-4418-WDGXKEY-X.                                                
018100         05  W-4418-ADFLOMR        PIC S9(3)   COMP-3.                    
018200         05  W-4418-ADRUTNIV       PIC S9(3)   COMP-3.                    
018300         05  W-4418-DIHMODUL       PIC S9(3)   COMP-3.                    
018400         05  W-4418-DIDMODUL       PIC S9(3)   COMP-3.                    
018500         05  FILLER                PIC X(2)    VALUE LOW-VALUE.           
018600     03  W-4418-WDGXKEY-X-LOW.                                            
018700         05  W-LOW-4418-ADFLOMR    PIC S9(3)   COMP-3.                    
018800         05  W-LOW-4418-ADRUTNIV   PIC S9(3)   COMP-3.                    
018900         05  W-LOW-4418-DIHMODUL   PIC S9(3)   COMP-3.                    
019000         05  W-LOW-4418-DIDMODUL   PIC S9(3)   COMP-3.                    
019100         05  FILLER                PIC X(2)    VALUE LOW-VALUE.           
019200     03  W-4418-WDGXKEY-X-HIGH.                                           
019300         05  W-HIGH-4418-ADFLOMR   PIC S9(3)   COMP-3.                    
019400         05  FILLER                PIC X(8)    VALUE HIGH-VALUE.          
019500     03  W-4420-WDGXKEY-X.                                                
019600         05  W-4420-ADVMODUL       PIC S9(3)   COMP-3.                    
019700         05  W-4420-LOWVALUE       PIC X(8)    VALUE LOW-VALUE.           
019800     03  W-SEK-WDE6C1KY-X-MIN.                                            
019900         05  W-SEK-IDTRPTNR-MIN      PIC S9(3) COMP-3.                    
020000         05  W-SEK-DARFS-MIN         PIC 9(12).                           
020100         05  W-SEK-ADCLGEO-MIN.                                           
020200             07 W-SEK-IDDC-MIN           PIC X(2).                        
020300             07 W-SEK-ADFLGEO-MIN        PIC X(3).                        
020400         05  W-SEK-ADFLOMR-MIN       PIC S9(3) COMP-3.                    
020500         05  W-SEK-ADRUTNIV-MIN      PIC S9(3) COMP-3.                    
020600         05  W-SEK-ADVMODUL-MIN      PIC S9(3) COMP-3.                    
020700         05  W-SEK-IDDISTR-MIN       PIC S9(5) COMP-3.                    
020800         05  W-SEK-IDKUNDNR-MIN      PIC S9(7) COMP-3.                    
020900         05  W-SEK-IDPRODNR-MIN      PIC S9(7) COMP-3.                    
021000         05  W-SEK-IDKOLLI-FLER-MIN PIC S9(5) COMP-3.                     
021100         05  W-SEK-IDKOLLI-MIN       PIC S9(5) COMP-3.                    
021200     SKIP2                                                                
021300     03  W-SEK-WDE6C1KY-X-MAX.                                            
021400         05  W-SEK-IDTRPTNR-MAX      PIC S9(3) COMP-3.                    
021500         05  W-SEK-DARFS-MAX         PIC 9(12).                           
021600         05  W-SEK-ADCLGEO-MAX.                                           
021700             07 W-SEK-IDDC-MAX           PIC X(2).                        
021800             07 W-SEK-ADFLGEO-MAX        PIC X(3).                        
021900         05  W-SEK-ADFLOMR-MAX       PIC S9(3) COMP-3.                    
022000         05  W-SEK-ADRUTNIV-MAX      PIC S9(3) COMP-3.                    
022100         05  W-SEK-ADVMODUL-MAX      PIC S9(3) COMP-3.                    
022200         05  W-SEK-IDDISTR-MAX       PIC S9(5) COMP-3.                    
022300         05  W-SEK-IDKUNDNR-MAX      PIC S9(7) COMP-3.                    
022400         05  W-SEK-IDPRODNR-MAX      PIC S9(7) COMP-3.                    
022500         05  W-SEK-IDKOLLI-FLER-MAX PIC S9(5) COMP-3.                     
022600         05  W-SEK-IDKOLLI-MAX       PIC S9(5) COMP-3.                    
022700     SKIP2                                                                
022800     03 W-SEK-ADCLGEO-X.                                                  
022900        05 W-SEK-IDDC                PIC X(2).                            
023000        05 W-SEK-ADFLGEO             PIC X(3).                            
023100     03  W-SEK-ADFLOMR-X.                                                 
023200        05  W-SEK-ADFLOMR            PIC S9(3) COMP-3.                    
023300     03 W-SEK-ADRUTNIV-X.                                                 
023400        05  W-SEK-ADRUTNIV           PIC S9(3) COMP-3.                    
023500     EJECT                                                                
023600******************************************************************        
023700*                                                                         
023800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
023900*                                                                         
024000 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
024100     SKIP3                                                                
024200*01  MID -COPY W4I65101                                                   
024300     EJECT                                                                
024400*01  -COPY WMSGAREA                                                       
024500     EJECT                                                                
024600*  03  MOD -COPY W4O65101           -RED MSG-AREA.                        
024700     EJECT                                                                
024800*01  -COPY WMFSAREA                                                       
024900     EJECT                                                                
025000******************************************************************        
025100*                                                                         
025200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025300*                                                                         
025400 01  IMS-WS.                                                              
025500   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
025600     SKIP3                                                                
025700*                        **** STATUS-KOD FRÅN IMS                         
025800   03  STATUS-WS                 PIC XX.                                  
025900     88  SEGMENT-FINNS                       VALUE '  '.                  
026000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026100     SKIP3                                                                
026200   03  GODK-STATUSKODER.                                                  
026300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026400     SKIP3                                                                
026500 01    SSA1                      PIC X(192).                              
026600 01    SSA2                      PIC X(64).                               
026700     EJECT                                                                
026800 01  DLI-IO-AREA.                                                         
026900     03  IO-AREA         PIC X(100) VALUE SPACE.                          
027000*                                                                         
027100*    03  WLXXDO01  -COPY WDGX4411 -PRE PLATS- -RED IO-AREA.               
027200     EJECT                                                                
027300*    03  WLXXDO11  -COPY WDGX4412 -PRE PLATS- -RED IO-AREA.               
027400     EJECT                                                                
027500*    03  WLXXDO21  -COPY WDGX4414 -PRE PLATS- -RED IO-AREA.               
027600     EJECT                                                                
027700*    03  WLXXDP01  -COPY WDGX4417 -PRE PLATS- -RED IO-AREA.               
027800     EJECT                                                                
027900*    03  WLXXDP11  -COPY WDGX4418 -PRE PLATS- -RED IO-AREA.               
028000     EJECT                                                                
028100*    03  WLXXDP21  -COPY WDGX4420 -PRE PLATS- -RED IO-AREA.               
028200     EJECT                                                                
028300*    03            -COPY WDE6C1   -PRE PLATS- -RED IO-AREA.               
028400     EJECT                                                                
028500*                            IMS FUNKTIONSKODER                           
028600*01    -COPY W0003                                                        
028700     EJECT                                                                
028800 LINKAGE SECTION.                                                         
028900*01  -COPY W0009     -PRE MSG-                                            
029000     SKIP2                                                                
029100*01  -COPY W0008     -PRE PLATS-DM-                                       
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE PLATS-DN-                                       
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008     -PRE PLATS-DP-                                       
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008     -PRE PLATS-DO-                                       
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008     -PRE PLATS-GMTC-                                     
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008     -PRE PLATS-WDB6-                                     
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200 PROCEDURE DIVISION USING  MSG-PCB                                        
031300                           PLATS-DM-PCB   PLATS-DN-PCB                    
031400                           PLATS-DP-PCB   PLATS-DO-PCB                    
031500                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
031600                           PLATS-WDB6-PCB.                                
031700 STYR SECTION.                                                            
031800     ENTRY 'DLITCBL' USING MSG-PCB                                        
031900                           PLATS-DM-PCB   PLATS-DN-PCB                    
032000                           PLATS-DP-PCB   PLATS-DO-PCB                    
032100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
032200                           PLATS-WDB6-PCB.                                
032300                                                                          
032400     PERFORM IMS-GET-MSG                                                  
032500     IF SEGMENT-FINNS                                                     
032600        PERFORM A-INIT                                                    
032700        IF WS-GODKAEND-BILD                                               
032800          IF WS-IDDC NOT = SPACE                                          
032900             IF KEY-ADFLOMR  NUMERIC AND                                  
033000                KEY-ADRUTNIV NUMERIC                                      
033100                IF KEY-ADFLGEO NOT = SPACE AND                            
033200                   KEY-ADFLOMR NOT = ZERO                                 
033300                   EVALUATE TRUE                                          
033400                     WHEN MFS-UPDATE                AND                   
033500                          (MID-ADFLGEO-IN = ALL '+' AND                   
033600                          MID-ADFLOMR-IN  = ALL '+' AND                   
033700                          MID-ADRUTNIV-IN = ALL '+' AND                   
033800                          MID-IDDC-IN     = ALL '+')                      
033900                       PERFORM B-KOLLA-INDATA                             
034000                       IF INDATA-FEL = NEJ                                
034100                          PERFORM C-UPPDATERA                             
034200                          IF INDATA-FEL = NEJ                             
034300                             IF UPDATE-DONE = JA                          
034400                             MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF        
034500                             END-IF                                       
034600                          END-IF                                          
034700                          IF FEL-BORTTAG = JA                             
034800                             MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL        
034900                             PERFORM MFS-ROER-EJ-BILD                     
035000                          ELSE                                            
035100                             PERFORM MFS-RENSA-BILD                       
035200                             PERFORM MFS-RENSA-BILD-NY                    
035300                          END-IF                                          
035400                       ELSE                                               
035500                          MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL           
035600                          MOVE JA TO INDATA-FEL                           
035700                          PERFORM MFS-ROER-EJ-BILD                        
035800                       END-IF                                             
035900                     WHEN MFS-IDTRANS = '4651'       AND                  
036000                          MID-ADFLGEO-IN  = ALL '+'  AND                  
036100                          MID-ADFLOMR-IN  = ALL '+'  AND                  
036200                          MID-ADRUTNIV-IN = ALL '+'  AND                  
036300                          MID-IDDC-IN     = ALL '+'                       
036400                       PERFORM E-KOLLA-OM-MAN-MENAT-PF11                  
036500                       IF PF11-TEST = JA                                  
036600                          MOVE JA TO INDATA-FEL                           
036700                          MOVE FEL-5(SPRAK-IX) TO MOD-TEMFSFEL            
036800                          PERFORM MFS-ROER-EJ-BILD                        
036900                          PERFORM MFS-SAETT-LAES-IGEN-ATTR                
037000                                                                          
037100                          MOVE 1 TO INDX                                  
037200                          PERFORM UNTIL INDX > MAX-IX                     
037300                            IF MID-TESPAERR (INDX) (1:4) = 'TRPT'         
037400                              MOVE MFS-STAENG-FAELT                       
037500                                TO MOD-TESPAERR-ATTR(INDX)                
037600                            END-IF                                        
037700                            ADD 1 TO INDX                                 
037800                          END-PERFORM                                     
037900                                                                          
038000                       END-IF                                             
038100                  END-EVALUATE                                            
038200                  IF INDATA-FEL = NEJ                                     
038300                     PERFORM D-LAES                                       
038400                     PERFORM MFS-GRUND-FORMAT-NY                          
038500                     PERFORM MFS-RENSA-BILD-NY                            
038600                  END-IF                                                  
038700               ELSE                                                       
038800                  MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                   
038900                  PERFORM MFS-RENSA-BILD                                  
039000                  PERFORM MFS-RENSA-BILD-NY                               
039100                  PERFORM MFS-GRUND-FORMAT                                
039200               END-IF                                                     
039300             ELSE                                                         
039400                MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                     
039500                PERFORM MFS-RENSA-BILD                                    
039600                PERFORM MFS-RENSA-BILD-NY                                 
039700                PERFORM MFS-GRUND-FORMAT                                  
039800             END-IF                                                       
039900          ELSE                                                            
040000            MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                         
040100            PERFORM MFS-RENSA-BILD                                        
040200            PERFORM MFS-RENSA-BILD-NY                                     
040300            PERFORM MFS-GRUND-FORMAT                                      
040400          END-IF                                                          
040500        ELSE                                                              
040600          PERFORM F-RENSA-NYCKLAR                                         
040700        END-IF                                                            
040800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
040900        PERFORM IMS-INSERT-MSG                                            
041000     END-IF                                                               
041100                                                                          
041200     MOVE ZERO TO RETURN-CODE                                             
041300     GOBACK                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 A-INIT SECTION.                                                          
041700                                                                          
041800     IF MSG-DUBBLA-TRANSKODER                                             
041900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I65101                 
042000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
042100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
042200       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
042300       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
042400     ELSE                                                                 
042500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I65101                 
042600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
042700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
042800       MOVE SPACE                         TO MFS-KDTRTYP                  
042900                                             MFS-IDPFK                    
043000     END-IF                                                               
043100     MOVE MFS-IDTRANS             TO WS-IDTRANS                           
043200                                                                          
043300     MOVE LOW-VALUE               TO MSG-AREA                             
043400     MOVE 'W4O651N1'              TO MFS-IDMOD                            
043500     MOVE '4651'                  TO MOD-IDTRANS                          
043600                                                                          
043700     IF MFS-IDTRANS NOT = '4651'                                          
043800       MOVE SPACE                         TO MFS-KDTRTYP                  
043900     END-IF                                                               
044000                                                                          
044100     IF MFS-KDMFSFOR = '2'                                                
044200       MOVE +2                            TO SPRAK-IX                     
044300     ELSE                                                                 
044400       MOVE +1                            TO SPRAK-IX                     
044500     END-IF                                                               
044600*                                                                         
044700***** CHECK  GEO AREA                                                     
044800*                                                                         
044900     IF MID-ADFLGEO-IN = ALL '+'                                          
045000        MOVE MID-ADFLGEO-UT               TO KEY-ADFLGEO                  
045100     ELSE                                                                 
045200        MOVE MID-ADFLGEO-IN               TO KEY-ADFLGEO                  
045300     END-IF                                                               
045400*                                                                         
045500***** CHECK LOADING AREA                                                  
045600*                                                                         
045700     IF MID-ADFLOMR-IN = ALL '+'                                          
045800        MOVE MID-ADFLOMR-UT               TO ADFLOMR-WS                   
045900        INSPECT ADFLOMR-WS REPLACING LEADING SPACE BY ZERO                
046000     ELSE                                                                 
046100        MOVE MID-ADFLOMR-IN               TO ADFLOMR-WS                   
046200     END-IF                                                               
046300*                                                                         
046400***** CHECK SQUARE LEVEL                                                  
046500*                                                                         
046600     IF MID-ADRUTNIV-IN = ALL '+'                                         
046700        MOVE MID-ADRUTNIV-UT               TO ADRUTNIV-WS                 
046800        INSPECT ADRUTNIV-WS REPLACING LEADING SPACE BY ZERO               
046900     ELSE                                                                 
047000        MOVE MID-ADRUTNIV-IN               TO ADRUTNIV-WS                 
047100     END-IF                                                               
047200*                                                                         
047300***** CHECK DISTRIBUTION CENTRE                                           
047400*                                                                         
047500     IF MID-IDDC-IN = ALL '+'                                             
047600         MOVE MID-IDDC-UT                  TO WS-IDDC                     
047700     ELSE                                                                 
047800         MOVE MID-IDDC-IN                  TO WS-IDDC                     
047900     END-IF                                                               
048000                                                                          
048100     MOVE ZERO                    TO DIDMODUL-WS                          
048200                                     DIHMODUL-WS                          
048300                                                                          
048400     MOVE KEY-ADFLGEO             TO MOD-ADFLGEO-UT                       
048500     MOVE ADFLOMR-WS              TO MOD-ADFLOMR-UT                       
048600     INSPECT MOD-ADFLOMR-UT REPLACING LEADING ZERO BY SPACE               
048700                                                                          
048800     MOVE ADRUTNIV-WS             TO MOD-ADRUTNIV-UT                      
048900     INSPECT MOD-ADRUTNIV-UT REPLACING LEADING ZERO BY SPACE              
049000                                                                          
049100     MOVE WS-IDDC                 TO MOD-IDDC-UT                          
049200                                                                          
049300     MOVE MFS-RENSA-FAELT         TO MOD-ADFLGEO-IN                       
049400                                     MOD-ADFLOMR-IN                       
049500                                     MOD-ADRUTNIV-IN                      
049600                                     MOD-IDDC-IN                          
049700                                     MOD-TEMFSFEL                         
049800                                     MOD-TEMFSINF                         
049900                                                                          
050000     MOVE NEJ                     TO INDATA-FEL                           
050100                                     FEL-BORTTAG                          
050200     MOVE MID-ADVMODUL-SISTA      TO MOD-ADVMODUL-FOERRA                  
050300     MOVE MID-ADHMODUL-SISTA      TO MOD-ADHMODUL-FOERRA                  
050400     .                                                                    
050500     EJECT                                                                
050600 B-KOLLA-INDATA SECTION.                                                  
050700                                                                          
050800     MOVE +1 TO INDX                                                      
050900                                                                          
051000     PERFORM UNTIL INDX > MAX-IX                                          
051100       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
051200          IF MID-KDCMD (INDX) = 'B' OR 'Ä' OR ' '                         
051300             MOVE MFS-ALFA-FAELT-RAETT TO                                 
051400                  MOD-KDCMD-ATTR (INDX)                                   
051500             IF MID-KDCMD (INDX) = 'Ä'                                    
051600                IF MID-VLRUTNIV(INDX) NOT = ALL '+'                       
051700                   IF MID-VLRUTNIV(INDX) NUMERIC                          
051800                      MOVE MFS-NUM-FAELT-RAETT TO                         
051900                           MOD-VLRUTNIV-ATTR(INDX)                        
052000                   ELSE                                                   
052100                      MOVE MFS-ADD-LAES-IN-FAELT TO                       
052200                           MOD-KDCMD-ATTR(INDX)                           
052300                           MOD-TESPAERR-ATTR(INDX)                        
052400                      MOVE MFS-NUM-FAELT-FEL TO                           
052500                           MOD-VLRUTNIV-ATTR(INDX)                        
052600                      MOVE JA TO INDATA-FEL                               
052700                   END-IF                                                 
052800                END-IF                                                    
052900                IF MID-TESPAERR(INDX) NOT = ALL '+' AND                   
053000                  MID-FLAGGA-TRPT-DOLD(INDX) = NEJ                        
053100                   MOVE MID-TESPAERR(INDX) TO W-RUTA-TESPAERR             
053200                   IF W-TRPT = 'TRPT'                                     
053300                      MOVE MFS-ADD-LAES-IN-FAELT TO                       
053400                           MOD-KDCMD-ATTR(INDX)                           
053500                           MOD-VLRUTNIV-ATTR(INDX)                        
053600                      MOVE MFS-ALFA-FAELT-FEL TO                          
053700                           MOD-TESPAERR-ATTR(INDX)                        
053800                      MOVE JA TO INDATA-FEL                               
053900                   ELSE                                                   
054000                      MOVE MFS-ALFA-FAELT-RAETT TO                        
054100                           MOD-TESPAERR-ATTR(INDX)                        
054200                   END-IF                                                 
054300                END-IF                                                    
054400             END-IF                                                       
054500          ELSE                                                            
054600             MOVE MFS-ADD-LAES-IN-FAELT TO                                
054700                  MOD-VLRUTNIV-ATTR(INDX)                                 
054800                  MOD-TESPAERR-ATTR(INDX)                                 
054900             MOVE MFS-ALFA-FAELT-FEL TO                                   
055000                  MOD-KDCMD-ATTR (INDX)                                   
055100             MOVE JA TO INDATA-FEL                                        
055200          END-IF                                                          
055300       END-IF                                                             
055400       ADD +1 TO INDX                                                     
055500     END-PERFORM                                                          
055600     PERFORM BA-KOLLA-NYUPPLAEGG                                          
055700     .                                                                    
055800     EJECT                                                                
055900 BA-KOLLA-NYUPPLAEGG SECTION.                                             
056000                                                                          
056100     IF MID-ADRUTNIV-NY = ALL '+' AND                                     
056200        MID-DIDMODUL-NY = ALL '+' AND                                     
056300        MID-DIHMODUL-NY = ALL '+' AND                                     
056400        MID-ADVMODUL-NY = ALL '+' AND                                     
056500        MID-ADHMODUL-NY = ALL '+' AND                                     
056600        MID-VLRUTNIV-NY = ALL '+' AND                                     
056700        MID-TESPAERR-NY = ALL '+'                                         
056800        MOVE NEJ TO NYUPDATE                                              
056900                                                                          
057000     ELSE                                                                 
057100                                                                          
057200        IF MID-ADRUTNIV-NY = ALL '+'                                      
057300           PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                            
057400           MOVE MFS-NUM-FAELT-FEL TO                                      
057500                MOD-ADRUTNIV-NY-ATTR                                      
057600           MOVE JA TO INDATA-FEL                                          
057700        ELSE                                                              
057800           IF MID-ADRUTNIV-NY NUMERIC                                     
057900              MOVE MFS-NUM-FAELT-RAETT TO                                 
058000                   MOD-ADRUTNIV-NY-ATTR                                   
058100           ELSE                                                           
058200              PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                         
058300              MOVE MFS-NUM-FAELT-FEL TO                                   
058400                   MOD-ADRUTNIV-NY-ATTR                                   
058500              MOVE JA TO INDATA-FEL                                       
058600           END-IF                                                         
058700        END-IF                                                            
058800                                                                          
059000        IF KEY-ADFLOMR > 700 AND < 900                                    
059100           PERFORM BAA-KOLLA-IFYLLT                                       
059200        ELSE                                                              
059300           PERFORM BAB-KOLLA-EJ-IFYLLT                                    
059400        END-IF                                                            
059500                                                                          
059600        IF MID-VLRUTNIV-NY = ALL '+'                                      
059700           MOVE MFS-NUM-FAELT-RAETT     TO MOD-VLRUTNIV-NY-ATTR           
059800        ELSE                                                              
059900           IF MID-VLRUTNIV-NY NUMERIC                                     
060000              MOVE MFS-NUM-FAELT-RAETT  TO MOD-VLRUTNIV-NY-ATTR           
060100           ELSE                                                           
060200              PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                         
060300              MOVE MFS-NUM-FAELT-FEL    TO MOD-VLRUTNIV-NY-ATTR           
060400              MOVE JA TO INDATA-FEL                                       
060500           END-IF                                                         
060600        END-IF                                                            
060700                                                                          
060800        IF MID-TESPAERR-NY = ALL '+'                                      
060900           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-VLRUTNIV-NY-ATTR          
061000        ELSE                                                              
061100           MOVE MID-TESPAERR-NY TO W-RUTA-TESPAERR                        
061200           IF W-TRPT = 'TRPT'                                             
061300              PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                         
061400              MOVE MFS-ALFA-FAELT-FEL   TO MOD-TESPAERR-NY-ATTR           
061500              MOVE JA TO INDATA-FEL                                       
061600           ELSE                                                           
061700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESPAERR-NY-ATTR           
061800           END-IF                                                         
061900        END-IF                                                            
062000                                                                          
062100        IF INDATA-FEL = NEJ                                               
062200           MOVE JA  TO NYUPDATE                                           
062300        ELSE                                                              
062400           MOVE NEJ TO NYUPDATE                                           
062500        END-IF                                                            
062600     END-IF                                                               
062700     .                                                                    
062800     EJECT                                                                
062900 BAA-KOLLA-IFYLLT SECTION.                                                
063000                                                                          
063100     IF MID-DIDMODUL-NY = ALL '+'                                         
063200        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
063300        MOVE MFS-NUM-FAELT-FEL      TO MOD-DIDMODUL-NY-ATTR               
063400        MOVE JA TO INDATA-FEL                                             
063500     ELSE                                                                 
063600        IF MID-DIDMODUL-NY NUMERIC                                        
063700           MOVE MFS-NUM-FAELT-RAETT TO MOD-DIDMODUL-NY-ATTR               
063800        ELSE                                                              
063900           PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                            
064000           MOVE MFS-NUM-FAELT-FEL   TO MOD-DIDMODUL-NY-ATTR               
064100           MOVE JA TO INDATA-FEL                                          
064200        END-IF                                                            
064300     END-IF                                                               
064400     IF MID-DIHMODUL-NY = ALL '+'                                         
064500        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
064600        MOVE MFS-NUM-FAELT-FEL      TO MOD-DIHMODUL-NY-ATTR               
064700        MOVE JA TO INDATA-FEL                                             
064800     ELSE                                                                 
064900        IF MID-DIHMODUL-NY NUMERIC                                        
065000           MOVE MFS-NUM-FAELT-RAETT TO MOD-DIHMODUL-NY-ATTR               
065100        ELSE                                                              
065200           PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                            
065300           MOVE MFS-NUM-FAELT-FEL   TO MOD-DIHMODUL-NY-ATTR               
065400           MOVE JA TO INDATA-FEL                                          
065500        END-IF                                                            
065600     END-IF                                                               
065700     IF MID-ADVMODUL-NY = ALL '+'                                         
065800        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
065900        MOVE MFS-NUM-FAELT-FEL      TO MOD-ADVMODUL-NY-ATTR               
066000        MOVE JA TO INDATA-FEL                                             
066100     ELSE                                                                 
066200        IF MID-ADVMODUL-NY NUMERIC                                        
066300           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADVMODUL-NY-ATTR               
066400        ELSE                                                              
066500           PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                            
066600           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADVMODUL-NY-ATTR               
066700           MOVE JA TO INDATA-FEL                                          
066800        END-IF                                                            
066900     END-IF                                                               
067000     IF MID-ADHMODUL-NY = ALL '+'                                         
067100        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
067200        MOVE MFS-NUM-FAELT-FEL      TO MOD-ADHMODUL-NY-ATTR               
067300        MOVE JA TO INDATA-FEL                                             
067400     ELSE                                                                 
067500        IF MID-ADHMODUL-NY NUMERIC                                        
067600           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADHMODUL-NY-ATTR               
067700        ELSE                                                              
067800           PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                            
067900           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADHMODUL-NY-ATTR               
068000           MOVE JA TO INDATA-FEL                                          
068100        END-IF                                                            
068200     END-IF                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 BAB-KOLLA-EJ-IFYLLT SECTION.                                             
068600                                                                          
068700     IF MID-DIDMODUL-NY = ALL '+'                                         
068800        MOVE MFS-NUM-FAELT-RAETT TO MOD-DIDMODUL-NY-ATTR                  
068900     ELSE                                                                 
069000        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
069100        MOVE MFS-NUM-FAELT-FEL   TO MOD-DIDMODUL-NY-ATTR                  
069200        MOVE JA TO INDATA-FEL                                             
069300     END-IF                                                               
069400     IF MID-DIHMODUL-NY = ALL '+'                                         
069500        MOVE MFS-NUM-FAELT-RAETT TO MOD-DIHMODUL-NY-ATTR                  
069600     ELSE                                                                 
069700        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
069800        MOVE MFS-NUM-FAELT-FEL   TO MOD-DIHMODUL-NY-ATTR                  
069900        MOVE JA TO INDATA-FEL                                             
070000     END-IF                                                               
070100     IF MID-ADVMODUL-NY = ALL '+'                                         
070200        MOVE MFS-NUM-FAELT-RAETT TO MOD-ADVMODUL-NY-ATTR                  
070300     ELSE                                                                 
070400        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
070500        MOVE MFS-NUM-FAELT-FEL   TO MOD-ADVMODUL-NY-ATTR                  
070600        MOVE JA TO INDATA-FEL                                             
070700     END-IF                                                               
070800     IF MID-ADHMODUL-NY = ALL '+'                                         
070900        MOVE MFS-NUM-FAELT-RAETT TO MOD-ADHMODUL-NY-ATTR                  
071000     ELSE                                                                 
071100        PERFORM MFS-SAETT-LAES-IGEN-ATTR-NY                               
071200        MOVE MFS-NUM-FAELT-FEL   TO MOD-ADHMODUL-NY-ATTR                  
071300        MOVE JA TO INDATA-FEL                                             
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 C-UPPDATERA SECTION.                                                     
071800                                                                          
071900     MOVE +1 TO INDX                                                      
072000     MOVE NEJ TO UPDATE-DONE                                              
072100                                                                          
072200     PERFORM UNTIL INDX > MAX-IX                                          
072300        IF MID-KDCMD(INDX) = ALL '+'                                      
072400           CONTINUE                                                       
072500        ELSE                                                              
072600           IF MID-KDCMD-INGENTING(INDX)                                   
072700              CONTINUE                                                    
072800           ELSE                                                           
072900              IF MID-KDCMD-DELETE(INDX)                                   
073000                 PERFORM CA-BORTTAG                                       
073100              ELSE                                                        
073200                 PERFORM CB-AENDRING                                      
073300              END-IF                                                      
073400           END-IF                                                         
073500        END-IF                                                            
073600        ADD +1 TO INDX                                                    
073700     END-PERFORM                                                          
073800                                                                          
073900     IF INDATA-FEL = NEJ AND NYUPDATE = JA                                
074000        PERFORM CC-NYUPPLAEGG                                             
074100     END-IF                                                               
074200                                                                          
074300     .                                                                    
074400     EJECT                                                                
074500 CA-BORTTAG SECTION.                                                      
074600                                                                          
074710     IF ADFLOMR-WS > 700 AND < 900                                        
074800        PERFORM CAA-BORTTAG-STAELL                                        
074900     ELSE                                                                 
075000        PERFORM CAB-BORTTAG-LAST                                          
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 CAA-BORTTAG-STAELL SECTION.                                              
075500                                                                          
075600     MOVE LOW-VALUE    TO W-SEK-WDE6C1KY-X-MIN                            
075700     MOVE HIGH-VALUE   TO W-SEK-WDE6C1KY-X-MAX                            
075800     MOVE ZERO         TO W-SEK-IDTRPTNR-MIN                              
075900                          W-SEK-IDTRPTNR-MAX                              
076000     MOVE KEY-ADFLGEO  TO W-4417-ADFLGEO                                  
076100                          W-SEK-ADFLGEO-MIN                               
076200                          W-SEK-ADFLGEO-MAX                               
076300     MOVE WS-IDDC      TO W-4417-IDDC                                     
076400                          W-SEK-IDDC-MIN                                  
076500                          W-SEK-IDDC-MAX                                  
076600     PERFORM IMS-LAS-STAELL-ROT                                           
076700     IF SEGMENT-FINNS                                                     
076800        MOVE KEY-ADFLOMR        TO W-4418-ADFLOMR                         
076900                                   W-SEK-ADFLOMR-MIN                      
077000                                   W-SEK-ADFLOMR-MAX                      
077100        MOVE MID-ADRUTNIV(INDX) TO W-4418-ADRUTNIV                        
077200                                   W-SEK-ADRUTNIV-MIN                     
077300                                   W-SEK-ADRUTNIV-MAX                     
077400        MOVE MID-DIHMODUL(INDX) TO W-4418-DIHMODUL                        
077500        MOVE MID-DIDMODUL(INDX) TO W-4418-DIDMODUL                        
077600        PERFORM IMS-LAS-KOLLI-SEK-RUTA                                    
077700        IF SEGMENT-FINNS                                                  
077800          MOVE JA TO FEL-BORTTAG                                          
077900          MOVE JA TO INDATA-FEL                                           
078000        ELSE                                                              
078100          PERFORM IMS-LAS-STAELL-AENDR-KVAL                               
078200          IF SEGMENT-FINNS                                                
078300             PERFORM IMS-DELETE-STAELL                                    
078400             MOVE JA TO UPDATE-DONE                                       
078500             PERFORM CAAA-RENSA-STAELL                                    
078600          END-IF                                                          
078700        END-IF                                                            
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100 CAAA-RENSA-STAELL SECTION.                                               
079200                                                                          
079300     PERFORM IMS-LAS-STAELL-NIVA-FIRST                                    
079400     IF SEGMENT-SAKNAS                                                    
079500        PERFORM IMS-LAS-STAELL-ROT                                        
079600        IF SEGMENT-FINNS                                                  
079700           PERFORM IMS-DELETE-STAELL                                      
079800           MOVE JA TO UPDATE-DONE                                         
079900        END-IF                                                            
080000     END-IF                                                               
080100     .                                                                    
080200     EJECT                                                                
080300 CAB-BORTTAG-LAST SECTION.                                                
080400                                                                          
080500     MOVE KEY-ADFLGEO  TO W-4411-ADFLGEO                                  
080600     MOVE WS-IDDC      TO W-4411-IDDC                                     
080700     PERFORM IMS-LAS-RUTA-ROT                                             
080800     IF SEGMENT-FINNS                                                     
080900        MOVE KEY-ADFLOMR  TO W-4412-ADFLOMR                               
081000        PERFORM IMS-LAS-RUTA-LASTOMR                                      
081100        IF SEGMENT-FINNS                                                  
081200           MOVE MID-ADRUTNIV(INDX) TO W-4414-ADRUTNIV                     
081300           PERFORM IMS-LAS-RUTA-RUTA-KVAL                                 
081400           IF SEGMENT-FINNS                                               
081500              MOVE PLATS-RUTA-TESPAERR TO W-RUTA-TESPAERR                 
081600              IF W-TRPT = 'TRPT'                                          
081700                 MOVE JA TO INDATA-FEL                                    
081800                 MOVE JA TO FEL-BORTTAG                                   
081900              ELSE                                                        
082000                 PERFORM IMS-DELETE-RUTA                                  
082100                 MOVE JA TO UPDATE-DONE                                   
082200                 PERFORM CABA-RENSA-LAST                                  
082300              END-IF                                                      
082400           END-IF                                                         
082500        END-IF                                                            
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900 CABA-RENSA-LAST SECTION.                                                 
083000                                                                          
083100     PERFORM IMS-LAS-RUTA-RUTA-FIRST                                      
083200     IF SEGMENT-SAKNAS                                                    
083300        PERFORM IMS-LAS-RUTA-LAST-FIRST-KVAL                              
083400        IF SEGMENT-FINNS                                                  
083500           PERFORM IMS-DELETE-RUTA                                        
083600           PERFORM IMS-LAS-RUTA-LAST-FIRST                                
083700           IF SEGMENT-SAKNAS                                              
083800              PERFORM IMS-LAS-RUTA-ROT                                    
083900              IF SEGMENT-FINNS                                            
084000                 PERFORM IMS-DELETE-RUTA                                  
084100              END-IF                                                      
084200           END-IF                                                         
084300        END-IF                                                            
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 CB-AENDRING SECTION.                                                     
084800                                                                          
084900     IF MID-TESPAERR(INDX) = ALL '+' AND                                  
085000        MID-VLRUTNIV(INDX) = ALL '+'                                      
085100        CONTINUE                                                          
085200     ELSE                                                                 
085310        IF ADFLOMR-WS > 700 AND < 900                                     
085400           PERFORM CBA-UPDATE-STAELL                                      
085500        ELSE                                                              
085600           PERFORM CBB-UPDATE-LAST                                        
085700        END-IF                                                            
085800     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100 CBA-UPDATE-STAELL SECTION.                                               
086200                                                                          
086300     PERFORM CBAA-UPDATE-VOLYM                                            
086400     IF MID-TESPAERR(INDX) = ALL '+'                                      
086500        CONTINUE                                                          
086600     ELSE                                                                 
086700        PERFORM CBAB-UPDATE-SPAERR                                        
086800     END-IF                                                               
086900     .                                                                    
087000     EJECT                                                                
087100 CBAA-UPDATE-VOLYM SECTION.                                               
087200                                                                          
087300     MOVE KEY-ADFLGEO  TO W-4417-ADFLGEO                                  
087400     MOVE WS-IDDC      TO W-4417-IDDC                                     
087500     PERFORM IMS-LAS-STAELL-ROT                                           
087600     IF SEGMENT-FINNS                                                     
087700        MOVE KEY-ADFLOMR  TO W-4418-ADFLOMR                               
087800        MOVE MID-ADRUTNIV(INDX) TO W-4418-ADRUTNIV                        
087900        MOVE MID-DIDMODUL(INDX) TO W-4418-DIDMODUL                        
088000        MOVE MID-DIHMODUL(INDX) TO W-4418-DIHMODUL                        
088100        PERFORM IMS-LAS-STAELL-AENDR-KVAL                                 
088200        IF SEGMENT-FINNS                                                  
088300           IF MID-VLRUTNIV(INDX) = ALL '+'                                
088400              CONTINUE                                                    
088500           ELSE                                                           
088600              MOVE MID-VLRUTNIV(INDX) TO PLATS-STAELL-VLRUTNIV            
088700              PERFORM IMS-REPLACE-STAELL                                  
088800              MOVE JA TO UPDATE-DONE                                      
088900           END-IF                                                         
089000           MOVE MID-ADVMODUL(INDX) TO W-4420-ADVMODUL                     
089100           PERFORM IMS-LAS-STAELL-SPAERR-KVAL                             
089200        END-IF                                                            
089300     END-IF                                                               
089400     .                                                                    
089500     EJECT                                                                
089600 CBAB-UPDATE-SPAERR SECTION.                                              
089700                                                                          
089800     IF SEGMENT-FINNS                                                     
089900        IF MID-TESPAERR (INDX) = SPACE                                    
090000           PERFORM CBABA-BORTTAG-SPAERR                                   
090100        ELSE                                                              
090200           MOVE MID-TESPAERR (INDX) TO PLATS-SPAERR-TESPAERR              
090300           PERFORM IMS-REPLACE-STAELL                                     
090400           MOVE JA TO UPDATE-DONE                                         
090500        END-IF                                                            
090600     ELSE                                                                 
090700        PERFORM CBABB-NYUPPLAEGG-SPAERR                                   
090800     END-IF                                                               
090900     .                                                                    
091000     EJECT                                                                
091100 CBABA-BORTTAG-SPAERR SECTION.                                            
091200                                                                          
091300     MOVE +5 TO PLATS-KDCALL                                              
091400     MOVE ZERO                TO PLATS-IDTRPTNR                           
091500     MOVE ZERO                TO PLATS-TIRFS                              
091600     MOVE WS-IDDC             TO PLATS-IDDC                               
091700     MOVE KEY-ADFLGEO         TO PLATS-ADFLGEO                            
091800     MOVE KEY-ADFLOMR         TO PLATS-ADFLOMR                            
091900     MOVE MID-ADRUTNIV(INDX)  TO PLATS-ADRUTNIV                           
092000     MOVE MID-DIHMODUL(INDX)  TO PLATS-DIHMODUL                           
092100     MOVE MID-DIDMODUL(INDX)  TO PLATS-DIDMODUL                           
092200     MOVE MID-ADVMODUL(INDX)  TO PLATS-ADVMODUL                           
092300     MOVE MID-ADHMODUL(INDX)  TO PLATS-ADHMODUL                           
092400                                                                          
092500     CALL W403PLAT USING PLATS-W403PLAT                                   
092600                         PLATS-DM-PCB                                     
092700                         PLATS-DN-PCB                                     
092800                         PLATS-DP-PCB                                     
092900                         PLATS-DO-PCB                                     
093000                         PLATS-WDE6C-PCB                                  
093100                         PLATS-GMTC-PCB                                   
093200                         PLATS-WDB6-PCB                                   
093300                                                                          
093400     MOVE JA TO UPDATE-DONE                                               
093500     IF PLATS-KDSVAR NOT = SPACE                                          
093600         MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                            
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 CBABB-NYUPPLAEGG-SPAERR SECTION.                                         
094100                                                                          
094200     MOVE +4                  TO PLATS-KDCALL                             
094300     MOVE WS-IDDC             TO PLATS-IDDC                               
094400     MOVE KEY-ADFLGEO         TO PLATS-ADFLGEO                            
094500     MOVE KEY-ADFLOMR         TO PLATS-ADFLOMR                            
094600     MOVE MID-ADRUTNIV(INDX)  TO PLATS-ADRUTNIV                           
094700     MOVE MID-DIHMODUL(INDX)  TO PLATS-DIHMODUL                           
094800     MOVE MID-DIDMODUL(INDX)  TO PLATS-DIDMODUL                           
094900     MOVE MID-ADVMODUL(INDX)  TO PLATS-ADVMODUL                           
095000     MOVE MID-ADHMODUL(INDX)  TO PLATS-ADHMODUL                           
095100     MOVE MID-TESPAERR(INDX)  TO PLATS-TESPAERR                           
095200     IF MID-VLRUTNIV(INDX) = ALL '+'                                      
095300        MOVE MID-VLRUTNIV-DOLT(INDX) TO PLATS-VLRUTNIV                    
095400     ELSE                                                                 
095500        MOVE MID-VLRUTNIV(INDX)  TO PLATS-VLRUTNIV                        
095600     END-IF                                                               
095700     CALL W403PLAT USING PLATS-W403PLAT                                   
095800                         PLATS-DM-PCB                                     
095900                         PLATS-DN-PCB                                     
096000                         PLATS-DP-PCB                                     
096100                         PLATS-DO-PCB                                     
096200                         PLATS-WDE6C-PCB                                  
096300                         PLATS-GMTC-PCB                                   
096400                         PLATS-WDB6-PCB                                   
096500                                                                          
096600     MOVE JA TO UPDATE-DONE                                               
096700     .                                                                    
096800     EJECT                                                                
096900 CBB-UPDATE-LAST SECTION.                                                 
097000                                                                          
097100     MOVE KEY-ADFLGEO  TO W-4411-ADFLGEO                                  
097200     MOVE WS-IDDC      TO W-4411-IDDC                                     
097300     PERFORM IMS-LAS-RUTA-ROT                                             
097400     IF SEGMENT-FINNS                                                     
097500        MOVE KEY-ADFLOMR  TO W-4412-ADFLOMR                               
097600        PERFORM IMS-LAS-RUTA-LASTOMR                                      
097700        IF SEGMENT-FINNS                                                  
097800           MOVE MID-ADRUTNIV(INDX) TO W-4414-ADRUTNIV                     
097900           PERFORM IMS-LAS-RUTA-RUTA-KVAL                                 
098000           IF SEGMENT-FINNS                                               
098100              IF MID-VLRUTNIV(INDX) = ALL '+'                             
098200                 MOVE MID-TESPAERR(INDX) TO PLATS-RUTA-TESPAERR           
098300                 PERFORM IMS-REPLACE-RUTA                                 
098400                 MOVE JA TO UPDATE-DONE                                   
098500              ELSE                                                        
098600                 MOVE MID-VLRUTNIV(INDX) TO PLATS-RUTA-VLRUTNIV           
098700                 IF MID-TESPAERR(INDX) = ALL '+'                          
098800                    CONTINUE                                              
098900                 ELSE                                                     
099000                    MOVE MID-TESPAERR(INDX) TO PLATS-RUTA-TESPAERR        
099100                 END-IF                                                   
099200                 PERFORM IMS-REPLACE-RUTA                                 
099300                 MOVE JA TO UPDATE-DONE                                   
099400              END-IF                                                      
099500           END-IF                                                         
099600        END-IF                                                            
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 CC-NYUPPLAEGG SECTION.                                                   
100100                                                                          
100200     MOVE +4                  TO PLATS-KDCALL                             
100300     MOVE WS-IDDC             TO PLATS-IDDC                               
100400     MOVE KEY-ADFLGEO         TO PLATS-ADFLGEO                            
100500     MOVE KEY-ADFLOMR         TO PLATS-ADFLOMR                            
100600     MOVE MID-ADRUTNIV-NY     TO PLATS-ADRUTNIV                           
100700     IF MID-DIHMODUL-NY = ALL '+'                                         
100800        MOVE ZERO             TO PLATS-DIHMODUL                           
100900     ELSE                                                                 
101000        MOVE MID-DIHMODUL-NY  TO PLATS-DIHMODUL                           
101100     END-IF                                                               
101200     IF MID-DIDMODUL-NY = ALL '+'                                         
101300        MOVE ZERO             TO PLATS-DIDMODUL                           
101400     ELSE                                                                 
101500        MOVE MID-DIDMODUL-NY  TO PLATS-DIDMODUL                           
101600     END-IF                                                               
101700     IF MID-ADVMODUL-NY = ALL '+'                                         
101800        MOVE ZERO             TO PLATS-ADVMODUL                           
101900     ELSE                                                                 
102000        MOVE MID-ADVMODUL-NY  TO PLATS-ADVMODUL                           
102100     END-IF                                                               
102200     IF MID-ADHMODUL-NY = ALL '+'                                         
102300        MOVE ZERO             TO PLATS-ADHMODUL                           
102400     ELSE                                                                 
102500        MOVE MID-ADHMODUL-NY  TO PLATS-ADHMODUL                           
102600     END-IF                                                               
102700     IF MID-TESPAERR-NY = ALL '+'                                         
102800        MOVE SPACE            TO PLATS-TESPAERR                           
102900     ELSE                                                                 
103000        MOVE MID-TESPAERR-NY  TO PLATS-TESPAERR                           
103100     END-IF                                                               
103200     IF MID-VLRUTNIV-NY = ALL '+'                                         
103300        MOVE ZERO             TO PLATS-VLRUTNIV                           
103400     ELSE                                                                 
103500        MOVE MID-VLRUTNIV-NY  TO PLATS-VLRUTNIV                           
103600     END-IF                                                               
103700                                                                          
103800     CALL W403PLAT USING PLATS-W403PLAT                                   
103900                         PLATS-DM-PCB                                     
104000                         PLATS-DN-PCB                                     
104100                         PLATS-DP-PCB                                     
104200                         PLATS-DO-PCB                                     
104300                         PLATS-WDE6C-PCB                                  
104400                         PLATS-GMTC-PCB                                   
104500                         PLATS-WDB6-PCB                                   
104600                                                                          
104700     MOVE JA TO UPDATE-DONE                                               
104800     .                                                                    
104900     EJECT                                                                
105000 D-LAES SECTION.                                                          
105100                                                                          
105200     EVALUATE TRUE                                                        
105300        WHEN MFS-IDPFK = '7'                                              
105400           MOVE ZERO                 TO KEY-ADRUTNIV                      
105500                                        KEY-DIDMODUL                      
105600                                        KEY-DIHMODUL                      
105700                                        MID-ADVMODUL-SISTA                
105800                                        MID-ADHMODUL-SISTA                
105900        WHEN MFS-IDPFK = '8'                                              
106000           MOVE MID-ADRUTNIV-NEXT    TO KEY-ADRUTNIV                      
106100           MOVE MID-DIDMODUL-NEXT    TO KEY-DIDMODUL                      
106200           MOVE MID-DIHMODUL-NEXT    TO KEY-DIHMODUL                      
106300        WHEN OTHER                                                        
106400           IF MID-ADFLGEO-IN  = ALL '+'   AND                             
106500              MID-ADFLOMR-IN  = ALL '+'   AND                             
106600              MID-ADRUTNIV-IN = ALL '+'                                   
106700             MOVE MID-ADRUTNIV-FIRST TO KEY-ADRUTNIV                      
106800             MOVE MID-DIDMODUL-FIRST TO KEY-DIDMODUL                      
106900             MOVE MID-DIHMODUL-FIRST TO KEY-DIHMODUL                      
107000             MOVE MID-ADVMODUL-FOERRA TO MID-ADVMODUL-SISTA               
107100                                         MOD-ADVMODUL-FOERRA              
107200             MOVE MID-ADHMODUL-FOERRA TO MID-ADHMODUL-SISTA               
107300                                         MOD-ADHMODUL-FOERRA              
107400           ELSE                                                           
107500             MOVE ZERO                TO MID-ADVMODUL-SISTA               
107600                                         MID-ADHMODUL-SISTA               
107700           END-IF                                                         
107800     END-EVALUATE                                                         
107900                                                                          
108000                                                                          
108100     MOVE +1 TO INDX                                                      
108200                                                                          
108310     IF KEY-ADFLOMR > +700 AND < +900                                     
108400        PERFORM DA-LAES-STAELL-INFO                                       
108500     ELSE                                                                 
108600        PERFORM DB-LAES-RUTA-INFO                                         
108700     END-IF                                                               
108800                                                                          
108900     IF INDX < MAX-IX-PLUS-1                                              
109000        PERFORM DC-BLANKA-REST-RADER                                      
109100     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400 DA-LAES-STAELL-INFO SECTION.                                             
109500                                                                          
109600     MOVE WS-IDDC      TO W-4417-IDDC                                     
109700     MOVE KEY-ADFLGEO  TO W-4417-ADFLGEO                                  
109800     PERFORM IMS-LAS-STAELL-ROT                                           
109900     IF SEGMENT-FINNS                                                     
110000        PERFORM DAA-LAES-OKVAL-NIVA                                       
110100     ELSE                                                                 
110200        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
110300     END-IF                                                               
110400     .                                                                    
110500     EJECT                                                                
110600 DAA-LAES-OKVAL-NIVA SECTION.                                             
110700                                                                          
110800     MOVE KEY-ADFLOMR  TO W-LOW-4418-ADFLOMR                              
110900                          W-HIGH-4418-ADFLOMR                             
111000                          W-4418-ADFLOMR                                  
111100     MOVE KEY-ADRUTNIV TO W-LOW-4418-ADRUTNIV                             
111200     MOVE KEY-DIDMODUL TO W-LOW-4418-DIDMODUL                             
111300     MOVE KEY-DIHMODUL TO W-LOW-4418-DIHMODUL                             
111400                                                                          
111500     PERFORM IMS-LAS-STAELL-NIVA-OKVAL                                    
111600                                                                          
111700     IF SEGMENT-SAKNAS                                                    
111800        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
111900     ELSE                                                                 
112000       MOVE PLATS-STAELL-ADRUTNIV TO MOD-ADRUTNIV-FIRST                   
112100       MOVE PLATS-STAELL-DIDMODUL TO MOD-DIDMODUL-FIRST                   
112200       MOVE PLATS-STAELL-DIHMODUL TO MOD-DIHMODUL-FIRST                   
112300       MOVE +1 TO INDX                                                    
112400       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
112500             INDX > MAX-IX                                                
112600                                                                          
112700        MOVE PLATS-STAELL-ADRUTNIV TO SPAR-ADRUTNIV                       
112800                                    W-4418-ADRUTNIV                       
112900        MOVE PLATS-STAELL-DIHMODUL TO SPAR-DIHMODUL                       
113000                                    W-4418-DIHMODUL                       
113100        MOVE PLATS-STAELL-DIDMODUL TO SPAR-DIDMODUL                       
113200                                    W-4418-DIDMODUL                       
113300        MOVE PLATS-STAELL-VLRUTNIV TO SPAR-VLRUTNIV                       
113400                                                                          
113500        MOVE MID-ADHMODUL-SISTA TO SPAR-ADHMODUL                          
113600        PERFORM IMS-LAS-STAELL-SPAERR-OKVAL                               
113700                                                                          
113800        PERFORM UNTIL SEGMENT-SAKNAS OR PLATS-SPAERR-ADVMODUL             
113900        > MID-ADVMODUL-SISTA                                              
114000          PERFORM IMS-LAS-STAELL-SPAERR-OKVAL                             
114100        END-PERFORM                                                       
114200                                                                          
114300        IF SEGMENT-FINNS                                                  
114400          PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-IX                   
114500            IF PLATS-SPAERR-ADVMODUL > SPAR-ADHMODUL + 1                  
114600              ADD 1 SPAR-ADHMODUL GIVING MOD-ADVMODUL(INDX)               
114700              SUBTRACT 1 FROM PLATS-SPAERR-ADVMODUL GIVING                
114800                              MOD-ADHMODUL(INDX)                          
114900                              SPAR-ADHMODUL                               
115000              MOVE SPACE TO MOD-TESPAERR(INDX)                            
115100              PERFORM DAAA-FLYTTA-SPAR                                    
115200              ADD +1 TO INDX                                              
115300            ELSE                                                          
115400              MOVE PLATS-SPAERR-TESPAERR TO WS-SLUT-TESPAERR              
115500              IF SLUT-TESPAERR NOT = WS-STAELL-SLUT                       
115600                MOVE PLATS-SPAERR-ADVMODUL TO MOD-ADVMODUL(INDX)          
115700                MOVE PLATS-SPAERR-ADHMODUL TO MOD-ADHMODUL(INDX)          
115800                                            SPAR-ADHMODUL                 
115900                MOVE PLATS-SPAERR-TESPAERR TO MOD-TESPAERR(INDX)          
116000                PERFORM DAAA-FLYTTA-SPAR                                  
116100                ADD +1 TO INDX                                            
116200              END-IF                                                      
116300              PERFORM IMS-LAS-STAELL-SPAERR-OKVAL                         
116400            END-IF                                                        
116500          END-PERFORM                                                     
116600        ELSE                                                              
116700          COMPUTE MOD-ADVMODUL(INDX) = SPAR-ADHMODUL + 1                  
116800          MOVE 400 TO MOD-ADHMODUL(INDX)                                  
116900          MOVE SPACE TO MOD-TESPAERR(INDX)                                
117000        END-IF                                                            
117100        IF INDX = MAX-IX-PLUS-1                                           
117200          MOVE +13 TO INDX                                                
117300          MOVE MOD-ADVMODUL(INDX) TO FIXA-ADVMODUL                        
117400          MOVE MOD-ADHMODUL(INDX) TO FIXA-ADHMODUL                        
117500          INSPECT FIXA-ADVMODUL REPLACING LEADING SPACE BY ZERO           
117600          INSPECT FIXA-ADHMODUL REPLACING LEADING SPACE BY ZERO           
117700          MOVE FIXA-ADVMODUL      TO MOD-ADVMODUL-SISTA                   
117800          MOVE FIXA-ADHMODUL      TO MOD-ADHMODUL-SISTA                   
117900          MOVE MED-2(SPRAK-IX) TO MOD-TEMFSINF                            
118000          MOVE SPAR-ADRUTNIV TO MOD-ADRUTNIV-NEXT                         
118100          MOVE SPAR-DIDMODUL TO MOD-DIDMODUL-NEXT                         
118200          MOVE SPAR-DIHMODUL TO MOD-DIHMODUL-NEXT                         
118300          MOVE MAX-IX-PLUS-1 TO INDX                                      
118400        ELSE                                                              
118500          MOVE ZERO TO MID-ADVMODUL-SISTA                                 
118600                       MID-ADHMODUL-SISTA                                 
118700          PERFORM IMS-LAS-STAELL-NIVA-OKVAL                               
118800        END-IF                                                            
118900     END-PERFORM                                                          
119000                                                                          
119100     IF INDX = MAX-IX-PLUS-1                                              
119200        MOVE MED-2(SPRAK-IX) TO MOD-TEMFSINF                              
119300        MOVE SPAR-ADRUTNIV   TO MOD-ADRUTNIV-NEXT                         
119400        MOVE SPAR-DIDMODUL   TO MOD-DIDMODUL-NEXT                         
119500        MOVE SPAR-DIHMODUL   TO MOD-DIHMODUL-NEXT                         
119600     ELSE                                                                 
119700        IF SEGMENT-FINNS                                                  
119800           MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                          
119900           MOVE PLATS-STAELL-ADRUTNIV TO MOD-ADRUTNIV-NEXT                
120000           MOVE PLATS-STAELL-DIDMODUL TO MOD-DIDMODUL-NEXT                
120100           MOVE PLATS-STAELL-DIHMODUL TO MOD-DIHMODUL-NEXT                
120200        ELSE                                                              
120300           MOVE ZERO             TO MOD-ADRUTNIV-NEXT                     
120400                                    MOD-DIDMODUL-NEXT                     
120500                                    MOD-DIHMODUL-NEXT                     
120600                                    MOD-ADVMODUL-SISTA                    
120700                                    MOD-ADHMODUL-SISTA                    
120800        END-IF                                                            
120900     END-IF                                                               
121000     END-IF                                                               
121100     .                                                                    
121200     EJECT                                                                
121300 DAAA-FLYTTA-SPAR SECTION.                                                
121400                                                                          
121500     MOVE SPAR-ADRUTNIV  TO MOD-ADRUTNIV(INDX)                            
121600     MOVE SPAR-DIDMODUL  TO MOD-DIDMODUL(INDX)                            
121700     MOVE SPAR-DIHMODUL  TO MOD-DIHMODUL(INDX)                            
121800     IF SPAR-VLRUTNIV = ZERO                                              
121900        MOVE MFS-RENSA-FAELT TO MOD-VLRUTNIV(INDX)                        
122000     ELSE                                                                 
122100        MOVE SPAR-VLRUTNIV   TO MOD-VLRUTNIV(INDX)                        
122200     END-IF                                                               
122300     MOVE MFS-FORMATETS-ATTR TO MOD-VLRUTNIV-ATTR(INDX)                   
122400     MOVE SPAR-VLRUTNIV  TO MOD-VLRUTNIV-DOLT(INDX)                       
122500     EJECT                                                                
122600     .                                                                    
122700     EJECT                                                                
122800 DB-LAES-RUTA-INFO SECTION.                                               
122900                                                                          
123000     MOVE +1 TO INDX                                                      
123100     MOVE WS-IDDC      TO W-4411-IDDC                                     
123200     MOVE KEY-ADFLGEO  TO W-4411-ADFLGEO                                  
123300     PERFORM IMS-LAS-RUTA-ROT                                             
123400     IF SEGMENT-FINNS                                                     
123500        MOVE KEY-ADFLOMR  TO W-4412-ADFLOMR                               
123600        MOVE KEY-ADRUTNIV TO W-4414-ADRUTNIV                              
123700        PERFORM IMS-LAS-RUTA-LASTOMR                                      
123800        IF SEGMENT-FINNS                                                  
123900           PERFORM DBA-LAES-OKVAL-RUTA                                    
124000        ELSE                                                              
124100           MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                           
124200           MOVE ZERO            TO MOD-ADRUTNIV-NEXT                      
124300        END-IF                                                            
124400     ELSE                                                                 
124500        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
124600        MOVE ZERO               TO MOD-ADRUTNIV-FIRST                     
124700                                   MOD-ADRUTNIV-NEXT                      
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 DBA-LAES-OKVAL-RUTA SECTION.                                             
125200                                                                          
125300     MOVE ZERO                 TO MOD-ADRUTNIV-FIRST                      
125400                                                                          
125500     PERFORM IMS-LAS-RUTA-RUTA-OKVAL                                      
125600     IF SEGMENT-FINNS                                                     
125700       MOVE PLATS-RUTA-ADRUTNIV TO MOD-ADRUTNIV-FIRST                     
125800       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
125900                   INDX > MAX-IX                                          
126000                                                                          
126100        MOVE PLATS-RUTA-ADRUTNIV TO MOD-ADRUTNIV(INDX)                    
126200        MOVE ZERO              TO MOD-DIHMODUL(INDX)                      
126300                                  MOD-DIDMODUL(INDX)                      
126400                                  MOD-ADVMODUL(INDX)                      
126500                                  MOD-ADHMODUL(INDX)                      
126600        IF PLATS-RUTA-VLRUTNIV = ZERO                                     
126700           MOVE MFS-RENSA-FAELT   TO MOD-VLRUTNIV(INDX)                   
126800        ELSE                                                              
126900           MOVE PLATS-RUTA-VLRUTNIV TO MOD-VLRUTNIV(INDX)                 
127000        END-IF                                                            
127100        MOVE MFS-FORMATETS-ATTR   TO MOD-VLRUTNIV-ATTR(INDX)              
127200        MOVE PLATS-RUTA-VLRUTNIV  TO MOD-VLRUTNIV-DOLT(INDX)              
127300        MOVE PLATS-RUTA-TESPAERR TO MOD-TESPAERR(INDX)                    
127400                                  W-RUTA-TESPAERR                         
127500        IF W-TRPT = 'TRPT'                                                
127600           MOVE MFS-STAENG-FAELT TO MOD-TESPAERR-ATTR(INDX)               
127700           MOVE JA               TO MOD-FLAGGA-TRPT-DOLD(INDX)            
127800        ELSE                                                              
127900           MOVE NEJ              TO MOD-FLAGGA-TRPT-DOLD(INDX)            
128000        END-IF                                                            
128100        ADD +1 TO INDX                                                    
128200        PERFORM IMS-LAS-RUTA-RUTA-OKVAL                                   
128300       END-PERFORM                                                        
128400                                                                          
128500       IF SEGMENT-FINNS                                                   
128600          MOVE MED-2 (SPRAK-IX)  TO MOD-TEMFSINF                          
128700          MOVE PLATS-RUTA-ADRUTNIV TO MOD-ADRUTNIV-NEXT                   
128800       ELSE                                                               
128900          MOVE ZERO              TO MOD-ADRUTNIV-NEXT                     
129000       END-IF                                                             
129100     ELSE                                                                 
129200       MOVE ZERO               TO MOD-ADRUTNIV-NEXT                       
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600 DC-BLANKA-REST-RADER SECTION.                                            
129700                                                                          
129800     PERFORM UNTIL INDX > MAX-IX                                          
129900        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                     
130000                                 MOD-VLRUTNIV-ATTR(INDX)                  
130100                                 MOD-TESPAERR-ATTR(INDX)                  
130200        MOVE MFS-RENSA-FAELT TO MOD-KDCMD(INDX)                           
130300                                MOD-ADRUTNIV(INDX)                        
130400                                MOD-DIDMODUL(INDX)                        
130500                                MOD-DIHMODUL(INDX)                        
130600                                MOD-ADVMODUL(INDX)                        
130700                                MOD-ADHMODUL(INDX)                        
130800                                MOD-VLRUTNIV(INDX)                        
130900                                MOD-TESPAERR(INDX)                        
131000                                MOD-VLRUTNIV-DOLT(INDX)                   
131100        ADD +1 TO INDX                                                    
131200     END-PERFORM                                                          
131300     .                                                                    
131400     EJECT                                                                
131500 E-KOLLA-OM-MAN-MENAT-PF11 SECTION.                                       
131600                                                                          
131700     MOVE NEJ TO PF11-TEST                                                
131800     MOVE +1 TO INDX                                                      
131900     PERFORM UNTIL INDX > MAX-IX                                          
132000        IF MID-KDCMD(INDX) = 'Ä' OR 'B'                                   
132100           MOVE JA TO PF11-TEST                                           
132200        END-IF                                                            
132300        ADD +1 TO INDX                                                    
132400     END-PERFORM                                                          
132500                                                                          
132600     IF PF11-TEST = NEJ                                                   
132700        IF MID-ADRUTNIV-NY NOT = ALL '+'                                  
132800           MOVE JA TO PF11-TEST                                           
132900        END-IF                                                            
133000     END-IF                                                               
133100     .                                                                    
133200     EJECT                                                                
133300 F-RENSA-NYCKLAR SECTION.                                                 
133400     MOVE MFS-RENSA-FAELT            TO MOD-ADFLGEO-UT                    
133500                                        MOD-ADFLOMR-UT                    
133600                                        MOD-ADRUTNIV-UT                   
133700                                        MOD-IDDC-UT                       
133800     .                                                                    
133900     EJECT                                                                
134000 MFS-GRUND-FORMAT SECTION.                                                
134100                                                                          
134200     MOVE +1 TO INDX                                                      
134300     PERFORM UNTIL INDX > MAX-IX                                          
134400        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR(INDX)                   
134500                                   MOD-VLRUTNIV-ATTR(INDX)                
134600        ADD +1 TO INDX                                                    
134700     END-PERFORM                                                          
134800     MOVE MFS-FORMATETS-ATTR    TO MOD-ADRUTNIV-NY-ATTR                   
134900                                   MOD-DIDMODUL-NY-ATTR                   
135000                                   MOD-DIHMODUL-NY-ATTR                   
135100                                   MOD-ADVMODUL-NY-ATTR                   
135200                                   MOD-ADHMODUL-NY-ATTR                   
135300                                   MOD-VLRUTNIV-NY-ATTR                   
135400                                   MOD-TESPAERR-NY-ATTR                   
135500     .                                                                    
135600     EJECT                                                                
135700 MFS-GRUND-FORMAT-NY SECTION.                                             
135800                                                                          
135900     MOVE MFS-FORMATETS-ATTR    TO MOD-ADRUTNIV-NY-ATTR                   
136000                                   MOD-DIDMODUL-NY-ATTR                   
136100                                   MOD-DIHMODUL-NY-ATTR                   
136200                                   MOD-ADVMODUL-NY-ATTR                   
136300                                   MOD-ADHMODUL-NY-ATTR                   
136400                                   MOD-VLRUTNIV-NY-ATTR                   
136500                                   MOD-TESPAERR-NY-ATTR                   
136600     .                                                                    
136700     EJECT                                                                
136800 MFS-RENSA-BILD SECTION.                                                  
136900                                                                          
137000     MOVE +1 TO INDX                                                      
137100     PERFORM UNTIL INDX > MAX-IX                                          
137200        MOVE MFS-RENSA-FAELT TO MOD-KDCMD(INDX)                           
137300                                MOD-ADRUTNIV(INDX)                        
137400                                MOD-DIDMODUL(INDX)                        
137500                                MOD-DIHMODUL(INDX)                        
137600                                MOD-ADVMODUL(INDX)                        
137700                                MOD-ADHMODUL(INDX)                        
137800                                MOD-VLRUTNIV(INDX)                        
137900                                MOD-TESPAERR(INDX)                        
138000                                MOD-VLRUTNIV-DOLT(INDX)                   
138100        ADD +1 TO INDX                                                    
138200     END-PERFORM                                                          
138300     .                                                                    
138400     EJECT                                                                
138500 MFS-RENSA-BILD-NY SECTION.                                               
138600                                                                          
138700     MOVE MFS-RENSA-FAELT    TO MOD-ADRUTNIV-NY                           
138800                                MOD-DIDMODUL-NY                           
138900                                MOD-DIHMODUL-NY                           
139000                                MOD-ADVMODUL-NY                           
139100                                MOD-ADHMODUL-NY                           
139200                                MOD-VLRUTNIV-NY                           
139300                                MOD-TESPAERR-NY                           
139400     .                                                                    
139500     EJECT                                                                
139600 MFS-ROER-EJ-BILD SECTION.                                                
139700                                                                          
139800     MOVE +1 TO INDX                                                      
139900                                                                          
140000       MOVE MFS-ROER-EJ-FAELT TO MOD-ADRUTNIV-FIRST                       
140100                                 MOD-ADRUTNIV-NEXT                        
140200                                 MOD-DIDMODUL-FIRST                       
140300                                 MOD-DIDMODUL-NEXT                        
140400                                 MOD-DIHMODUL-FIRST                       
140500                                 MOD-DIHMODUL-NEXT                        
140600                                 MOD-ADVMODUL-SISTA                       
140700                                 MOD-ADHMODUL-SISTA                       
140800                                 MOD-ADVMODUL-FOERRA                      
140900                                 MOD-ADHMODUL-FOERRA                      
141000                                                                          
141100     PERFORM UNTIL INDX > MAX-IX                                          
141200                                                                          
141300       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD(INDX)                         
141400                                  MOD-ADRUTNIV(INDX)                      
141500                                  MOD-DIDMODUL(INDX)                      
141600                                  MOD-DIHMODUL(INDX)                      
141700                                  MOD-ADVMODUL(INDX)                      
141800                                  MOD-ADHMODUL(INDX)                      
141900                                  MOD-VLRUTNIV(INDX)                      
142000                                  MOD-TESPAERR(INDX)                      
142100                                  MOD-VLRUTNIV-DOLT(INDX)                 
142200       ADD +1 TO INDX                                                     
142300     END-PERFORM                                                          
142400                                                                          
142500     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADRUTNIV-NY                           
142600                                MOD-DIDMODUL-NY                           
142700                                MOD-DIHMODUL-NY                           
142800                                MOD-ADVMODUL-NY                           
142900                                MOD-ADHMODUL-NY                           
143000                                MOD-VLRUTNIV-NY                           
143100                                MOD-TESPAERR-NY                           
143200     .                                                                    
143300     EJECT                                                                
143400 MFS-SAETT-LAES-IGEN-ATTR SECTION.                                        
143500                                                                          
143600     MOVE +1 TO INDX                                                      
143700                                                                          
143800     PERFORM UNTIL INDX > MAX-IX                                          
143900                                                                          
144000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)                 
144100                                     MOD-VLRUTNIV-ATTR(INDX)              
144200                                     MOD-TESPAERR-ATTR(INDX)              
144300       ADD +1 TO INDX                                                     
144400     END-PERFORM                                                          
144500                                                                          
144600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADRUTNIV-NY-ATTR                   
144700                                   MOD-DIDMODUL-NY-ATTR                   
144800                                   MOD-DIHMODUL-NY-ATTR                   
144900                                   MOD-ADVMODUL-NY-ATTR                   
145000                                   MOD-ADHMODUL-NY-ATTR                   
145100                                   MOD-VLRUTNIV-NY-ATTR                   
145200                                   MOD-TESPAERR-NY-ATTR                   
145300     SKIP3                                                                
145400     .                                                                    
145500 MFS-SAETT-LAES-IGEN-ATTR-NY SECTION.                                     
145600                                                                          
145700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADRUTNIV-NY-ATTR                   
145800                                   MOD-DIDMODUL-NY-ATTR                   
145900                                   MOD-DIHMODUL-NY-ATTR                   
146000                                   MOD-ADVMODUL-NY-ATTR                   
146100                                   MOD-ADHMODUL-NY-ATTR                   
146200                                   MOD-VLRUTNIV-NY-ATTR                   
146300                                   MOD-TESPAERR-NY-ATTR                   
146400     .                                                                    
146500     EJECT                                                                
146600* IMS SEKTIONER                                                           
146700     SKIP3                                                                
146800 IMS-GET-MSG SECTION.                                                     
146900                                                                          
147000     MOVE '  QC' TO GODK-STATUSKODER                                      
147100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
147200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147300     PERFORM IMS-STATUSKONTROLL                                           
147400     SKIP3                                                                
147500     .                                                                    
147600 IMS-INSERT-MSG SECTION.                                                  
147700                                                                          
147800     IF NOT ENGLISH-TEXT                                                  
147900       MOVE '0' TO MFS-KDHUVOMR                                           
148000     END-IF                                                               
148100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
148200     MOVE SPACE TO GODK-STATUSKODER                                       
148300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
148400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800 IMS-LAS-RUTA-ROT SECTION.                                                
148900                                                                          
149000     STRING 'WLXXDO01(WDGXKEY  =' W-4411-WDGXKEY-X ')'                    
149100            DELIMITED BY SIZE INTO SSA1                                   
149200     MOVE '  GE' TO GODK-STATUSKODER                                      
149300     CALL CBLTDLI USING GHU PLATS-DO-PCB DLI-IO-AREA SSA1                 
149400     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     SKIP3                                                                
149700     .                                                                    
149800 IMS-LAS-RUTA-LASTOMR SECTION.                                            
149900                                                                          
150000     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
150100            DELIMITED BY SIZE INTO SSA1                                   
150200     MOVE '  GE' TO GODK-STATUSKODER                                      
150300     CALL CBLTDLI USING GNP PLATS-DO-PCB DLI-IO-AREA SSA1                 
150400     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
150500     PERFORM IMS-STATUSKONTROLL                                           
150600     SKIP3                                                                
150700     .                                                                    
150800 IMS-LAS-RUTA-LAST-FIRST SECTION.                                         
150900                                                                          
151000     MOVE 'WLXXDO11*F' TO SSA1                                            
151100     MOVE '  GE' TO GODK-STATUSKODER                                      
151200     CALL CBLTDLI USING GNP PLATS-DO-PCB DLI-IO-AREA SSA1                 
151300     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
151400     PERFORM IMS-STATUSKONTROLL                                           
151500     .                                                                    
151600     EJECT                                                                
151700 IMS-LAS-RUTA-LAST-FIRST-KVAL SECTION.                                    
151800                                                                          
151900     STRING 'WLXXDO11*F(WDGXKEY  =' W-4412-WDGXKEY-X ')'                  
152000            DELIMITED BY SIZE INTO SSA1                                   
152100     MOVE '  GE' TO GODK-STATUSKODER                                      
152200     CALL CBLTDLI USING GHNP PLATS-DO-PCB DLI-IO-AREA SSA1                
152300     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
152400     PERFORM IMS-STATUSKONTROLL                                           
152500     .                                                                    
152600     EJECT                                                                
152700 IMS-LAS-RUTA-RUTA-KVAL SECTION.                                          
152800                                                                          
152900     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
153000            DELIMITED BY SIZE INTO SSA1                                   
153100     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
153200            DELIMITED BY SIZE INTO SSA2                                   
153300     MOVE '  GE' TO GODK-STATUSKODER                                      
153400     CALL CBLTDLI USING GHNP PLATS-DO-PCB                                 
153500           DLI-IO-AREA SSA1 SSA2                                          
153600     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
153700     PERFORM IMS-STATUSKONTROLL                                           
153800     SKIP3                                                                
153900     .                                                                    
154000 IMS-LAS-RUTA-RUTA-OKVAL SECTION.                                         
154100                                                                          
154200     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
154300            DELIMITED BY SIZE INTO SSA1                                   
154400     STRING 'WLXXDO21(WDGXKEY >=' W-4414-WDGXKEY-X ')'                    
154500            DELIMITED BY SIZE INTO SSA2                                   
154600     MOVE '  GE' TO GODK-STATUSKODER                                      
154700     CALL CBLTDLI USING GHNP PLATS-DO-PCB                                 
154800           DLI-IO-AREA SSA1 SSA2                                          
154900     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
155000     PERFORM IMS-STATUSKONTROLL                                           
155100     SKIP3                                                                
155200     .                                                                    
155300 IMS-LAS-RUTA-RUTA-FIRST SECTION.                                         
155400                                                                          
155500     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
155600            DELIMITED BY SIZE INTO SSA1                                   
155700     MOVE 'WLXXDO21*F' TO SSA2                                            
155800     MOVE '  GE' TO GODK-STATUSKODER                                      
155900     CALL CBLTDLI USING GNP PLATS-DO-PCB                                  
156000           DLI-IO-AREA SSA1 SSA2                                          
156100     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-REPLACE-RUTA SECTION.                                                
156600                                                                          
156700     MOVE '  ' TO GODK-STATUSKODER                                        
156800     CALL CBLTDLI USING REPL PLATS-DO-PCB DLI-IO-AREA                     
156900     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     SKIP3                                                                
157200     .                                                                    
157300 IMS-DELETE-RUTA SECTION.                                                 
157400                                                                          
157500     MOVE '  ' TO GODK-STATUSKODER                                        
157600     CALL CBLTDLI USING DLET PLATS-DO-PCB DLI-IO-AREA                     
157700     MOVE PLATS-DO-STATUS-CODE TO STATUS-WS                               
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     EJECT                                                                
158100 IMS-LAS-STAELL-ROT SECTION.                                              
158200                                                                          
158300     STRING 'WLXXDP01(WDGXKEY  =' W-4417-WDGXKEY-X ')'                    
158400            DELIMITED BY SIZE INTO SSA1                                   
158500     MOVE '  GE' TO GODK-STATUSKODER                                      
158600     CALL CBLTDLI USING GHU PLATS-DP-PCB DLI-IO-AREA SSA1                 
158700     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
158800     PERFORM IMS-STATUSKONTROLL                                           
158900     SKIP3                                                                
159000     .                                                                    
159100 IMS-LAS-STAELL-AENDR-KVAL SECTION.                                       
159200                                                                          
159300     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
159400            DELIMITED BY SIZE INTO SSA1                                   
159500     MOVE '  GE' TO GODK-STATUSKODER                                      
159600     CALL CBLTDLI USING GHNP PLATS-DP-PCB DLI-IO-AREA SSA1                
159700     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
159800     PERFORM IMS-STATUSKONTROLL                                           
159900     .                                                                    
160000     EJECT                                                                
160100 IMS-LAS-STAELL-NIVA-OKVAL SECTION.                                       
160200                                                                          
160300     STRING 'WLXXDP11(WDGXKEY >=' W-4418-WDGXKEY-X-LOW                    
160400     '&WDGXKEY =<' W-4418-WDGXKEY-X-HIGH ')'                              
160500            DELIMITED BY SIZE INTO SSA1                                   
160600     MOVE '  GE' TO GODK-STATUSKODER                                      
160700     CALL CBLTDLI USING GHNP PLATS-DP-PCB DLI-IO-AREA SSA1                
160800     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
160900     PERFORM IMS-STATUSKONTROLL                                           
161000     SKIP3                                                                
161100     .                                                                    
161200 IMS-LAS-STAELL-NIVA-FIRST SECTION.                                       
161300                                                                          
161400     MOVE 'WLXXDP11*F' TO SSA1                                            
161500     MOVE '  GE' TO GODK-STATUSKODER                                      
161600     CALL CBLTDLI USING GNP PLATS-DP-PCB DLI-IO-AREA SSA1                 
161700     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
161800     PERFORM IMS-STATUSKONTROLL                                           
161900     SKIP3                                                                
162000     .                                                                    
162100 IMS-LAS-STAELL-SPAERR-KVAL SECTION.                                      
162200                                                                          
162300     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
162400            DELIMITED BY SIZE INTO SSA1                                   
162500     STRING 'WLXXDP21(WDGXKEY  =' W-4420-WDGXKEY-X ')'                    
162600            DELIMITED BY SIZE INTO SSA2                                   
162700     MOVE '  GE' TO GODK-STATUSKODER                                      
162800     CALL CBLTDLI USING GHNP PLATS-DP-PCB                                 
162900           DLI-IO-AREA SSA1 SSA2                                          
163000     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
163100     PERFORM IMS-STATUSKONTROLL                                           
163200     .                                                                    
163300     EJECT                                                                
163400 IMS-LAS-STAELL-SPAERR-OKVAL SECTION.                                     
163500                                                                          
163600     STRING 'WLXXDP11(WDGXKEY  =' W-4418-WDGXKEY-X ')'                    
163700            DELIMITED BY SIZE INTO SSA1                                   
163800     MOVE 'WLXXDP21 ' TO SSA2                                             
163900     MOVE '  GE' TO GODK-STATUSKODER                                      
164000     CALL CBLTDLI USING GHNP PLATS-DP-PCB                                 
164100           DLI-IO-AREA SSA1 SSA2                                          
164200     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     SKIP3                                                                
164500     .                                                                    
164600 IMS-REPLACE-STAELL SECTION.                                              
164700                                                                          
164800     MOVE '  ' TO GODK-STATUSKODER                                        
164900     CALL CBLTDLI USING REPL PLATS-DP-PCB DLI-IO-AREA                     
165000     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
165100     PERFORM IMS-STATUSKONTROLL                                           
165200     SKIP3                                                                
165300     .                                                                    
165400 IMS-DELETE-STAELL SECTION.                                               
165500                                                                          
165600     MOVE '  ' TO GODK-STATUSKODER                                        
165700     CALL CBLTDLI USING DLET PLATS-DP-PCB DLI-IO-AREA                     
165800     MOVE PLATS-DP-STATUS-CODE TO STATUS-WS                               
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     EJECT                                                                
166200 IMS-LAS-KOLLI-SEK-RUTA SECTION.                                          
166300     SKIP2                                                                
166400     STRING 'WDE6C1  (WDE6C1KY>=' W-SEK-WDE6C1KY-X-MIN                    
166500            '&WDE6C1KY<=' W-SEK-WDE6C1KY-X-MAX                            
166600            '&ADCLGEO  =' W-SEK-ADCLGEO-X                                 
166700            '&ADFLOMR  =' W-SEK-ADFLOMR-X                                 
166800            '&ADRUTNIV =' W-SEK-ADRUTNIV-X ')'                            
166900            DELIMITED BY SIZE INTO SSA1                                   
167000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
167100     CALL CBLTDLI USING GU                                                
167200     PLATS-WDE6C-PCB DLI-IO-AREA SSA1                                     
167300     MOVE PLATS-WDE6C-STATUS-CODE TO STATUS-WS                            
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-STATUSKONTROLL SECTION.                                              
167800                                                                          
167900     SET STATUS-IX TO 1                                                   
168000     SEARCH GODK-STATUS AT END CALL FELLOG                                
168100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
168200     END-SEARCH                                                           
168300     CONTINUE                                                             
168400     .                                                                    
