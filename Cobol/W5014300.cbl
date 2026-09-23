000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5014300.                                                
000400 AUTHOR.         ANN WESTBERG.                                            
000500 DATE-WRITTEN.   AUG   89.                                                
000510 DATE-COMPILED.                                                           
000800                                                                          
000900*   FUNKTION.                                                             
001000*       ETT PROGRAM SOM SÖKER/ÄNDRAR/TAR BORT/UPPDATERAR                  
001100*       DETALJKOD OCH DESS EMBALLAGEKOSTNAD.                              
001300*                                                                         
001310*       PROGRAMMET ÄR KOPIERAT FRÅN OCH SKALL ERSÄTTA W4011400            
001320*       24 MAJ 1995 AV KJH.                                               
001400*                                                                         
001500*   INDATA.                                                               
001600*       TRANSAKTION: W5T143                                               
001700*       MID:         W5I14301                                             
001800*                                                                         
001900*   UTDATA.                                                               
002000*       MOD:         W5O14301                                             
002100   SKIP3                                                                  
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77    IDPGM                     PIC X(8)    VALUE 'W5014300'.            
002900 77    JA                        PIC X(1)    VALUE 'J'.                   
003000 77    NEJ                       PIC X(1)    VALUE 'N'.                   
003100 77    UPPDATERING               PIC X(1)    VALUE 'N'.                   
003200 77    PRIS-OK                   PIC X(1)    VALUE 'N'.                   
003300 77    IDARTNR-IFYLLD            PIC X(1)    VALUE 'N'.                   
003400 77    PRDMTRL-IFYLLD            PIC X(1)    VALUE 'N'.                   
003500 77    KDCMD-IFYLLD              PIC X(1)    VALUE 'N'.                   
003600 77    INGENTING-IFYLLT          PIC X(1)    VALUE 'N'.                   
003700 77    FEL-FINNS1                PIC X(1)    VALUE 'N'.                   
003800 77    FEL-FINNS2                PIC X(1)    VALUE 'N'.                   
003900 77    FEL-FINNS3                PIC X(1)    VALUE 'N'.                   
004000 77    KDCMD-OK                  PIC X(1)    VALUE 'N'.                   
004100 77    IDARTNR-OK                PIC X(1)    VALUE 'N'.                   
004200 77    PRDMTRL-OK                PIC X(1)    VALUE 'N'.                   
004300 77    NAESTA-DETALJKOD          PIC S9(9)   VALUE ZERO.                  
004400 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +441 COMP SYNC.        
004700     EJECT                                                                
004800                                                                          
004900 01    SUB-PGM.                                                           
005000   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005210   03    WMEDAREA                PIC X(8)    VALUE 'WMEDAREA'.            
005220   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005300    SKIP2                                                                 
005400                                                                          
005500 01    WS-IDTRANS                PIC X(4).                                
005600     88    EGEN-BILD             VALUE '5143'.                            
006100     EJECT                                                                
006200****************** HJÄLP-FÄLT ************************************        
006300                                                                          
006400 01    DETALJKOD.                                                         
006500   03    WS-IDARTNR-ALPA         PIC X(9)           VALUE SPACE.          
006600   03    WS-IDARTNR-NUM          PIC S9(9)          VALUE ZERO.           
006700   SKIP3                                                                  
006800 01    DETALJKOD-NYUPPLAEGG.                                              
006900   03    WS-IDARTNR-ALPA-NY      PIC X(9)           VALUE SPACE.          
007000   03    WS-IDARTNR-NUM-NY       PIC S9(9)          VALUE ZERO.           
007100   SKIP3                                                                  
007200 01    HJAELP-FAELT.                                                      
007300   03    WS-IDARTNR-HJAELP       PIC S9(9)          VALUE ZERO.           
007400   SKIP3                                                                  
007500 01    KOSTNAD-MATRIAL.                                                   
007600   03    WS-PRDMTRL              PIC S9(6)V9(3)     VALUE ZERO.           
007700   SKIP3                                                                  
007800                                                                          
007900*********************   SWITCHAR   *******************************        
008000                                                                          
008100 01    NYCKEL-OK                 PIC X(1)    VALUE SPACE.                 
008200     SKIP2                                                                
008300 01    NYA-NYCKLAR               PIC X(1)    VALUE SPACE.                 
008400     SKIP2                                                                
008500 01    INDATA-OK                 PIC X(1)    VALUE SPACE.                 
008600     SKIP2                                                                
008700 01    NYUPPLAEGG                PIC X(1)    VALUE SPACE.                 
008800     SKIP2                                                                
008900 01    BORTTAG                   PIC X(1)    VALUE SPACE.                 
009000     SKIP2                                                                
009100 01    AENDRING                  PIC X(1)    VALUE SPACE.                 
009200     SKIP2                                                                
009300                                                                          
009400*********************  NYCKLAR TILL DLI  *************************        
009500 01    FILLER                    PIC X(16)   VALUE 'NYCKLAR-DLI'.         
009700                                                                          
009800 01    NYCKLAR-TILL-DLI.                                                  
010200   03   W-WDGXKEY-5133-X.                                                 
010300     05    FILLER                  PIC X(4)    VALUE '5133'.              
010400     05    FILLER                  PIC X(26)   VALUE LOW-VALUE.           
010500   03    W-IDARTNR-X.                                                     
010600     05    W-IDARTNR               PIC S9(9)        COMP-3.               
010700                                                                          
010800*********************  MEDDELANDEN  ******************************        
010810 01  MESSAGE-CODES.                                                       
010811     03  KOD-KORR-UPPLYSTA-FLT   PIC X(3)   VALUE '001'.                  
010812     03  KOD-TRYCK-PF11          PIC X(3)   VALUE '003'.                  
010813     03  KOD-URVAL-SAKNAS        PIC X(3)   VALUE '005'.                  
010814     03  KOD-PF11-INGET-INDATA   PIC X(3)   VALUE '011'.                  
010815     03  KOD-UPPDATERING-UTFORD  PIC X(3)   VALUE '101'.                  
010816     03  KOD-MER-INFO-FINNS      PIC X(3)   VALUE '105'.                  
010820     03  KOD-SISTA-SIDAN         PIC X(3)   VALUE '106'.                  
018300     EJECT                                                                
018600*************   AREOR FÖR MFS OCH SKÄRMHANTERING    **************        
018700*                                                                         
018800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
018900     SKIP3                                                                
019000*01      MID -COPY W5I14301                                               
019100     EJECT                                                                
019200*01    -COPY WMSGAREA                                                     
019300     EJECT                                                                
019400*  03    MOD -COPY W5O14301 -RED MSG-AREA.                                
019500     EJECT                                                                
019600*01    -COPY WMFSAREA                                                     
019700     EJECT                                                                
019800*01    -COPY WDECAREA                                                     
019801   EJECT                                                                  
019803*  --- PARAMETRAR TILL SUBPGM WMEDKONV                                    
019810*  01 -COPY WMEDAREA.                                                     
019900     EJECT                                                                
020000******************************************************************        
020100*                                                                         
020200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020300*                                                                         
020400 01    IMS-WS.                                                            
020500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
020600     SKIP3                                                                
020700*                        **** STATUS-KOD FRÅN IMS                         
020800   03    STATUS-WS               PIC XX.                                  
020900     88    SEGMENT-FINNS                     VALUE '  '.                  
021000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
021100     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
021200     SKIP3                                                                
021300   03    GODK-STATUSKODER.                                                
021400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
021500     SKIP3                                                                
021600 01    SSA1                      PIC X(164).                              
021700 01    SSA2                      PIC X(164).                              
021900     EJECT                                                                
022000*                            IMS FUNKTIONSKODER                           
022100*01     -COPY W0003                                                       
022200     EJECT                                                                
022300*                            DLI INPUT-OUTPUT AREA                        
022400 01    FILLER                    PIC X(11)   VALUE                        
022500         'DLI-IO-AREA'.                                                   
022600 01    DLI-IO-AREA.                                                       
022700   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
022800     EJECT                                                                
022900*  03    WL513311 -COPY WDGX5134   -RED IO-AREA.                          
023000     EJECT                                                                
023100 LINKAGE SECTION.                                                         
023200*01    -COPY W0009     -PRE MSG-                                          
023300     EJECT                                                                
023400*01    -COPY W0008     -PRE 5133-                                         
023500     05  FILLER                  PIC X(35).                               
023600     EJECT                                                                
023700*01    -COPY W0008     -PRE ARTC-                                         
023800     05  FILLER                  PIC X(9).                                
023900     EJECT                                                                
024000 PROCEDURE DIVISION USING MSG-PCB 5133-PCB ARTC-PCB.                      
024100     ENTRY 'DLITCBL' USING MSG-PCB 5133-PCB ARTC-PCB.                     
024200                                                                          
024300 STYR SECTION.                                                            
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600     IF SEGMENT-FINNS                                                     
024700       PERFORM A-INIT-SPARA-INPUT                                         
024800       PERFORM B-KONTR-NYCKEL                                             
024900       IF NYCKEL-OK = JA                                                  
025000         IF NYA-NYCKLAR = JA                                              
025100           PERFORM C-NYA-NYCKLAR                                          
025200         ELSE                                                             
025300           IF MFS-UPDATE                                                  
025400             PERFORM D-KONTR-INDATA                                       
025500             IF FEL-FINNS1 = NEJ AND FEL-FINNS2 = NEJ AND                 
025600                FEL-FINNS3 = NEJ                                          
025700               PERFORM E-UPPDATERA                                        
025800             END-IF                                                       
025900           ELSE                                                           
026000             IF MFS-IDPFK = '7'                                           
026100               PERFORM F-LAES-FOERSTA-DETALJKOD                           
026200             ELSE                                                         
026300               IF MFS-IDPFK = '8'                                         
026400                 PERFORM G-LAES-NAESTA-DETALJKOD                          
026500               ELSE                                                       
026600                 PERFORM H-LAES-SAMMA-DETALJKOD                           
026700               END-IF                                                     
026800             END-IF                                                       
026900             PERFORM S01-VISA-TABELL                                      
027000           END-IF                                                         
027100         END-IF                                                           
027200       END-IF                                                             
027300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027400       PERFORM IMS-INSERT-MSG                                             
027500     END-IF                                                               
027600     MOVE ZERO TO RETURN-CODE                                             
027700     GOBACK                                                               
027800     .                                                                    
027900     EJECT                                                                
028100 A-INIT-SPARA-INPUT SECTION.                                              
028200                                                                          
028300     IF MSG-DUBBLA-TRANSKODER                                             
028400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I14301                 
028500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
028600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
028700       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
028800     ELSE                                                                 
028900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I14301                 
029000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
029100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
029200       MOVE ' '                           TO MFS-KDTRTYP                  
029300     END-IF                                                               
029400                                                                          
029500     MOVE MSG-IDPFK          TO MFS-IDPFK                                 
029600     MOVE LOW-VALUE          TO MSG-AREA                                  
029700     MOVE 'W5O14301'         TO MFS-IDMOD                                 
029800     MOVE '5143'             TO MOD-IDTRANS                               
029900     MOVE MFS-IDTRANS        TO WS-IDTRANS                                
030001                                                                          
030002     IF MFS-KDMFSFOR = '1'                                                
030003       MOVE 'S  '            TO MED-IDSKYLT                               
030004     ELSE                                                                 
030005       MOVE 'GB '            TO MED-IDSKYLT                               
030010     END-IF                                                               
030600                                                                          
030700     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                              
030800                                MOD-TEMFSFEL                              
030900     .                                                                    
031000     EJECT                                                                
031100 B-KONTR-NYCKEL SECTION.                                                  
031200                                                                          
031300     MOVE NEJ TO NYA-NYCKLAR                                              
031400                                                                          
031500     IF MID-IDARTNR-EMB-IN = ALL '+'                                      
031600       MOVE MID-IDARTNR-EMB-UT       TO WS-IDARTNR-ALPA                   
031700       INSPECT WS-IDARTNR-ALPA REPLACING LEADING SPACE BY ZERO            
031800     ELSE                                                                 
031900       MOVE MID-IDARTNR-EMB-IN       TO WS-IDARTNR-ALPA                   
032000       INSPECT WS-IDARTNR-ALPA REPLACING ALL SPACE BY ZERO                
032100       MOVE JA                       TO NYA-NYCKLAR                       
032200     END-IF                                                               
032300                                                                          
032400     IF WS-IDARTNR-ALPA NUMERIC                                           
032500       MOVE JA                    TO NYCKEL-OK                            
032600     ELSE                                                                 
032700       MOVE NEJ                   TO NYCKEL-OK                            
032710       MOVE KOD-URVAL-SAKNAS      TO MED-IDMFSFEL                         
032720       CALL WMEDKONV USING MED-WMEDAREA                                   
032730       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
032900       MOVE ZERO                  TO MOD-IDARTNR-EMB-SPAR                 
033000     END-IF                                                               
033100                                                                          
033200     MOVE WS-IDARTNR-ALPA         TO WS-IDARTNR-NUM                       
033300     MOVE WS-IDARTNR-NUM          TO MOD-IDARTNR-EMB-UT                   
033400     INSPECT MOD-IDARTNR-EMB-UT REPLACING LEADING ZERO BY SPACE           
033500     MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-EMB-IN                   
033600                                                                          
033700     IF NOT EGEN-BILD                                                     
033800       MOVE NEJ                   TO NYCKEL-OK                            
033900       MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-EMB-UT                   
034000                                     MOD-IDARTNR-EMB                      
034010       MOVE KOD-URVAL-SAKNAS      TO MED-IDMFSFEL                         
034020       CALL WMEDKONV USING MED-WMEDAREA                                   
034030       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
034040       MOVE ZERO                  TO MOD-IDARTNR-EMB-SPAR                 
034300       MOVE SPACE                 TO MOD-TEMFSFEL                         
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 C-NYA-NYCKLAR SECTION.                                                   
034800                                                                          
034900     EVALUATE TRUE                                                        
035000                                                                          
035100       WHEN MID-IDARTNR-EMB-IN = ALL '+'  AND                             
035200            MFS-UPDATE                                                    
035300         PERFORM D-KONTR-INDATA                                           
035400         IF FEL-FINNS1 = NEJ   AND  FEL-FINNS2 = NEJ  AND                 
035500            FEL-FINNS3 = NEJ                                              
035600           PERFORM E-UPPDATERA                                            
035700         END-IF                                                           
035800                                                                          
035900       WHEN MID-IDARTNR-EMB-IN = ALL '+'  AND                             
036000            NOT MFS-UPDATE                                                
036100         PERFORM IMS-GET-OKVAL-DETALJKOD                                  
036200                                                                          
036300       WHEN OTHER                                                         
036400         MOVE WS-IDARTNR-NUM       TO W-IDARTNR                           
036500         PERFORM IMS-GET-DETALJKOD-ELLER-NAESTA                           
036600                                                                          
036700     END-EVALUATE                                                         
036800                                                                          
036900                                                                          
037000     IF NOT MFS-UPDATE                                                    
037100       PERFORM S01-VISA-TABELL                                            
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500                                                                          
037600 D-KONTR-INDATA SECTION.                                                  
037700                                                                          
037800     EVALUATE TRUE                                                        
037900                                                                          
038000       WHEN MID-IDARTNR-EMB NOT = ALL '+'   AND                           
038100            MID-IDARTNR-EMB NUMERIC         AND                           
038200            MID-IDARTNR-EMB NOT = ZERO                                    
038300          MOVE JA TO IDARTNR-IFYLLD                                       
038400          MOVE NEJ TO FEL-FINNS1                                          
038500                                                                          
038600       WHEN MID-IDARTNR-EMB NOT = ALL '+'   AND                           
038700            (MID-IDARTNR-EMB NOT NUMERIC    OR                            
038800             MID-IDARTNR-EMB = ZERO)                                      
038900          PERFORM DA-LAES-IN-OEVRIGA-FAELT                                
039000          MOVE JA TO FEL-FINNS1                                           
039100          MOVE MFS-ROER-EJ-FAELT       TO MOD-IDARTNR-EMB                 
039200                                                                          
039300       WHEN MID-IDARTNR-EMB = ALL '+'   AND                               
039400            MID-PRDMTRL = ALL '+'       AND                               
039500            MID-KDCMD = ALL '+'                                           
039600          MOVE NEJ TO FEL-FINNS1                                          
039700          MOVE JA TO INGENTING-IFYLLT                                     
039800                                                                          
039900       WHEN OTHER                                                         
040000          PERFORM DA-LAES-IN-OEVRIGA-FAELT                                
040100          MOVE JA TO FEL-FINNS1                                           
040200          MOVE MFS-RENSA-FAELT         TO MID-IDARTNR-EMB                 
040300                                                                          
040400     END-EVALUATE                                                         
040500                                                                          
040600     MOVE NEJ TO PRDMTRL-IFYLLD                                           
040700                                                                          
040800     IF MID-PRDMTRL NOT = ALL '+'                                         
040900       MOVE JA TO PRDMTRL-IFYLLD                                          
041000       MOVE NEJ TO FEL-FINNS2                                             
041100     END-IF                                                               
041200                                                                          
041300     EVALUATE TRUE                                                        
041400                                                                          
041500       WHEN MID-KDCMD-INGENTING OR MID-KDCMD = '+'                        
041600         MOVE NEJ TO KDCMD-IFYLLD                                         
041700         MOVE NEJ TO FEL-FINNS3                                           
041800                                                                          
041900       WHEN MID-KDCMD NOT = '+' AND MID-KDCMD-DELETE                      
042000         MOVE JA TO KDCMD-IFYLLD                                          
042100         MOVE NEJ TO FEL-FINNS3                                           
042200                                                                          
042300       WHEN OTHER                                                         
042400         PERFORM DB-LAES-IN-OEVRIGA-FAELT                                 
042500         MOVE JA TO FEL-FINNS3                                            
042600                                                                          
042700     END-EVALUATE                                                         
042800     .                                                                    
042900     EJECT                                                                
043000 DA-LAES-IN-OEVRIGA-FAELT SECTION.                                        
043100                                                                          
043200     IF MID-PRDMTRL NOT = ALL '+'                                         
043300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-PRDMTRL-NY-ATTR                 
043400       MOVE MFS-ROER-EJ-FAELT      TO MOD-PRDMTRL                         
043500     END-IF                                                               
043600                                                                          
043700     IF MID-KDCMD NOT =  '+'                                              
043800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-ATTR                      
043900       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDCMD                           
044000     END-IF                                                               
044100                                                                          
044200     PERFORM MFS-ROER-EJ-TABELL                                           
044300     MOVE MFS-NUM-FAELT-FEL        TO MOD-IDARTNR-NY-ATTR                 
044310     MOVE KOD-KORR-UPPLYSTA-FLT    TO MED-IDMFSFEL                        
044320     CALL WMEDKONV USING MED-WMEDAREA                                     
044330     MOVE MED-MFSFEL               TO MOD-TEMFSFEL                        
044500     MOVE MFS-ROER-EJ-FAELT        TO MOD-IDARTNR-EMB-SPAR                
044600                                      MOD-IDARTNR-EMB-RAD1                
044700     .                                                                    
044800     EJECT                                                                
044900 DB-LAES-IN-OEVRIGA-FAELT SECTION.                                        
045000                                                                          
045100     IF MID-IDARTNR-EMB NOT = ALL '+'                                     
045200       MOVE MID-IDARTNR-EMB TO MOD-IDARTNR-EMB                            
045300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-NY-ATTR                 
045400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-EMB                          
045500     END-IF                                                               
045600                                                                          
045700     IF MID-PRDMTRL NOT = ALL '+'                                         
045800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-PRDMTRL-NY-ATTR                 
045900       MOVE MFS-ROER-EJ-FAELT      TO MOD-PRDMTRL                         
046000     END-IF                                                               
046100                                                                          
046200     PERFORM MFS-ROER-EJ-TABELL                                           
046300     MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDCMD-ATTR                      
046310     MOVE KOD-KORR-UPPLYSTA-FLT    TO MED-IDMFSFEL                        
046320     CALL WMEDKONV USING MED-WMEDAREA                                     
046330     MOVE MED-MFSFEL               TO MOD-TEMFSFEL                        
046500     MOVE MFS-ROER-EJ-FAELT        TO MOD-IDARTNR-EMB-SPAR                
046600                                      MOD-IDARTNR-EMB-RAD1                
046700                                      MOD-KDCMD                           
046800     .                                                                    
046900     EJECT                                                                
047000                                                                          
047100 E-UPPDATERA SECTION.                                                     
047200                                                                          
047300     EVALUATE TRUE                                                        
047400                                                                          
047500       WHEN IDARTNR-IFYLLD = JA    AND                                    
047600            PRDMTRL-IFYLLD = JA    AND                                    
047700            KDCMD-IFYLLD = NEJ                                            
047800         PERFORM EA-KONTR-KOSTNADS-FAELT                                  
047900                                                                          
048000       WHEN IDARTNR-IFYLLD = JA    AND                                    
048100           (PRDMTRL-IFYLLD = NEJ OR PRDMTRL-IFYLLD = JA) AND              
048200            KDCMD-IFYLLD = JA                                             
048300         MOVE JA             TO PRIS-OK                                   
048400         MOVE JA             TO BORTTAG                                   
048500                                                                          
048600       WHEN INGENTING-IFYLLT = JA                                         
048700         MOVE NEJ TO PRIS-OK                                              
048800         PERFORM MFS-ROER-EJ-TABELL                                       
048810         MOVE KOD-PF11-INGET-INDATA TO MED-IDMFSFEL                       
048820         CALL WMEDKONV USING MED-WMEDAREA                                 
048830         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
049000         MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-EMB                  
049100                                                                          
049200       WHEN IDARTNR-IFYLLD = JA    AND                                    
049300            PRDMTRL-IFYLLD = NEJ   AND                                    
049400            KDCMD-IFYLLD = NEJ                                            
049410          MOVE KOD-KORR-UPPLYSTA-FLT  TO MED-IDMFSFEL                     
049420          CALL WMEDKONV USING MED-WMEDAREA                                
049430          MOVE MED-MFSFEL             TO MOD-TEMFSFEL                     
049600          MOVE NEJ TO PRIS-OK                                             
049700          PERFORM MFS-ROER-EJ-TABELL                                      
049800          MOVE MFS-ROER-EJ-FAELT      TO MOD-IDARTNR-EMB                  
049900          MOVE MFS-NUM-FAELT-FEL      TO MOD-PRDMTRL-NY-ATTR              
050000          MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-NY-ATTR              
050100          MOVE MFS-RENSA-FAELT        TO MOD-PRDMTRL                      
050200                                         MOD-KDCMD                        
050300                                                                          
050400     END-EVALUATE                                                         
050500                                                                          
050600     PERFORM EB-TABORT-AENDRA-NYUPPLAEGG                                  
050700     .                                                                    
050800     EJECT                                                                
050900 EA-KONTR-KOSTNADS-FAELT SECTION.                                         
051000                                                                          
051100     MOVE +6            TO DEC-KVHELTAL                                   
051200     MOVE +3            TO DEC-KVDECIMAL                                  
051300     MOVE MID-PRDMTRL   TO DEC-IDFRIDATA                                  
051400                                                                          
051500     CALL WDECEDIT USING DEC-WDECAREA                                     
051600                                                                          
051700     IF DEC-KDSVAR-OK                                                     
051800       MOVE DEC-IDEDITDATA TO WS-PRDMTRL                                  
051900       MOVE JA TO PRIS-OK                                                 
052000     ELSE                                                                 
052100       MOVE NEJ TO PRIS-OK                                                
052200       PERFORM MFS-ROER-EJ-TABELL                                         
052210       MOVE KOD-KORR-UPPLYSTA-FLT       TO MED-IDMFSFEL                   
052220       CALL WMEDKONV USING MED-WMEDAREA                                   
052230       MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                   
052400       MOVE MFS-NUM-FAELT-FEL           TO MOD-PRDMTRL-NY-ATTR            
052500       MOVE MFS-ADD-LAES-IN-FAELT       TO MOD-IDARTNR-NY-ATTR            
052600       MOVE MFS-ROER-EJ-FAELT           TO MOD-PRDMTRL                    
052700                                           MOD-IDARTNR-EMB                
052800     END-IF                                                               
052900     .                                                                    
053000     EJECT                                                                
053100                                                                          
053200 EB-TABORT-AENDRA-NYUPPLAEGG SECTION.                                     
053300                                                                          
053400     IF PRIS-OK = JA                                                      
053500       MOVE MID-IDARTNR-EMB          TO WS-IDARTNR-ALPA-NY                
053600       MOVE WS-IDARTNR-ALPA-NY       TO WS-IDARTNR-NUM-NY                 
053700       MOVE WS-IDARTNR-NUM-NY        TO W-IDARTNR                         
053800                                                                          
053900       PERFORM IMS-GHU-DETALJKOD                                          
054000       IF SEGMENT-FINNS                                                   
054100         IF BORTTAG = JA                                                  
054200           PERFORM EBA-TA-BORT-DETALJKOD                                  
054300         ELSE                                                             
054400           PERFORM EBB-AENDRA-DETALJKOD                                   
054500         END-IF                                                           
054600       ELSE                                                               
054700         IF MID-KDCMD = '+' OR MID-KDCMD-INGENTING                        
054800           PERFORM EBC-NYUPPLAEGG-DETALJKOD                               
054900         ELSE                                                             
055000           PERFORM MFS-ROER-EJ-TABELL                                     
055100           PERFORM DB-LAES-IN-OEVRIGA-FAELT                               
055110           MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                   
055120           CALL WMEDKONV USING MED-WMEDAREA                               
055130           MOVE MED-MFSFEL              TO MOD-TEMFSFEL                   
055300         END-IF                                                           
055400       END-IF                                                             
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 EBA-TA-BORT-DETALJKOD SECTION.                                           
055900                                                                          
056000     MOVE WS-IDARTNR-NUM-NY              TO WS-IDARTNR-HJAELP             
056100     PERFORM IMS-DLT-DETALJKOD                                            
056200     MOVE JA                             TO UPPDATERING                   
056210     MOVE KOD-UPPDATERING-UTFORD         TO MED-IDMFSINF                  
056220     CALL WMEDKONV USING MED-WMEDAREA                                     
056230     MOVE MED-MFSINF                     TO MOD-TEMFSINF                  
056400     MOVE MID-IDARTNR-EMB-RAD            TO W-IDARTNR                     
056500                                                                          
056600     PERFORM IMS-GET-DETALJKOD-ELLER-NAESTA                               
056700     IF SEGMENT-FINNS                                                     
056800       PERFORM S011-LAEGG-UT-RAD1                                         
056900       PERFORM IMS-GU-WDR1-ROT                                            
057000       PERFORM S012-LAEGG-UT-TABELL                                       
057100     ELSE                                                                 
057200       PERFORM IMS-GET-OKVAL-DETALJKOD                                    
057300       IF SEGMENT-FINNS                                                   
057400         PERFORM S011-LAEGG-UT-RAD1                                       
057500         PERFORM IMS-GU-WDR1-ROT                                          
057600         PERFORM S012-LAEGG-UT-TABELL                                     
057700       ELSE                                                               
057710         MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                     
057720         CALL WMEDKONV USING MED-WMEDAREA                                 
057730         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
057900         PERFORM MFS-RENSA-TABELL                                         
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 EBB-AENDRA-DETALJKOD SECTION.                                            
058500                                                                          
058600     MOVE WS-PRDMTRL               TO 5134-PRDMTRL                        
058700                                                                          
058800     PERFORM IMS-REPL-DETALJKOD                                           
058810     MOVE KOD-UPPDATERING-UTFORD   TO MED-IDMFSINF                        
058820     CALL WMEDKONV USING MED-WMEDAREA                                     
058830     MOVE MED-MFSINF               TO MOD-TEMFSINF                        
059000     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-PRDMTRL-ATTR(+1)                
059100     MOVE WS-IDARTNR-NUM-NY        TO W-IDARTNR                           
059200     MOVE JA TO UPPDATERING                                               
059300                                                                          
059400     PERFORM IMS-GET-DETALJKOD                                            
059500     PERFORM S011-LAEGG-UT-RAD1                                           
059600     PERFORM IMS-GU-WDR1-ROT                                              
059700     PERFORM S012-LAEGG-UT-TABELL                                         
059800     .                                                                    
059900     EJECT                                                                
060000 EBC-NYUPPLAEGG-DETALJKOD SECTION.                                        
060100                                                                          
060200     MOVE WS-IDARTNR-NUM-NY      TO 5134-IDARTNR-EMB                      
060300                                                                          
060400     MOVE WS-PRDMTRL             TO 5134-PRDMTRL                          
060500                                                                          
060600     PERFORM IMS-ISRT-DETALJKOD                                           
060610     MOVE KOD-UPPDATERING-UTFORD TO MED-IDMFSINF                          
060620     CALL WMEDKONV USING MED-WMEDAREA                                     
060630     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
060800     MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-PRDMTRL-ATTR(+1)                  
060900                                    MOD-IDARTNR-EMB-ATTR(+1)              
061000     MOVE WS-IDARTNR-NUM-NY      TO W-IDARTNR                             
061100                                                                          
061200     PERFORM IMS-GET-DETALJKOD                                            
061300     PERFORM S011-LAEGG-UT-RAD1                                           
061400     PERFORM IMS-GU-WDR1-ROT                                              
061500     MOVE JA TO UPPDATERING                                               
061600     PERFORM S012-LAEGG-UT-TABELL                                         
061700     .                                                                    
061800     EJECT                                                                
061900 F-LAES-FOERSTA-DETALJKOD SECTION.                                        
062000                                                                          
062100     PERFORM IMS-GET-OKVAL-DETALJKOD                                      
062200     .                                                                    
062300     SKIP3                                                                
062400 G-LAES-NAESTA-DETALJKOD SECTION.                                         
062500                                                                          
062600     MOVE MID-IDARTNR-EMB-SPAR        TO W-IDARTNR                        
062700                                                                          
062800     PERFORM IMS-GET-DETALJKOD                                            
062900     .                                                                    
063000     SKIP3                                                                
063100 H-LAES-SAMMA-DETALJKOD SECTION.                                          
063200                                                                          
063300     MOVE MID-IDARTNR-EMB-RAD         TO WS-IDARTNR-HJAELP                
063400     MOVE WS-IDARTNR-HJAELP           TO W-IDARTNR                        
063500                                                                          
063600     PERFORM IMS-GET-DETALJKOD-ELLER-NAESTA                               
063700     .                                                                    
063800     EJECT                                                                
063900 S01-VISA-TABELL SECTION.                                                 
064000                                                                          
064100     IF SEGMENT-FINNS                                                     
064200       IF MID-IDARTNR-EMB = ALL '+'    AND                                
064300         MID-PRDMTRL = ALL '+'         AND                                
064400         MID-KDCMD = '+'                                                  
064500         PERFORM S011-LAEGG-UT-RAD1                                       
064600         PERFORM IMS-GU-WDR1-ROT                                          
064700         PERFORM S012-LAEGG-UT-TABELL                                     
064800       ELSE                                                               
064900         PERFORM S013-KONTR-AENDRAT-FAELT                                 
065000       END-IF                                                             
065100     ELSE                                                                 
065200       IF MID-IDARTNR-EMB = ALL '+'    AND                                
065300         MID-PRDMTRL = ALL '+'         AND                                
065400         MID-KDCMD = '+'                                                  
065500         PERFORM MFS-RENSA-TABELL                                         
065510         MOVE KOD-URVAL-SAKNAS             TO MED-IDMFSFEL                
065520         CALL WMEDKONV USING MED-WMEDAREA                                 
065530         MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                
065700         MOVE MFS-RENSA-FAELT              TO MOD-IDARTNR-EMB             
065800       ELSE                                                               
065900         PERFORM S013-KONTR-AENDRAT-FAELT                                 
066000       END-IF                                                             
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 S011-LAEGG-UT-RAD1 SECTION.                                              
066500                                                                          
066600     SET MOD-IX TO +1                                                     
066700     MOVE MFS-RENSA-FAELT               TO MOD-IDARTNR-EMB                
066800     MOVE 5134-IDARTNR-EMB              TO                                
066900     MOD-IDARTNR-EMB-RAD(MOD-IX)                                          
067000     MOVE MOD-IDARTNR-EMB-RAD(MOD-IX)   TO MOD-IDARTNR-EMB-RAD1           
067100     MOVE 5134-PRDMTRL                  TO WS-PRDMTRL                     
067200     MOVE WS-PRDMTRL                    TO MOD-PRDMTRL-RAD(MOD-IX)        
067300     MOVE 5134-IDARTNR-EMB              TO WS-IDARTNR-NUM                 
067400     MOVE WS-IDARTNR-NUM                TO MOD-IDARTNR-EMB-SPAR           
067500     SET MOD-IX UP BY +1                                                  
067600     .                                                                    
067700     EJECT                                                                
067800 S012-LAEGG-UT-TABELL SECTION.                                            
067900                                                                          
068000       MOVE WS-IDARTNR-NUM          TO W-IDARTNR                          
068100                                                                          
068200       PERFORM IMS-GNP-DETALJKOD                                          
068300       IF SEGMENT-FINNS                                                   
068500         MOVE 5134-IDARTNR-EMB      TO MOD-IDARTNR-EMB-RAD(MOD-IX)        
068600         MOVE 5134-PRDMTRL          TO WS-PRDMTRL                         
068800         MOVE WS-PRDMTRL            TO MOD-PRDMTRL-RAD(MOD-IX)            
068900         MOVE 5134-IDARTNR-EMB      TO WS-IDARTNR-NUM                     
069000         MOVE WS-IDARTNR-NUM        TO MOD-IDARTNR-EMB-SPAR               
069100                                                                          
069200         SET MOD-IX UP BY +1                                              
069300                                                                          
069400         PERFORM UNTIL MOD-IX > 12 OR SEGMENT-SAKNAS                      
069500           MOVE NAESTA-DETALJKOD    TO W-IDARTNR                          
069600                                                                          
069700           PERFORM IMS-GNP-DETALJKOD                                      
069800           IF SEGMENT-FINNS                                               
070000             MOVE 5134-IDARTNR-EMB  TO MOD-IDARTNR-EMB-RAD(MOD-IX)        
070100             MOVE 5134-PRDMTRL      TO WS-PRDMTRL                         
070300             MOVE WS-PRDMTRL        TO MOD-PRDMTRL-RAD(MOD-IX)            
070400             MOVE 5134-IDARTNR-EMB  TO WS-IDARTNR-NUM                     
070600             MOVE 5134-IDARTNR-EMB  TO MOD-IDARTNR-EMB-SPAR               
070700             SET MOD-IX UP BY +1                                          
070800           ELSE                                                           
070900*            SET MOD-IX TO +12                                            
071000             IF UPPDATERING = JA                                          
071100               CONTINUE                                                   
071200             ELSE                                                         
071300               MOVE KOD-SISTA-SIDAN TO MED-IDMFSINF                       
071310               CALL WMEDKONV USING MED-WMEDAREA                           
071320               MOVE MED-MFSINF      TO MOD-TEMFSINF                       
071400             END-IF                                                       
071500           END-IF                                                         
071600                                                                          
071700         END-PERFORM                                                      
071800                                                                          
071900         PERFORM S0121-SAETT-INDEX                                        
072000       ELSE                                                               
072100         SET MOD-IX TO +12                                                
072200         IF UPPDATERING = JA                                              
072300           CONTINUE                                                       
072400         ELSE                                                             
072500           MOVE KOD-SISTA-SIDAN     TO MED-IDMFSINF                       
072510           CALL WMEDKONV USING MED-WMEDAREA                               
072520           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
072600         END-IF                                                           
072900         MOVE MOD-IDARTNR-EMB-RAD(1) TO MOD-IDARTNR-EMB-SPAR              
073000       END-IF                                                             
073100       .                                                                  
073200       EJECT                                                              
073300 S0121-SAETT-INDEX SECTION.                                               
073400                                                                          
073500     IF MOD-IX = 13                                                       
073600                                                                          
073700       PERFORM IMS-GNP-DETALJKOD                                          
073800       IF SEGMENT-FINNS                                                   
074000         MOVE 5134-IDARTNR-EMB      TO MOD-IDARTNR-EMB-SPAR               
074100         INSPECT MOD-IDARTNR-EMB-SPAR REPLACING ALL                       
074200                                 SPACE BY ZERO                            
074300         IF UPPDATERING = JA                                              
074400           CONTINUE                                                       
074500         ELSE                                                             
074510           MOVE KOD-MER-INFO-FINNS  TO MED-IDMFSINF                       
074520           CALL WMEDKONV USING MED-WMEDAREA                               
074530           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
074700         END-IF                                                           
074800       ELSE                                                               
074900         IF UPPDATERING = JA                                              
075000           CONTINUE                                                       
075100         ELSE                                                             
075110           MOVE KOD-SISTA-SIDAN     TO MED-IDMFSINF                       
075120           CALL WMEDKONV USING MED-WMEDAREA                               
075130           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
075300         END-IF                                                           
075500         MOVE MOD-IDARTNR-EMB-RAD(+1) TO  MOD-IDARTNR-EMB-SPAR            
075600         INSPECT MOD-IDARTNR-EMB-SPAR REPLACING ALL                       
075700         SPACE BY ZERO                                                    
075800       END-IF                                                             
075900     ELSE                                                                 
076000       IF MOD-IX < 12                                                     
076200         MOVE MOD-IDARTNR-EMB-RAD(+1) TO  MOD-IDARTNR-EMB-SPAR            
076300       END-IF                                                             
076400     END-IF                                                               
076500     .                                                                    
076600     EJECT                                                                
076700 S013-KONTR-AENDRAT-FAELT SECTION.                                        
076800                                                                          
076900     PERFORM MFS-ROER-EJ-TABELL                                           
076910       MOVE KOD-TRYCK-PF11               TO MED-IDMFSFEL                  
076920       CALL WMEDKONV USING MED-WMEDAREA                                   
076930       MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                  
077100                                                                          
077200     IF WS-IDARTNR-ALPA NOT = ALL '+'                                     
077300       MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-IDARTNR-NY-ATTR           
077400       MOVE MFS-ROER-EJ-FAELT            TO MOD-IDARTNR-EMB               
077500     END-IF                                                               
077600                                                                          
077700     IF MID-PRDMTRL NOT = ALL '+'                                         
077800       MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-PRDMTRL-NY-ATTR           
077900       MOVE MFS-ROER-EJ-FAELT            TO MOD-PRDMTRL                   
078000     END-IF                                                               
078100                                                                          
078200     IF MID-KDCMD NOT = '+'                                               
078300       MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-KDCMD-ATTR                
078400       MOVE MFS-ROER-EJ-FAELT            TO MOD-KDCMD                     
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 MFS-ROER-EJ-TABELL SECTION.                                              
078900                                                                          
079000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-EMB-RAD(+1)                    
079100                               MOD-PRDMTRL-RAD(+1)                        
079200                               MOD-IDARTNR-EMB-RAD(+2)                    
079300                               MOD-PRDMTRL-RAD(+2)                        
079400                               MOD-IDARTNR-EMB-RAD(+3)                    
079500                               MOD-PRDMTRL-RAD(+3)                        
079600                               MOD-IDARTNR-EMB-RAD(+4)                    
079700                               MOD-PRDMTRL-RAD(+4)                        
079800                               MOD-IDARTNR-EMB-RAD(+5)                    
079900                               MOD-PRDMTRL-RAD(+5)                        
080000                               MOD-IDARTNR-EMB-RAD(+6)                    
080100                               MOD-PRDMTRL-RAD(+6)                        
080200                               MOD-IDARTNR-EMB-RAD(+7)                    
080300                               MOD-PRDMTRL-RAD(+7)                        
080400                               MOD-IDARTNR-EMB-RAD(+8)                    
080500                               MOD-PRDMTRL-RAD(+8)                        
080600                               MOD-IDARTNR-EMB-RAD(+9)                    
080700                               MOD-PRDMTRL-RAD(+9)                        
080800                               MOD-IDARTNR-EMB-RAD(+10)                   
080900                               MOD-PRDMTRL-RAD(+10)                       
081000                               MOD-IDARTNR-EMB-RAD(+11)                   
081100                               MOD-PRDMTRL-RAD(+11)                       
081200                               MOD-IDARTNR-EMB-RAD(+12)                   
081300                               MOD-PRDMTRL-RAD(+12)                       
081400                                                                          
081500     .                                                                    
081600     EJECT                                                                
081700 MFS-RENSA-TABELL SECTION.                                                
081800                                                                          
081900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMB-RAD(+1)                      
082000                             MOD-PRDMTRL-RAD(+1)                          
082100                             MOD-IDARTNR-EMB-RAD(+2)                      
082200                             MOD-PRDMTRL-RAD(+2)                          
082300                             MOD-IDARTNR-EMB-RAD(+3)                      
082400                             MOD-PRDMTRL-RAD(+3)                          
082500                             MOD-IDARTNR-EMB-RAD(+4)                      
082600                             MOD-PRDMTRL-RAD(+4)                          
082700                             MOD-IDARTNR-EMB-RAD(+5)                      
082800                             MOD-PRDMTRL-RAD(+5)                          
082900                             MOD-IDARTNR-EMB-RAD(+6)                      
083000                             MOD-PRDMTRL-RAD(+6)                          
083100                             MOD-IDARTNR-EMB-RAD(+7)                      
083200                             MOD-PRDMTRL-RAD(+7)                          
083300                             MOD-IDARTNR-EMB-RAD(+8)                      
083400                             MOD-PRDMTRL-RAD(+8)                          
083500                             MOD-IDARTNR-EMB-RAD(+9)                      
083600                             MOD-PRDMTRL-RAD(+9)                          
083700                             MOD-IDARTNR-EMB-RAD(+10)                     
083800                             MOD-PRDMTRL-RAD(+10)                         
083900                             MOD-IDARTNR-EMB-RAD(+11)                     
084000                             MOD-PRDMTRL-RAD(+11)                         
084100                             MOD-IDARTNR-EMB-RAD(+12)                     
084200                             MOD-PRDMTRL-RAD(+12)                         
084300                                                                          
084400     .                                                                    
084500     EJECT                                                                
084600* IMS SEKTIONER                                                           
084700     SKIP3                                                                
084800 IMS-GET-MSG SECTION.                                                     
084900                                                                          
085000     MOVE '  QC' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
085200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     SKIP3                                                                
085600 IMS-INSERT-MSG SECTION.                                                  
085700                                                                          
085800     IF ENGLISH-TEXT                                                      
085900       MOVE '0' TO MFS-KDHUVOMR                                           
086000     END-IF                                                               
086100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
086200     MOVE SPACE TO GODK-STATUSKODER                                       
086300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
086400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     EJECT                                                                
086800 IMS-GET-DETALJKOD SECTION.                                               
086900                                                                          
087000     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
087100            DELIMITED BY SIZE INTO SSA1                                   
087200     STRING 'WL513311(IDARTNR  =' W-IDARTNR-X ')'                         
087300            DELIMITED BY SIZE INTO SSA2                                   
087400     MOVE '  GE' TO GODK-STATUSKODER                                      
087500     CALL CBLTDLI USING GU 5133-PCB DLI-IO-AREA                           
087600                                    SSA1 SSA2                             
087700     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     SKIP2                                                                
088100 IMS-GET-DETALJKOD-ELLER-NAESTA SECTION.                                  
088200                                                                          
088300     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
088400            DELIMITED BY SIZE INTO SSA1                                   
088500     STRING 'WL513311(IDARTNR >=' W-IDARTNR-X ')'                         
088600            DELIMITED BY SIZE INTO SSA2                                   
088700     MOVE '  GE' TO GODK-STATUSKODER                                      
088800     CALL CBLTDLI USING GU 5133-PCB DLI-IO-AREA                           
088900                                    SSA1 SSA2                             
089000     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
089100     PERFORM IMS-STATUSKONTROLL                                           
089200     .                                                                    
089300     SKIP2                                                                
089400 IMS-GU-WDR1-ROT SECTION.                                                 
089500                                                                          
089600     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
089700            DELIMITED BY SIZE INTO SSA1                                   
089800     MOVE '  GE' TO GODK-STATUSKODER                                      
089900     CALL CBLTDLI USING GU 5133-PCB DLI-IO-AREA SSA1                      
090000     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
090100     PERFORM IMS-STATUSKONTROLL                                           
090200     .                                                                    
090300     EJECT                                                                
090400 IMS-GET-OKVAL-DETALJKOD SECTION.                                         
090500                                                                          
090600     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
090700            DELIMITED BY SIZE INTO SSA1                                   
090800     MOVE 'WL513311' TO SSA2                                              
090900     MOVE '  GE' TO GODK-STATUSKODER                                      
091000     CALL CBLTDLI USING GU 5133-PCB DLI-IO-AREA                           
091100                                    SSA1 SSA2                             
091200     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
091300     PERFORM IMS-STATUSKONTROLL                                           
091400     .                                                                    
091500     SKIP2                                                                
091600 IMS-GHU-DETALJKOD SECTION.                                               
091700                                                                          
091800     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
091900            DELIMITED BY SIZE INTO SSA1                                   
092000     STRING 'WL513311(IDARTNR  =' W-IDARTNR-X ')'                         
092100            DELIMITED BY SIZE INTO SSA2                                   
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GHU 5133-PCB DLI-IO-AREA                          
092400                                    SSA1 SSA2                             
092500     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     EJECT                                                                
092900 IMS-DLT-DETALJKOD SECTION.                                               
093000                                                                          
093100     MOVE '  ' TO GODK-STATUSKODER                                        
093200     CALL CBLTDLI USING DLET 5133-PCB DLI-IO-AREA                         
093300     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     SKIP2                                                                
093700 IMS-REPL-DETALJKOD SECTION.                                              
093800                                                                          
093900     MOVE '  ' TO GODK-STATUSKODER                                        
094000     CALL CBLTDLI USING REPL 5133-PCB DLI-IO-AREA                         
094100     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
094200     PERFORM IMS-STATUSKONTROLL                                           
094300     .                                                                    
094400     SKIP2                                                                
094500 IMS-ISRT-DETALJKOD SECTION.                                              
094600                                                                          
094700     STRING 'WL513301(WDGXKEY  =' W-WDGXKEY-5133-X ')'                    
094800            DELIMITED BY SIZE INTO SSA1                                   
094900     MOVE 'WL513311' TO SSA2                                              
095000     MOVE '  II' TO GODK-STATUSKODER                                      
095100     CALL CBLTDLI USING ISRT 5133-PCB DLI-IO-AREA                         
095200                                    SSA1 SSA2                             
095300     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
095400     PERFORM IMS-STATUSKONTROLL                                           
095500     .                                                                    
095600     EJECT                                                                
095700 IMS-GNP-DETALJKOD SECTION.                                               
095800                                                                          
095900     STRING 'WL513311(IDARTNR  >' W-IDARTNR-X ')'                         
096000            DELIMITED BY SIZE INTO SSA1                                   
096100     MOVE '  GE' TO GODK-STATUSKODER                                      
096200     CALL CBLTDLI USING GNP 5133-PCB DLI-IO-AREA                          
096300                                    SSA1                                  
096400     MOVE 5133-STATUS-CODE TO STATUS-WS                                   
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     .                                                                    
096700     EJECT                                                                
096800 IMS-STATUSKONTROLL SECTION.                                              
096900                                                                          
097000     SET STATUS-IX TO 1                                                   
097100     SEARCH GODK-STATUS                                                   
097110       AT END                                                             
097120         CALL FELLOG                                                      
097200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
097210         CONTINUE                                                         
097300     END-SEARCH                                                           
097400     .                                                                    
