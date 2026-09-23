000300     SKIP3                                                                
000400 ID DIVISION.                                                             
000500     SKIP2                                                                
000600 PROGRAM-ID.     W0080800.                                                
001000*AUTHOR.         STIG MüLLER                                              
001100*DATE-WRITTEN.   AUG   84.                                                
001200                                                                          
001400                                                                          
001500*    FUNKTION.                                                            
001600*        PROGRAM FÖR ATT UPPDATERA LASTBÄRARTYPTEXTER.                    
001700                                                                          
001800                                                                          
001900                                                                          
002000                                                                          
002100*    INDATA.                                                              
002200*        TRANSAKTION: W0T808                                              
002300*        MID:         W0I80801                                            
002400                                                                          
002500*    UTDATA.                                                              
002600*        MOD:         W0O80801                                            
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W0080800'.            
004000 77    JA                        PIC X       VALUE 'J'.                   
004100 77    NEJ                       PIC X       VALUE 'N'.                   
004200 77    SPRAK-IX                  PIC S9(9)   VALUE +0 COMP SYNC.          
004300 77    KDLBTYP-WS                PIC X(3)    VALUE SPACE.                 
004400 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +0   COMP SYNC.         
004500 77    BELBTYP-INMATAD           PIC X      VALUE 'N'.                    
004600 77    WS-IDTRANS                PIC X(4).                                
004700       88  WS-GODKAEND-BILD                 VALUE '0808'.                 
004800     SKIP3                                                                
004810                                                                          
004820 01  DYNAMISKA-SUBPROGRAM.                                                
004830   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004840   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004850                                                                          
004900 01    UPPDAT-TYP-WS             PIC X.                                   
005000     88  ANDRING          VALUE 'Ä' 'C'.                                  
005100     88  BORTTAG          VALUE 'B' 'D'.                                  
005200     88  NYUPPLAGGNING    VALUE 'N'.                                      
005300     SKIP3                                                                
005400 01    UPPDATERING               PIC X.                                   
005500     88  UPPDAT-GODK      VALUE 'J'.                                      
005600     EJECT                                                                
005700 01    NYCKLAR-TILL-DLI.                                                  
005800   03    W-WDGXKEY-X.                                                     
005900     05  W-473I-IDHTYP       PIC X(4)     VALUE '4739'.                   
006000     05  W-473I-KDLBTYP      PIC S9(3)           COMP-3.                  
006100     05  W-473I-LOWVALUE     PIC X(24)    VALUE LOW-VALUE.                
006200     SKIP3                                                                
006300 01    MEDDELANDE.                                                        
006400   03    FEL1.                                                            
006500      05 FILLER                  PIC X(40)                                
006600         VALUE 'FEL NYCKEL'.                                              
006700      05 FILLER                  PIC X(40)                                
006800         VALUE 'WRONG KEY '.                                              
006900   03    FILLER                  REDEFINES FEL1.                          
007000      05 FEL-1                   PIC X(40)   OCCURS 2.                    
007100                                                                          
007200   03    FEL2.                                                            
007300      05 FILLER                  PIC X(40)                                
007400         VALUE 'INMATNINGSFÄLT FEL IFYLLT'.                               
007500      05 FILLER                  PIC X(40)                                
007600         VALUE 'INPUT FIELD NOT CORRECT'.                                 
007700   03    FILLER                  REDEFINES FEL2.                          
007800      05 FEL-2                   PIC X(40)   OCCURS 2.                    
007900                                                                          
008000   03    FEL3.                                                            
008100      05 FILLER                  PIC X(40)                                
008200         VALUE 'LASTBÄRARTYP FINNS EJ REGISTRERAD'.                       
008300      05 FILLER                  PIC X(40)                                
008400         VALUE 'CARRIER TYPE NOT REGISTERED '.                            
008500   03    FILLER                  REDEFINES FEL3.                          
008600      05 FEL-3                   PIC X(40)   OCCURS 2.                    
008700                                                                          
008800   03    FEL4.                                                            
008900      05 FILLER                  PIC X(40)                                
009000         VALUE 'LASTBÄRARTYP FINNS REDAN REGISTRERAD'.                    
009100      05 FILLER                  PIC X(40)                                
009200         VALUE 'CARRIER TYPE ALREADY REGISTERED'.                         
009300   03    FILLER                  REDEFINES FEL4.                          
009400      05 FEL-4                   PIC X(40)   OCCURS 2.                    
009500                                                                          
009600   03    FEL5.                                                            
009700      05 FILLER                  PIC X(40)                                
009800         VALUE 'LASTBÄRARTYPTEXT EJ IFYLLD'.                              
009900      05 FILLER                  PIC X(40)                                
010000         VALUE 'CARRIER TYPE TEXT NOT REGISTERED'.                        
010100   03    FILLER                  REDEFINES FEL5.                          
010200      05 FEL-5                   PIC X(40)   OCCURS 2.                    
010300                                                                          
010400   03    MED1.                                                            
010500      05 FILLER                  PIC X(40)                                
010600         VALUE 'LASTBÄRARTYPTEXT UPPDATERAD'.                             
010700      05 FILLER                  PIC X(40)                                
010800         VALUE 'CARRIER TYPE TEXT UPDATED'.                               
010900   03    FILLER                  REDEFINES MED1.                          
011000      05 MED-1                   PIC X(40)   OCCURS 2.                    
011100                                                                          
011200   03    MED2.                                                            
011300      05 FILLER                  PIC X(40)                                
011400         VALUE 'LASTBÄRARTYPTEXT BORTTAGEN'.                              
011500      05 FILLER                  PIC X(40)                                
011600         VALUE 'CARRIERTYPE-TEXT DELETED'.                                
011700   03    FILLER                  REDEFINES MED2.                          
011800      05 MED-2                   PIC X(40)   OCCURS 2.                    
011900                                                                          
012000   03    MED3.                                                            
012100      05 FILLER                  PIC X(40)                                
012200         VALUE 'LASTBÄRARTYPTEXT REGISTRERAD'.                            
012300      05 FILLER                  PIC X(40)                                
012400         VALUE 'CARRIER TYPE TEXT REGISTERED'.                            
012500   03    FILLER                  REDEFINES MED3.                          
012600      05 MED-3                   PIC X(40)   OCCURS 2.                    
012700                                                                          
012800     EJECT                                                                
012900******************************************************************        
013000*                                                                         
013100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013200*                                                                         
013300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
013400     SKIP3                                                                
013500*01    MID -COPY W0I80801 -PRE MID-.                                      
013700     EJECT                                                                
013800*01    -COPY WMSGAREA                                                     
014000     EJECT                                                                
014100*  03    MOD -COPY W0O80801 -PRE MOD- -RED MSG-AREA.                      
014300     EJECT                                                                
014400*01    -COPY WMFSAREA                                                     
014600     EJECT                                                                
014700******************************************************************        
014800*                                                                         
014900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015000*                                                                         
015100 01    IMS-WS.                                                            
015200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
015300     SKIP3                                                                
015400*                        **** STATUS-KOD FRÅN IMS                         
015500   03    STATUS-WS               PIC XX.                                  
015600     88    SEGMENT-FINNS                     VALUE '  '.                  
015700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
015800     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
015900     SKIP3                                                                
016000   03    GODK-STATUSKODER.                                                
016100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
016200     SKIP3                                                                
016300 01    SSA1                      PIC X(64).                               
016400 01    SSA2                      PIC X(64).                               
016500     EJECT                                                                
016600*                            IMS FUNKTIONSKODER                           
016700*01    -COPY W0003                                                        
016900     EJECT                                                                
017000*                            DLI INPUT-OUTPUT AREA                        
017100 01    DLI-IO-AREA.                                                       
017200   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
017300     SKIP3                                                                
017400*  03    WL473901 -COPY WDGX473I -RED IO-AREA.                            
017600     EJECT                                                                
017700*  03    WL473911 -COPY WDGX4739 -RED IO-AREA.                            
017900     EJECT                                                                
018000 LINKAGE SECTION.                                                         
018100*01    -COPY W0009     -PRE MSG-                                          
018300     EJECT                                                                
018400*01    -COPY W0008     -PRE 4739-                                         
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800 PROCEDURE DIVISION USING MSG-PCB 4739-PCB.                               
018900     ENTRY 'DLITCBL' USING MSG-PCB 4739-PCB                               
019000     PERFORM IMS-GET-MSG                                                  
019100     IF SEGMENT-FINNS                                                     
019200       PERFORM A-INIT-SPARA-INPUT                                         
019300       IF WS-GODKAEND-BILD                                                
019400         IF KDLBTYP-WS NOT NUMERIC                                        
019500           MOVE FEL-1 (SPRAK-IX) TO MOD-MESSAGE-RAD1                      
019600         ELSE                                                             
019700           MOVE KDLBTYP-WS TO W-473I-KDLBTYP                              
019800           IF MFS-UPDATE  AND UPPDAT-GODK                                 
019900             IF ANDRING                                                   
020000               PERFORM C-ANDRING                                          
020100             ELSE                                                         
020200               EVALUATE TRUE                                              
020300               WHEN BORTTAG                                               
020400                 PERFORM D-BORTTAG                                        
020500               WHEN NYUPPLAGGNING                                         
020600                 PERFORM E-NYUPPLAGGNING                                  
020700                WHEN OTHER                                                
020800                 MOVE FEL-2 (SPRAK-IX) TO MOD-MESSAGE-RAD1                
020900               END-EVALUATE                                               
021000             END-IF                                                       
021100           ELSE                                                           
021200             PERFORM IMS-LAS-BARN                                         
021300             PERFORM B-LAS                                                
021400           END-IF                                                         
021500         END-IF                                                           
021600       ELSE                                                               
021700         MOVE MFS-RENSA-FAELT TO MOD-KDLBTYP-UT                           
021800                                 MOD-UPPDAT-TYP-UT                        
021900       END-IF                                                             
022000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
022100       PERFORM IMS-INSERT-MSG                                             
022200     END-IF                                                               
022300     MOVE ZERO TO RETURN-CODE                                             
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT-SPARA-INPUT SECTION.                                              
022800     IF MSG-DUBBLA-TRANSKODER                                             
022900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80801                 
023000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
023300     ELSE                                                                 
023400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80801                  
023500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023700       MOVE ' ' TO MFS-KDTRTYP                                            
023800     END-IF                                                               
023900     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023910     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W0O80801 + 4                  
024000     IF ENGLISH-TEXT                                                      
024100       MOVE +2 TO SPRAK-IX                                                
024200     ELSE                                                                 
024300       MOVE +1 TO SPRAK-IX                                                
024400     END-IF                                                               
024500     MOVE JA TO UPPDATERING                                               
024600     IF MID-KDLBTYP-IN = ALL '+'                                          
024700       MOVE MID-KDLBTYP-UT TO KDLBTYP-WS                                  
024800       INSPECT KDLBTYP-WS REPLACING ALL SPACE BY ZERO                     
024900     ELSE                                                                 
025000       MOVE MID-KDLBTYP-IN TO KDLBTYP-WS                                  
025100       MOVE NEJ TO UPPDATERING                                            
025200     END-IF                                                               
025300     IF MID-UPPDAT-TYP-IN = ALL '+'                                       
025400       MOVE MID-UPPDAT-TYP-UT TO UPPDAT-TYP-WS                            
025500     ELSE                                                                 
025600       MOVE MID-UPPDAT-TYP-IN TO UPPDAT-TYP-WS                            
025700       MOVE NEJ TO UPPDATERING                                            
025800     END-IF                                                               
025900     MOVE LOW-VALUE TO MSG-AREA                                           
026000     MOVE 'W0O80801' TO MFS-IDMOD                                         
026100     MOVE '0808' TO MOD-IDTRANS                                           
026200     MOVE KDLBTYP-WS TO MOD-KDLBTYP-UT                                    
026300     INSPECT MOD-KDLBTYP-UT REPLACING LEADING ZERO BY SPACE               
026400     MOVE UPPDAT-TYP-WS TO MOD-UPPDAT-TYP-UT                              
026500     MOVE MFS-RENSA-FAELT TO MOD-KDLBTYP-IN                               
026600                             MOD-UPPDAT-TYP-IN                            
026700                             MOD-MESSAGE-RAD1                             
026800                             MOD-MESSAGE-RAD23                            
026900     .                                                                    
027000     EJECT                                                                
027100 B-LAS SECTION.                                                           
027200     IF SEGMENT-FINNS                                                     
027300       MOVE LBTYP-BELBTYP (1) TO MOD-BELBTYP-SVE                          
027400       MOVE LBTYP-BELBTYP (2) TO MOD-BELBTYP-ENG                          
027500       MOVE LBTYP-BELBTYP (3) TO MOD-BELBTYP-FRA                          
027600       MOVE LBTYP-BELBTYP (4) TO MOD-BELBTYP-SPA                          
027700       MOVE LBTYP-BELBTYP (5) TO MOD-BELBTYP-TYS                          
027710       MOVE LBTYP-BELBTYP (6) TO MOD-BELBTYP-ITA                          
027800     ELSE                                                                 
027900       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 C-ANDRING SECTION.                                                       
028400     PERFORM IMS-LAS-BARN                                                 
028500     IF SEGMENT-FINNS                                                     
028600       IF MID-BELBTYP-SVE NOT = ALL '+'                                   
028700         MOVE MID-BELBTYP-SVE TO LBTYP-BELBTYP (1)                        
028800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-SVE-ATTR               
028900         MOVE JA TO BELBTYP-INMATAD                                       
029000       END-IF                                                             
029100       IF MID-BELBTYP-ENG NOT = ALL '+'                                   
029200         MOVE MID-BELBTYP-ENG TO LBTYP-BELBTYP (2)                        
029300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-ENG-ATTR               
029400         MOVE JA TO BELBTYP-INMATAD                                       
029500       END-IF                                                             
029600       IF MID-BELBTYP-FRA NOT = ALL '+'                                   
029700         MOVE MID-BELBTYP-FRA TO LBTYP-BELBTYP (3)                        
029800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-FRA-ATTR               
029900         MOVE JA TO BELBTYP-INMATAD                                       
030000       END-IF                                                             
030100       IF MID-BELBTYP-SPA NOT = ALL '+'                                   
030200         MOVE MID-BELBTYP-SPA TO LBTYP-BELBTYP (4)                        
030300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-SPA-ATTR               
030400         MOVE JA TO BELBTYP-INMATAD                                       
030500       END-IF                                                             
030600       IF MID-BELBTYP-TYS NOT = ALL '+'                                   
030700         MOVE MID-BELBTYP-TYS TO LBTYP-BELBTYP (5)                        
030800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-TYS-ATTR               
030900         MOVE JA TO BELBTYP-INMATAD                                       
031000       END-IF                                                             
031010       IF MID-BELBTYP-ITA NOT = ALL '+'                                   
031020         MOVE MID-BELBTYP-ITA TO LBTYP-BELBTYP (6)                        
031030         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-ita-ATTR               
031040         MOVE JA TO BELBTYP-INMATAD                                       
031050       END-IF                                                             
031100       IF BELBTYP-INMATAD = JA                                            
031200         PERFORM IMS-UPPDATERA                                            
031300       END-IF                                                             
031400       MOVE MED-1 (SPRAK-IX) TO MOD-MESSAGE-RAD23                         
031500       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SVE                          
031600       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ENG                          
031700       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-FRA                          
031800       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SPA                          
031900       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-TYS                          
031910       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ITA                          
032000     ELSE                                                                 
032100       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 D-BORTTAG SECTION.                                                       
032600     PERFORM IMS-LAS-ROT                                                  
032700     IF SEGMENT-SAKNAS                                                    
032800       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
032900     ELSE                                                                 
033000       PERFORM IMS-BORTTAG                                                
033100       MOVE MED-2 (SPRAK-IX) TO MOD-MESSAGE-RAD23                         
033200       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-SVE                            
033300       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-ENG                            
033400       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-FRA                            
033500       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-SPA                            
033600       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-TYS                            
033610       MOVE MFS-RENSA-FAELT TO MOD-BELBTYP-ITA                            
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 E-NYUPPLAGGNING SECTION.                                                 
034100     MOVE W-473I-IDHTYP       TO 473I-IDHTYP                              
034200     MOVE W-473I-KDLBTYP      TO 473I-KDLBTYP                             
034300     MOVE W-473I-LOWVALUE     TO 473I-LOWVALUE                            
034400     PERFORM IMS-NYUPPLAGGNING-ROT                                        
034500     IF SEGMENT-FINNS-REDAN                                               
034600       MOVE FEL-4 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
034700       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SVE                          
034800       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ENG                          
034900       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-FRA                          
035000       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SPA                          
035100       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-TYS                          
035110       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ITA                          
035200     ELSE                                                                 
035300       IF MID-BELBTYP-SVE NOT = ALL '+'                                   
035400         MOVE MID-BELBTYP-SVE TO LBTYP-BELBTYP (1)                        
035500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-SVE-ATTR               
035600         MOVE JA TO BELBTYP-INMATAD                                       
035700       END-IF                                                             
035800       IF MID-BELBTYP-ENG NOT = ALL '+'                                   
035900         MOVE MID-BELBTYP-ENG TO LBTYP-BELBTYP (2)                        
036000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-ENG-ATTR               
036100         MOVE JA TO BELBTYP-INMATAD                                       
036200       END-IF                                                             
036300       IF MID-BELBTYP-FRA NOT = ALL '+'                                   
036400         MOVE MID-BELBTYP-FRA TO LBTYP-BELBTYP (3)                        
036500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-FRA-ATTR               
036600         MOVE JA TO BELBTYP-INMATAD                                       
036700       END-IF                                                             
036800       IF MID-BELBTYP-SPA NOT = ALL '+'                                   
036900         MOVE MID-BELBTYP-SPA TO LBTYP-BELBTYP (4)                        
037000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-SPA-ATTR               
037100         MOVE JA TO BELBTYP-INMATAD                                       
037200       END-IF                                                             
037300       IF MID-BELBTYP-TYS NOT = ALL '+'                                   
037400         MOVE MID-BELBTYP-TYS TO LBTYP-BELBTYP (5)                        
037500         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-TYS-ATTR               
037600         MOVE JA TO BELBTYP-INMATAD                                       
037700       END-IF                                                             
037710       IF MID-BELBTYP-ITA NOT = ALL '+'                                   
037720         MOVE MID-BELBTYP-ITA TO LBTYP-BELBTYP (6)                        
037730         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELBTYP-ITA-ATTR               
037740         MOVE JA TO BELBTYP-INMATAD                                       
037750       END-IF                                                             
037800       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SVE                          
037900       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ENG                          
038000       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-FRA                          
038100       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-SPA                          
038200       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-TYS                          
038210       MOVE MFS-ROER-EJ-FAELT TO MOD-BELBTYP-ITA                          
038300       IF BELBTYP-INMATAD = JA                                            
038400         MOVE '1' TO LBTYP-KDSEGKEY                                       
038500         PERFORM IMS-NYUPPLAGGNING-BARN                                   
038600         MOVE MED-3 (SPRAK-IX) TO MOD-MESSAGE-RAD23                       
038700       ELSE                                                               
038800         MOVE FEL-5 (SPRAK-IX) TO MOD-MESSAGE-RAD1                        
038900       END-IF                                                             
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300* IMS SEKTIONER                                                           
039400     SKIP3                                                                
039500 IMS-GET-MSG SECTION.                                                     
039600     MOVE '  QC' TO GODK-STATUSKODER                                      
039700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-INSERT-MSG SECTION.                                                  
040300     IF ENGLISH-TEXT                                                      
040400       MOVE 'N' TO MFS-KDHUVOMR                                           
040500     END-IF                                                               
040600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040700     MOVE SPACE TO GODK-STATUSKODER                                       
040800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041000     PERFORM IMS-STATUSKONTROLL                                           
041100     .                                                                    
041200     EJECT                                                                
041300 IMS-LAS-BARN SECTION.                                                    
041400     STRING 'WL473901(WDGXKEY  =' W-WDGXKEY-X ')'                         
041500            DELIMITED BY SIZE INTO SSA1                                   
041600     MOVE 'WL473911' TO SSA2                                              
041700     MOVE '  GE' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GHU 4739-PCB DLI-IO-AREA SSA1 SSA2                
041900     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     SKIP3                                                                
042300 IMS-LAS-ROT SECTION.                                                     
042400     STRING 'WL473901(WDGXKEY  =' W-WDGXKEY-X ')'                         
042500            DELIMITED BY SIZE INTO SSA1                                   
042600     MOVE '  GE' TO GODK-STATUSKODER                                      
042700     CALL CBLTDLI USING GHU 4739-PCB DLI-IO-AREA SSA1                     
042800     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
042900     PERFORM IMS-STATUSKONTROLL                                           
043000     .                                                                    
043100     SKIP3                                                                
043200 IMS-UPPDATERA SECTION.                                                   
043300     MOVE '  ' TO GODK-STATUSKODER                                        
043400     CALL CBLTDLI USING REPL 4739-PCB DLI-IO-AREA                         
043500     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
043600     PERFORM IMS-STATUSKONTROLL                                           
043700     .                                                                    
043800     EJECT                                                                
043900 IMS-BORTTAG SECTION.                                                     
044000     MOVE '  ' TO GODK-STATUSKODER                                        
044100     CALL CBLTDLI USING DLET 4739-PCB DLI-IO-AREA                         
044200     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500     EJECT                                                                
044600 IMS-NYUPPLAGGNING-ROT SECTION.                                           
044700     MOVE 'WL473901 ' TO SSA1                                             
044800     MOVE '  II' TO GODK-STATUSKODER                                      
044900     CALL CBLTDLI USING ISRT 4739-PCB DLI-IO-AREA SSA1                    
045000     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
045100     PERFORM IMS-STATUSKONTROLL                                           
045200     .                                                                    
045300 IMS-NYUPPLAGGNING-BARN SECTION.                                          
045400     STRING 'WL473901(WDGXKEY  =' W-WDGXKEY-X ')'                         
045500            DELIMITED BY SIZE INTO SSA1                                   
045600     MOVE 'WL473911 ' TO SSA2                                             
045700     MOVE '  ' TO GODK-STATUSKODER                                        
045800     CALL CBLTDLI USING ISRT 4739-PCB DLI-IO-AREA SSA1 SSA2               
045900     MOVE 4739-STATUS-CODE TO STATUS-WS                                   
046000     PERFORM IMS-STATUSKONTROLL                                           
046100     .                                                                    
046200     EJECT                                                                
046300 IMS-STATUSKONTROLL SECTION.                                              
046400     SET STATUS-IX TO 1                                                   
046500     SEARCH GODK-STATUS                                                   
046510       AT END                                                             
046520         CALL FELLOG                                                      
046600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
046700     END-SEARCH                                                           
046900     .                                                                    
