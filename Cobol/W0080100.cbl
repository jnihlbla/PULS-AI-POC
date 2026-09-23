000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0080100.                                                
000400 AUTHOR.         STIG MULLER.                                             
000500 DATE-WRITTEN.   JULI 85.                                                 
000600                                                                          
000700*    FUNKTION.                                                            
000800*       IMSDC FRÅGE OCH UPPDATERINGSPROGRAM                               
000900*       FÖR PERSONKODS TEXTER PÅ WDP3                                     
001000                                                                          
001100*    INDATA.                                                              
001200*        TRANSAKTION: W0T801                                              
001300*                     W0T801U                                             
001400                                                                          
001500*        MID:         W0I80101                                            
001600                                                                          
001700*    UTDATA.                                                              
001800*        MOD:         W0O80101                                            
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77    IDPGM                     PIC X(8)    VALUE 'W0080100'.            
002800 77    JA                        PIC X       VALUE 'J'.                   
002900 77    NEJ                       PIC X       VALUE 'N'.                   
003000 77    FAELT-INMATAT             PIC X       VALUE 'N'.                   
003100 77    IDPERSON-WS               PIC X(3).                                
003200 77    KDARBTYP-WS               PIC X(8).                                
003300 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +262 COMP SYNC.        
003400 77    WS-IDTRANS                PIC X(4).                                
003500       88  WS-GODKAEND-BILD                  VALUE '0801'.                
003600*- - - - - - - - - - - - - - - - -INDEX RÄKNARE                           
003700 77    IX                        PIC S9(9)   VALUE +0 COMP SYNC.          
003800 77    INDX                      PIC S9(9)   VALUE +0 COMP SYNC.          
003900 77    SPRAK-IX                  PIC S9(9)   VALUE +0 COMP SYNC.          
004000*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
004100     SKIP3                                                                
004200 01  UPPDAT-TYP-WS               PIC X.                                   
004300   88  ANDRING          VALUE 'Ä' 'C'.                                    
004400   88  BORTTAG          VALUE 'B' 'D'.                                    
004500   88  NYUPPLAGGNING    VALUE 'N'.                                        
004600     SKIP3                                                                
004700 01  UPPDATERING                 PIC X.                                   
004800   88  UPPDAT-GODK      VALUE 'J'.                                        
004900 01  NYA-NYCKLAR-SW              PIC X VALUE 'N'.                         
005000   88  NYA-NYCKLAR      VALUE 'J'.                                        
005100   88  GAMLA-NYCKLAR    VALUE 'N'.                                        
005110 01  WS-IDMAIL-CORRECT           PIC X VALUE 'J'.                         
005120   88  IDMAIL-CORRECT   VALUE 'J' 'Y'.                                    
005130   88  IDMAIL-INCORRECT VALUE 'N'.                                        
005200     EJECT                                                                
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400     03 CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.           
005500     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
005600     03 FELLOG                    PIC X(8)    VALUE 'FELLOG  '.           
005700     SKIP2                                                                
005800 01  NYCKLAR-TILL-DLI.                                                    
005900   03    W-KDARBTYP-X.                                                    
006100     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
006200                                                                          
006300   03    W-IDPERSON-X.                                                    
006400     05    W-IDPERSON             PIC S9(3)   COMP-3 VALUE +0.            
006500     SKIP3                                                                
006600 01    MEDDELANDE.                                                        
006700   03    FEL1.                                                            
006800      05 FILLER                  PIC X(40)                                
006900         VALUE 'FEL NYCKEL'.                                              
007000      05 FILLER                  PIC X(40)                                
007100         VALUE 'WRONG KEY '.                                              
007200   03    FILLER                  REDEFINES FEL1.                          
007300      05 FEL-1                   PIC X(40)   OCCURS 2.                    
007400                                                                          
007500   03    FEL2.                                                            
007600      05 FILLER                  PIC X(40)                                
007700         VALUE 'INMATNINGSFÄLT FEL IFYLLT'.                               
007800      05 FILLER                  PIC X(40)                                
007900         VALUE 'INPUT FIELD NOT CORRECT'.                                 
008000   03    FILLER                  REDEFINES FEL2.                          
008100      05 FEL-2                   PIC X(40)   OCCURS 2.                    
008200                                                                          
008300   03    FEL3.                                                            
008400      05 FILLER                  PIC X(40)                                
008500         VALUE 'PERSONKOD FINNS EJ REGISTRERAD'.                          
008600      05 FILLER                  PIC X(40)                                
008700         VALUE 'PERSONAL CODE NOT REGISTRED   '.                          
008800   03    FILLER                  REDEFINES FEL3.                          
008900      05 FEL-3                   PIC X(40)   OCCURS 2.                    
009000                                                                          
009100   03    FEL4.                                                            
009200      05 FILLER                  PIC X(40)                                
009300         VALUE 'PERSONKOD FINNS REDAN REGISTRERAD'.                       
009400      05 FILLER                  PIC X(40)                                
009500         VALUE 'PERSONAL CODE ALREADY REGISTRED  '.                       
009600   03    FILLER                  REDEFINES FEL4.                          
009700      05 FEL-4                   PIC X(40)   OCCURS 2.                    
009800                                                                          
009900   03    FEL5.                                                            
010000      05 FILLER                  PIC X(40)                                
010100         VALUE 'PERSONDATA EJ IFYLLD '.                                   
010200      05 FILLER                  PIC X(40)                                
010300         VALUE 'PERSONAL DATA NOT REGISTRED'.                             
010400   03    FILLER                  REDEFINES FEL5.                          
010500      05 FEL-5                   PIC X(40)   OCCURS 2.                    
010600                                                                          
010700   03    FEL6.                                                            
010800      05 FILLER                  PIC X(40)                                
010900         VALUE 'ARBETSTYP SAKNAS     '.                                   
011000      05 FILLER                  PIC X(40)                                
011100         VALUE 'WORKTYPE MISSING           '.                             
011200   03    FILLER                  REDEFINES FEL6.                          
011300      05 FEL-6                   PIC X(40)   OCCURS 2.                    
011400                                                                          
011500   03    FEL7.                                                            
011600      05 FILLER                  PIC X(40)                                
011700         VALUE 'FELAKTIG E-MAIL ID'.                                      
011800      05 FILLER                  PIC X(40)                                
011900         VALUE 'INVALID E-MAIL ID'.                                       
012000   03    FILLER                  REDEFINES FEL7.                          
012100      05 FEL-7                   PIC X(40)   OCCURS 2.                    
012200                                                                          
012300   03    MED1.                                                            
012400      05 FILLER                  PIC X(40)                                
012500         VALUE 'PERSONDATA UPPDATERAD'.                                   
012600      05 FILLER                  PIC X(40)                                
012700         VALUE 'PERSONAL DATA UPDATED'.                                   
012800   03    FILLER                  REDEFINES MED1.                          
012900      05 MED-1                   PIC X(40)   OCCURS 2.                    
013000                                                                          
013100   03    MED2.                                                            
013200      05 FILLER                  PIC X(40)                                
013300         VALUE 'PERSONDATA BORTTAGEN '.                                   
013400      05 FILLER                  PIC X(40)                                
013500         VALUE 'PERSONAL DATA DELETED'.                                   
013600   03    FILLER                  REDEFINES MED2.                          
013700      05 MED-2                   PIC X(40)   OCCURS 2.                    
013800                                                                          
013900   03    MED3.                                                            
014000      05 FILLER                  PIC X(40)                                
014100         VALUE 'PERSONDATA REGISTRERAD'.                                  
014200      05 FILLER                  PIC X(40)                                
014300         VALUE 'PERSONAL DATA REGISTRED'.                                 
014400   03    FILLER                  REDEFINES MED3.                          
014500      05 MED-3                   PIC X(40)   OCCURS 2.                    
014600                                                                          
014700   03    MED4.                                                            
014800      05 FILLER                  PIC X(40)                                
014900         VALUE 'TRYCK PF11 FÖR UPPDATERING'.                              
015000      05 FILLER                  PIC X(40)                                
015100         VALUE 'PRESS PF11 FOR UPPDATE'.                                  
015200   03    FILLER                  REDEFINES MED4.                          
015300      05 MED-4                   PIC X(40)   OCCURS 2.                    
015400                                                                          
015500     EJECT                                                                
015600******************************************************************        
015700*                                                                         
015800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015900*                                                                         
016000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
016100     SKIP3                                                                
016200*01    MID -COPY W0I80101 -PRE MID-.                                      
016300     EJECT                                                                
016400*01    -COPY WMSGAREA                                                     
016500     EJECT                                                                
016600*    03  MOD -COPY W0O80101 -PRE MOD- -RED MSG-AREA.                      
016700     EJECT                                                                
016800*    -- PARAMS FOR SUBPROG W009EMAD (E-MAIL VALIDATION/COMPLETION)        
016900*                                                                         
017000 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
017100*01 -COPY W009EMAD                                                        
017200     EJECT                                                                
017300*01    -COPY WMFSAREA                                                     
017400     EJECT                                                                
017500******************************************************************        
017600*                                                                         
017700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017800*                                                                         
017900 01    IMS-WS.                                                            
018000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
018100     SKIP3                                                                
018200*                        **** STATUS-KOD FRÅN IMS                         
018300   03    STATUS-WS               PIC XX.                                  
018400     88    SEGMENT-FINNS                     VALUE '  '.                  
018500     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
018600     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
018700     SKIP3                                                                
018800   03    GODK-STATUSKODER.                                                
018900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
019000     SKIP3                                                                
019100 01    SSA1                      PIC X(64).                               
019200 01    SSA2                      PIC X(64).                               
019300     EJECT                                                                
019400*                            IMS FUNKTIONSKODER                           
019500*01    -COPY W0003                                                        
019600     EJECT                                                                
019700*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
019810 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P301'.         
019820     SKIP3                                                                
019830 01  DLI-IO-P301.                                                         
019840*    03  -COPY WDP301                                                     
019850     EJECT                                                                
019860 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
019870     SKIP3                                                                
019880 01  DLI-IO-P311.                                                         
019890*    03  -COPY WDP311                                                     
019900     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500*01    -COPY W0009     -PRE MSG-                                          
020600     EJECT                                                                
020700*01    -COPY W0008     -PRE WDP3-                                         
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION USING MSG-PCB WDP3-PCB.                               
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING MSG-PCB WDP3-PCB.                              
021300     PERFORM IMS-GET-MSG                                                  
021400     IF SEGMENT-FINNS                                                     
021500       PERFORM A-INIT-SPARA-INPUT                                         
021600       IF WS-GODKAEND-BILD                                                
021700                                                                          
021800         IF IDPERSON-WS NOT NUMERIC                                       
021900           MOVE FEL-1 (SPRAK-IX) TO MOD-MESSAGE-RAD1                      
022000         ELSE                                                             
022100           MOVE KDARBTYP-WS TO W-KDARBTYP                                 
022200           MOVE IDPERSON-WS TO W-IDPERSON                                 
022300           PERFORM IMS-GU-WDP301                                          
022400           IF SEGMENT-FINNS                                               
022500             IF MFS-UPDATE  AND UPPDAT-GODK                               
022600               IF ANDRING                                                 
022700                 PERFORM C-ANDRING                                        
022800               ELSE                                                       
022900                 EVALUATE TRUE                                            
023000                 WHEN BORTTAG                                             
023100                   PERFORM D-BORTTAG                                      
023200                 WHEN NYUPPLAGGNING                                       
023300                   PERFORM E-NYUPPLAGGNING                                
023400                  WHEN OTHER                                              
023500                   MOVE FEL-2 (SPRAK-IX) TO MOD-MESSAGE-RAD1              
023600                   PERFORM MFS-LAES-IN-IGEN                               
023700                   PERFORM MFS-ROER-EJ-INM-FAELT                          
023800                 END-EVALUATE                                             
023900               END-IF                                                     
024000             ELSE                                                         
024100               IF MID-INPUT = ALL '+' OR NYA-NYCKLAR                      
024200                 PERFORM IMS-GHNP-WDP311                                  
024300                 PERFORM B-LAS                                            
024400               ELSE                                                       
024500                 PERFORM MFS-LAES-IN-IGEN                                 
024600                 PERFORM MFS-ROER-EJ-INM-FAELT                            
024700                 MOVE MED-4 (SPRAK-IX) TO MOD-MESSAGE-RAD1                
024800               END-IF                                                     
024900             END-IF                                                       
025000           ELSE                                                           
025100             PERFORM MFS-RENSA-INM-FAELT                                  
025200             MOVE FEL-6 (SPRAK-IX) TO MOD-MESSAGE-RAD1                    
025300           END-IF                                                         
025400         END-IF                                                           
025500       ELSE                                                               
025600         MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                          
025700                                 MOD-IDPERSON-UT                          
025800                                 MOD-UPPDAT-TYP-UT                        
025900       END-IF                                                             
026000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
026100       PERFORM IMS-INSERT-MSG                                             
026200     END-IF                                                               
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT-SPARA-INPUT SECTION.                                              
026800     SKIP2                                                                
026900     IF MSG-DUBBLA-TRANSKODER                                             
027000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80101                 
027100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027300       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
027400     ELSE                                                                 
027500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80101                  
027600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027800       MOVE SPACE TO MFS-KDTRTYP                                          
027900     END-IF                                                               
028000     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
028100     IF ENGLISH-TEXT                                                      
028200       MOVE +2 TO SPRAK-IX                                                
028300     ELSE                                                                 
028400       MOVE +1 TO SPRAK-IX                                                
028500     END-IF                                                               
028600     MOVE JA TO UPPDATERING                                               
028700     IF MID-KDARBTYP-IN = ALL '+'                                         
028800       MOVE MID-KDARBTYP-UT TO KDARBTYP-WS                                
028900     ELSE                                                                 
029000       MOVE MID-KDARBTYP-IN TO KDARBTYP-WS                                
029100       MOVE NEJ TO UPPDATERING                                            
029200       MOVE JA  TO NYA-NYCKLAR-SW                                         
029300     END-IF                                                               
029400     IF MID-IDPERSON-IN = ALL '+'                                         
029500       MOVE MID-IDPERSON-UT TO IDPERSON-WS                                
029600       INSPECT IDPERSON-WS REPLACING ALL SPACE BY ZERO                    
029700     ELSE                                                                 
029800       MOVE MID-IDPERSON-IN TO IDPERSON-WS                                
029900       MOVE NEJ TO UPPDATERING                                            
030000       MOVE JA  TO NYA-NYCKLAR-SW                                         
030100     END-IF                                                               
030200     IF MID-UPPDAT-TYP-IN = ALL '+'                                       
030300       MOVE MID-UPPDAT-TYP-UT TO UPPDAT-TYP-WS                            
030400     ELSE                                                                 
030500       MOVE MID-UPPDAT-TYP-IN TO UPPDAT-TYP-WS                            
030600       MOVE NEJ TO UPPDATERING                                            
030700     END-IF                                                               
030800     IF MID-IDAVD = ALL '+'                                               
030900       CONTINUE                                                           
031000     ELSE                                                                 
031100       IF MID-IDAVD NUMERIC                                               
031200         CONTINUE                                                         
031300       ELSE                                                               
031400         MOVE NEJ TO UPPDATERING                                          
031500       END-IF                                                             
031600     END-IF                                                               
031700     MOVE LOW-VALUE TO MSG-AREA                                           
031800     MOVE 'W0O80101' TO MFS-IDMOD                                         
031900     MOVE '0801' TO MOD-IDTRANS                                           
032000     MOVE KDARBTYP-WS TO MOD-KDARBTYP-UT                                  
032100     MOVE IDPERSON-WS TO MOD-IDPERSON-UT                                  
032200     MOVE UPPDAT-TYP-WS TO MOD-UPPDAT-TYP-UT                              
032300     INSPECT MOD-IDPERSON-UT REPLACING LEADING ZERO BY SPACE              
032400     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
032500                             MOD-IDPERSON-IN                              
032600                             MOD-UPPDAT-TYP-IN                            
032700                             MOD-MESSAGE-RAD1                             
032800                             MOD-MESSAGE-RAD23                            
032900     .                                                                    
033000     EJECT                                                                
033100 B-LAS SECTION.                                                           
033200     SKIP2                                                                
033300     IF SEGMENT-FINNS                                                     
033400       MOVE PERS-BEINIT   TO MOD-BEINIT                                   
033500       MOVE PERS-IDNAMN   TO MOD-IDNAMN                                   
033600       MOVE PERS-IDTFN    TO MOD-IDTFN                                    
033700       MOVE PERS-IDAVD    TO MOD-IDAVD                                    
033800       MOVE PERS-IDMAIL   TO MOD-IDMAIL                                   
033900       PERFORM MFS-FORMATETS-ATTRIBUT                                     
034000     ELSE                                                                 
034100       PERFORM MFS-FORMATETS-ATTRIBUT                                     
034200       PERFORM MFS-RENSA-INM-FAELT                                        
034300       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 C-ANDRING SECTION.                                                       
034800     SKIP2                                                                
034900     PERFORM IMS-GHNP-WDP311                                              
035000     IF SEGMENT-FINNS                                                     
035100       IF MID-BEINIT   NOT = ALL '+'                                      
035200         MOVE MID-BEINIT    TO PERS-BEINIT                                
035300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEINIT-ATTR                    
035400         MOVE JA TO FAELT-INMATAT                                         
035500       ELSE                                                               
035600         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-BEINIT-ATTR                    
035700       END-IF                                                             
035800       IF MID-IDTFN   NOT = ALL '+'                                       
035900         MOVE MID-IDTFN     TO PERS-IDTFN                                 
036000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTFN-ATTR                     
036100         MOVE JA TO FAELT-INMATAT                                         
036200       ELSE                                                               
036300         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDTFN-ATTR                     
036400       END-IF                                                             
036500       IF MID-IDNAMN  NOT = ALL '+'                                       
036600         MOVE MID-IDNAMN    TO PERS-IDNAMN                                
036700         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDNAMN-ATTR                    
036800         MOVE JA TO FAELT-INMATAT                                         
036900       ELSE                                                               
037000         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDNAMN-ATTR                    
037100       END-IF                                                             
037200       IF MID-IDAVD   NOT = ALL '+'                                       
037300         MOVE MID-IDAVD     TO PERS-IDAVD                                 
037400         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAVD-ATTR                     
037500         MOVE JA TO FAELT-INMATAT                                         
037600       ELSE                                                               
037700         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDAVD-ATTR                     
037800       END-IF                                                             
037900       IF MID-IDMAIL    NOT = ALL '+'                                     
038100         MOVE MID-IDMAIL             TO EMAD-IDMAIL                       
038200         CALL W009EMAD USING EMAD-W009EMAD                                
038300         MOVE EMAD-IDMAIL  TO MID-IDMAIL  PERS-IDMAIL MOD-IDMAIL          
038400         IF EMAD-KDSVAR > SPACE                                           
038401           MOVE NEJ TO WS-IDMAIL-CORRECT                                  
038402           MOVE FEL-7 (SPRAK-IX) TO MOD-MESSAGE-RAD23                     
038403           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDMAIL-ATTR                   
038410         ELSE                                                             
038420           MOVE JA TO WS-IDMAIL-CORRECT                                   
038600           MOVE MID-IDMAIL  TO PERS-IDMAIL                                
038700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDMAIL-ATTR                  
038800           MOVE JA TO FAELT-INMATAT                                       
038810         END-IF                                                           
038900       ELSE                                                               
039000         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDMAIL-ATTR                    
039100       END-IF                                                             
039200       IF FAELT-INMATAT = JA AND IDMAIL-CORRECT                           
039300         PERFORM IMS-REPL-WDP311                                          
039400       END-IF                                                             
039410       IF IDMAIL-CORRECT                                                  
039500         MOVE MED-1 (SPRAK-IX) TO MOD-MESSAGE-RAD23                       
039510       END-IF                                                             
039600     ELSE                                                                 
039700       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
039800       PERFORM MFS-LAES-IN-IGEN                                           
039900     END-IF                                                               
040000     PERFORM MFS-ROER-EJ-INM-FAELT                                        
040100     .                                                                    
040200     EJECT                                                                
040300 D-BORTTAG SECTION.                                                       
040400     SKIP2                                                                
040510     PERFORM IMS-GHNP-WDP311                                              
040600     IF SEGMENT-SAKNAS                                                    
040700       PERFORM MFS-LAES-IN-IGEN                                           
040800       PERFORM MFS-ROER-EJ-INM-FAELT                                      
040900       MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
041000     ELSE                                                                 
041100       PERFORM IMS-DLET-WDP311                                            
041200       PERFORM MFS-RENSA-INM-FAELT                                        
041300       MOVE MED-2 (SPRAK-IX) TO MOD-MESSAGE-RAD23                         
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 E-NYUPPLAGGNING SECTION.                                                 
041800     SKIP2                                                                
041900     IF MID-IDNAMN NOT = ALL '+'                                          
042000       MOVE MID-IDNAMN    TO PERS-IDNAMN                                  
042100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDNAMN-ATTR                      
042200       MOVE JA TO FAELT-INMATAT                                           
042300     ELSE                                                                 
042400       MOVE SPACE         TO PERS-IDNAMN                                  
042500       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDNAMN-ATTR                      
042600     END-IF                                                               
042700     IF MID-IDTFN NOT = ALL '+'                                           
042800       MOVE MID-IDTFN     TO PERS-IDTFN                                   
042900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTFN-ATTR                       
043000       MOVE JA TO FAELT-INMATAT                                           
043100     ELSE                                                                 
043200       MOVE SPACE         TO PERS-IDTFN                                   
043300       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDTFN-ATTR                       
043400     END-IF                                                               
043500     IF MID-IDAVD NOT = ALL '+'                                           
043600       MOVE MID-IDAVD     TO PERS-IDAVD                                   
043700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAVD-ATTR                       
043800       MOVE JA TO FAELT-INMATAT                                           
043900     ELSE                                                                 
044000       MOVE +0            TO PERS-IDAVD                                   
044100       MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDAVD-ATTR                        
044200     END-IF                                                               
044300     IF MID-BEINIT NOT = ALL '+'                                          
044400       MOVE MID-BEINIT    TO PERS-BEINIT                                  
044500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEINIT-ATTR                      
044600       MOVE JA TO FAELT-INMATAT                                           
044700     ELSE                                                                 
044800       MOVE SPACE         TO PERS-BEINIT                                  
044900       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-BEINIT-ATTR                      
045000     END-IF                                                               
045100     IF MID-IDMAIL   NOT = ALL '+'                                        
045111       MOVE MID-IDMAIL               TO EMAD-IDMAIL                       
045112       CALL W009EMAD USING EMAD-W009EMAD                                  
045113       MOVE EMAD-IDMAIL    TO MID-IDMAIL  PERS-IDMAIL MOD-IDMAIL          
045114       IF EMAD-KDSVAR > SPACE                                             
045115         MOVE NEJ TO WS-IDMAIL-CORRECT                                    
045116         MOVE FEL-7 (SPRAK-IX) TO MOD-MESSAGE-RAD23                       
045117         MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDMAIL-ATTR                   
045118       ELSE                                                               
045119         MOVE JA TO WS-IDMAIL-CORRECT                                     
045120         MOVE MID-IDMAIL    TO PERS-IDMAIL                                
045121         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDMAIL-ATTR                    
045122         MOVE JA TO FAELT-INMATAT                                         
045123       END-IF                                                             
045500     ELSE                                                                 
045600       MOVE SPACE         TO PERS-IDMAIL                                  
045700       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDMAIL-ATTR                      
045800     END-IF                                                               
045900     IF FAELT-INMATAT = JA                                                
046000       MOVE W-IDPERSON TO PERS-IDPERSON                                   
046100       PERFORM IMS-ISRT-WDP311                                            
046200       IF SEGMENT-FINNS-REDAN                                             
046300         MOVE FEL-4 (SPRAK-IX) TO MOD-MESSAGE-RAD1                        
046400         PERFORM MFS-LAES-IN-IGEN                                         
046500         PERFORM MFS-ROER-EJ-INM-FAELT                                    
046600       ELSE                                                               
046700         MOVE MED-3 (SPRAK-IX) TO MOD-MESSAGE-RAD23                       
046800         PERFORM MFS-ROER-EJ-INM-FAELT                                    
046900       END-IF                                                             
047000     ELSE                                                                 
047100       MOVE FEL-5 (SPRAK-IX) TO MOD-MESSAGE-RAD1                          
047200       PERFORM MFS-LAES-IN-IGEN                                           
047300       PERFORM MFS-ROER-EJ-INM-FAELT                                      
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 MFS-LAES-IN-IGEN SECTION.                                                
047800                                                                          
047900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEINIT-ATTR                        
048000                                   MOD-IDNAMN-ATTR                        
048100                                   MOD-IDTFN-ATTR                         
048200                                   MOD-IDAVD-ATTR                         
048300                                   MOD-IDMAIL-ATTR                        
048400     .                                                                    
048500     SKIP3                                                                
048600 MFS-FORMATETS-ATTRIBUT SECTION.                                          
048700                                                                          
048800     MOVE MFS-FORMATETS-ATTR    TO MOD-BEINIT-ATTR                        
048900                                   MOD-IDNAMN-ATTR                        
049000                                   MOD-IDTFN-ATTR                         
049100                                   MOD-IDAVD-ATTR                         
049200                                   MOD-IDMAIL-ATTR                        
049300     .                                                                    
049400     SKIP3                                                                
049500 MFS-ROER-EJ-INM-FAELT SECTION.                                           
049600                                                                          
049700     MOVE MFS-ROER-EJ-FAELT     TO MOD-BEINIT                             
049800                                   MOD-IDNAMN                             
049900                                   MOD-IDTFN                              
050000                                   MOD-IDAVD                              
050100                                   MOD-IDMAIL                             
050200     .                                                                    
050300     SKIP3                                                                
050400 MFS-RENSA-INM-FAELT SECTION.                                             
050500                                                                          
050600     MOVE MFS-RENSA-FAELT   TO MOD-BEINIT                                 
050700                               MOD-IDNAMN                                 
050800                               MOD-IDTFN                                  
050900                               MOD-IDAVD                                  
051000                               MOD-IDMAIL                                 
051100     .                                                                    
051200     EJECT                                                                
051300* IMS SEKTIONER                                                           
051400     SKIP3                                                                
051500 IMS-GET-MSG SECTION.                                                     
051600     MOVE '  QC' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
051800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051900     PERFORM IMS-STATUSKONTROLL                                           
052000     .                                                                    
052100     SKIP3                                                                
052200 IMS-INSERT-MSG SECTION.                                                  
052300     IF ENGLISH-TEXT                                                      
052400       MOVE 'N' TO MFS-KDHUVOMR                                           
052500     END-IF                                                               
052600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
052700     MOVE SPACE TO GODK-STATUSKODER                                       
052800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
052900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200     EJECT                                                                
053300 IMS-GU-WDP301 SECTION.                                                   
053400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
053500            DELIMITED BY SIZE INTO SSA1                                   
053600     MOVE '  GE' TO GODK-STATUSKODER                                      
053700     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P301 SSA1                      
053800     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     .                                                                    
054100     SKIP3                                                                
054200 IMS-GHNP-WDP311 SECTION.                                                 
054500     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
054600            DELIMITED BY SIZE INTO SSA1                                   
054700     MOVE '  GE' TO GODK-STATUSKODER                                      
054800     CALL CBLTDLI USING GHNP WDP3-PCB DLI-IO-P311 SSA1                    
054900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200     SKIP3                                                                
055300 IMS-REPL-WDP311 SECTION.                                                 
055400     MOVE '  ' TO GODK-STATUSKODER                                        
055500     CALL CBLTDLI USING REPL WDP3-PCB DLI-IO-P311                         
055600     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSKONTROLL                                           
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-DLET-WDP311 SECTION.                                                 
056100     MOVE '  ' TO GODK-STATUSKODER                                        
056200     CALL CBLTDLI USING DLET WDP3-PCB DLI-IO-P311                         
056300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     SKIP3                                                                
056700 IMS-ISRT-WDP311 SECTION.                                                 
056800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
056900            DELIMITED BY SIZE INTO SSA1                                   
057000     MOVE 'WDP311' TO SSA2                                                
057100     MOVE '  II' TO GODK-STATUSKODER                                      
057200     CALL CBLTDLI USING ISRT WDP3-PCB DLI-IO-P311 SSA1 SSA2               
057300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
057400     PERFORM IMS-STATUSKONTROLL                                           
057500     .                                                                    
057600     SKIP3                                                                
057700 IMS-STATUSKONTROLL SECTION.                                              
057800     SET STATUS-IX TO 1                                                   
057900     SEARCH GODK-STATUS AT END CALL FELLOG                                
058000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
058100     END-SEARCH                                                           
058200     .                                                                    
