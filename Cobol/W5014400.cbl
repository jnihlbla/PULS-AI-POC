000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5014400.                                                
000400 AUTHOR.         ELAINE CURTSSON.                                         
000500     DATE-WRITTEN.   SEP   91.                                            
000600*                                                                         
000700*    REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        MPP PROGRAM SOM INGÅR I PRISSÄTTNING FÖR EMBALLAGE               
001000*                                SYSTEMET.                                
001100*        MÖJLIGHETER: SÖKNING OCH UPPDATERING AV EN VISS FP-GRUPP         
001200*        PROGRAMMET HETTE TIDIGARE W4011700 /KJH MAJ -95                  
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T144  W5T144U                                     
001600*        MID:         W5I14401                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W5O14401                                            
002000                                                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77    IDPGM                     PIC X(8)    VALUE 'W5014400'.            
002800 77    JA                        PIC X       VALUE 'J'.                   
002900 77    NEJ                       PIC X       VALUE 'N'.                   
003000 77    INDATA-OK                 PIC X       VALUE 'J'.                   
003100 77    NYCKEL-OK                 PIC X       VALUE 'N'.                   
003200 77    UPPDATERAT                PIC X       VALUE 'N'.                   
003300 77    KOSTNAD-FINNS             PIC X       VALUE 'N'.                   
003400 77    KOSTNAD-ZERO              PIC X       VALUE 'N'.                   
003500 77    KOSTNAD-INMATAD           PIC X       VALUE 'N'.                   
003600 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003700 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +582 COMP SYNC.        
003800                                                                          
003900 77    WS-KDFORPGP               PIC X(2)    VALUE SPACE.                 
004000 77    WS-KDFORPGP-NUM           PIC 9(2)    VALUE ZERO.                  
004100 77    WS-KDFORPGP-UPP           PIC X(2)    VALUE SPACE.                 
004200 77    WS-KDFORPGP-UPP-NUM       PIC 9(2)    VALUE ZERO.                  
004300 77    WS-KDFORPGP-SPAR-NUM      PIC 9(2)    VALUE ZERO.                  
004400 77    WS-KDFORPGP-RAD1-NUM      PIC 9(2)    VALUE ZERO.                  
004500                                                                          
004600 77    WS-PRDIRLON               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004700 77    WS-PROVRPAL               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004800 77    WS-PRDMTRL                PIC S9(6)V9(3)  VALUE +0 COMP-3.         
004900                                                                          
005000 01    WS-KDFORPGP-RAD1.                                                  
005100   03    KDFORPGP-RAD1           PIC X(2).                                
005200                                                                          
005300 01    WS-IDTRANS                PIC X(4).                                
005400   88    EGEN-BILD                           VALUE '5144'.                
005500     EJECT                                                                
005600                                                                          
005700 01    GENERELLA-SUBPROGRAM.                                              
005800   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006100   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006200                                                                          
006300*  03 FILLER  -COPY WDECAREA.                                             
006400   EJECT                                                                  
006500                                                                          
006600*  --- PARAMETRAR TILL SUBPGM WMEDKONV                                    
006700*  01 -COPY WMEDAREA.                                                     
006800   EJECT                                                                  
007000 01    NYCKLAR-TILL-DLI.                                                  
007100                                                                          
007200   03    W-WDGXKEY-HTYP-5137-X.                                           
007300     05    W-HTYP                PIC 9(4)    VALUE 5137.                  
007400     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
007500                                                                          
007600   03    W-KDFORPGP-X.                                                    
007700     05    W-KDFORPGP            PIC X(2)    VALUE ZERO.                  
007800   SKIP3                                                                  
007900 01  MESSAGE-CODES.                                                       
008000     03  KOD-KORR-UPPLYSTA-FLT   PIC X(3)   VALUE '001'.                  
008100     03  KOD-TRYCK-PF11          PIC X(3)   VALUE '003'.                  
008200     03  KOD-URVAL-SAKNAS        PIC X(3)   VALUE '005'.                  
008300     03  KOD-PF11-INGET-INDATA   PIC X(3)   VALUE '011'.                  
008400     03  KOD-UPPDATERING-UTFORD  PIC X(3)   VALUE '101'.                  
008500     03  KOD-MER-INFO-FINNS      PIC X(3)   VALUE '105'.                  
008600     03  KOD-SISTA-SIDAN         PIC X(3)   VALUE '106'.                  
008700     EJECT                                                                
008800******************************************************************        
008900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009000*                                                                         
009100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
009200                                                                          
009300*01    MID -COPY W5I14401.                                                
009400     EJECT                                                                
009500*01    -COPY WMSGAREA                                                     
009600     EJECT                                                                
009700*  03    MOD -COPY W5O14401 -RED MSG-AREA.                                
009800     EJECT                                                                
009900*01    -COPY WMFSAREA                                                     
010000     EJECT                                                                
010100******************************************************************        
010200*                                                                         
010300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500 01    IMS-WS.                                                            
010600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
010700     SKIP3                                                                
010800*                        **** STATUS-KOD FRÅN IMS                         
010900   03    STATUS-WS               PIC XX.                                  
011000     88    SEGMENT-FINNS                     VALUE '  '.                  
011100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
011200     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
011300     SKIP3                                                                
011400   03    GODK-STATUSKODER.                                                
011500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011600     SKIP3                                                                
011700 01    SSA1                      PIC X(164).                              
011800 01    SSA2                      PIC X(164).                              
011900     EJECT                                                                
012000*                            IMS FUNKTIONSKODER                           
012100*01    -COPY W0003                                                        
012200     EJECT                                                                
012400 01    DLI-IO-AREA.                                                       
012700*  03    WL513711 -COPY WDGX5138                                          
012800     EJECT                                                                
012900*                                                                         
013000 LINKAGE SECTION.                                                         
013100*01    -COPY W0009     -PRE MSG-                                          
013200     EJECT                                                                
013300*01    -COPY W0008     -PRE 5137-                                         
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION USING MSG-PCB 5137-PCB.                               
013700     ENTRY 'DLITCBL' USING MSG-PCB 5137-PCB.                              
013800                                                                          
013900 STYR SECTION.                                                            
014000                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT-INPUT                                               
014400       PERFORM B-SPARA-INPUT                                              
014500       PERFORM C-NYCKEL-KONTROLL                                          
014600       IF NYCKEL-OK = JA                                                  
014700         IF MFS-UPDATE                                                    
014800           PERFORM D-UPPDATERA                                            
014900         ELSE                                                             
015000           IF MFS-IDPFK = '7'                                             
015100             PERFORM E-LAES-PF7                                           
015200           ELSE                                                           
015300             IF MFS-IDPFK = '8'                                           
015400               PERFORM F-BLAEDDRA-PF8                                     
015500             ELSE                                                         
015600               PERFORM G-ENTER                                            
015700             END-IF                                                       
015800           END-IF                                                         
015900         END-IF                                                           
016000       END-IF                                                             
016100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016200       PERFORM IMS-INSERT-MSG                                             
016300     END-IF                                                               
016310                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 A-INIT-INPUT SECTION.                                                    
017000                                                                          
017100     IF MSG-DUBBLA-TRANSKODER                                             
017200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I14401                 
017300       MOVE MSG-IDTRANS-2            TO MFS-IDTRANS                       
017400       MOVE MSG-KDMFSFOR-2           TO MFS-KDMFSFOR                      
017500       MOVE MSG-KDTRTYP              TO MFS-KDTRTYP                       
017600       MOVE MSG-IDPFK                TO MFS-IDPFK                         
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I14401                 
017900       MOVE MSG-IDTRANS-1            TO MFS-IDTRANS                       
018000       MOVE MSG-KDMFSFOR-1           TO MFS-KDMFSFOR                      
018100       MOVE SPACE                    TO MFS-KDTRTYP                       
018200                                        MFS-IDPFK                         
018300     END-IF                                                               
018400                                                                          
018500     MOVE LOW-VALUE                  TO MSG-AREA                          
018600     MOVE 'W5O14401'                 TO MFS-IDMOD                         
018700     MOVE MFS-IDTRANS                TO WS-IDTRANS                        
018800     MOVE '5144'                     TO MOD-IDTRANS                       
018900                                                                          
019000     IF MFS-KDMFSFOR = '1'                                                
019100       MOVE 'S  '                    TO MED-IDSKYLT                       
019200     ELSE                                                                 
019300       MOVE 'GB '                    TO MED-IDSKYLT                       
019400     END-IF                                                               
019500                                                                          
019600     MOVE MFS-RENSA-FAELT            TO MOD-KDFORPGP-IN                   
019700                                        MOD-KDFORPGP-UPP                  
019800                                        MOD-TEMFSFEL                      
019900                                        MOD-TEMFSINF                      
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300 B-SPARA-INPUT SECTION.                                                   
020400                                                                          
020500     IF MID-KDFORPGP-IN = ALL '+'                                         
020600       MOVE MID-KDFORPGP-UT    TO WS-KDFORPGP                             
020700       INSPECT WS-KDFORPGP REPLACING LEADING SPACE BY ZERO                
020800     ELSE                                                                 
020900       MOVE MID-KDFORPGP-IN    TO WS-KDFORPGP                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE WS-KDFORPGP          TO MOD-KDFORPGP-UT                         
021300                                                                          
021400     IF NOT EGEN-BILD                                                     
021500       MOVE SPACE              TO MOD-KDFORPGP-UT                         
021600                                  WS-KDFORPGP                             
021700       MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL                            
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100                                                                          
022200 C-NYCKEL-KONTROLL SECTION.                                               
022300                                                                          
022400     IF WS-KDFORPGP NUMERIC                                               
022500       MOVE JA                  TO NYCKEL-OK                              
022600     ELSE                                                                 
022700       IF EGEN-BILD                                                       
022800          MOVE KOD-URVAL-SAKNAS TO MED-IDMFSFEL                           
022900          CALL WMEDKONV USING MED-WMEDAREA                                
023000          MOVE MED-MFSFEL       TO MOD-TEMFSFEL                           
023100       END-IF                                                             
023200     END-IF                                                               
023300                                                                          
023400     IF MID-KDFORPGP-UPP NOT = ALL '+'                                    
023500       MOVE MID-KDFORPGP-UPP    TO WS-KDFORPGP-UPP                        
023600       INSPECT WS-KDFORPGP-UPP REPLACING LEADING SPACE BY ZERO            
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024100 D-UPPDATERA SECTION.                                                     
024200                                                                          
024300     PERFORM DA-INMATNING-KONTROLL                                        
024400     IF INDATA-OK = JA                                                    
024500       IF MID-KDCMD-DELETE                                                
024600         PERFORM DB-RADERA-KDFORPGP                                       
024700       ELSE                                                               
024800         IF MID-KDCMD-INGENTING                                           
024900           IF MID-PRDIRLON = ALL '+' AND                                  
025000              MID-PRDMTRL  = ALL '+' AND                                  
025100              MID-PROVRPAL = ALL '+'                                      
025200             PERFORM MFS-ROER-EJ-BILD                                     
025300             MOVE KOD-PF11-INGET-INDATA TO MED-IDMFSFEL                   
025400             CALL WMEDKONV USING MED-WMEDAREA                             
025500             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
025600           ELSE                                                           
025700             PERFORM DC-AENDRING-TILLAEGG                                 
025800           END-IF                                                         
025900         ELSE                                                             
026000           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-ATTR                 
026100           MOVE MFS-ROER-EJ-FAELT       TO MOD-KDCMD                      
026200           MOVE KOD-KORR-UPPLYSTA-FLT   TO MED-IDMFSFEL                   
026300           CALL WMEDKONV USING MED-WMEDAREA                               
026400           MOVE MED-MFSFEL              TO MOD-TEMFSFEL                   
026500           PERFORM MFS-LAES-BILD-IGEN                                     
026600         END-IF                                                           
026700       END-IF                                                             
026800     ELSE                                                                 
026900       PERFORM MFS-LAES-BILD-IGEN                                         
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 DA-INMATNING-KONTROLL SECTION.                                           
027500                                                                          
027600     IF MID-KDFORPGP-UPP NOT = ALL '+' AND WS-KDFORPGP-UPP NUMERIC        
027700       MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDFORPGP-ATTR-UPP                
027800     ELSE                                                                 
027900       MOVE MFS-NUM-FAELT-FEL     TO MOD-KDFORPGP-ATTR-UPP                
028000       MOVE KOD-KORR-UPPLYSTA-FLT TO MED-IDMFSFEL                         
028100       CALL WMEDKONV USING MED-WMEDAREA                                   
028200       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
028300       MOVE NEJ TO INDATA-OK                                              
028400     END-IF                                                               
028500                                                                          
028600     IF MID-PRDIRLON NOT = ALL '+'                                        
028700       MOVE +4                      TO DEC-KVHELTAL                       
028800       MOVE +3                      TO DEC-KVDECIMAL                      
028900       MOVE MID-PRDIRLON            TO DEC-IDFRIDATA                      
029000                                                                          
029100       CALL WDECEDIT USING DEC-WDECAREA                                   
029200                                                                          
029300       IF DEC-KDSVAR-OK                                                   
029400         MOVE DEC-IDEDITDATA        TO WS-PRDIRLON                        
029500         MOVE MFS-NUM-FAELT-RAETT   TO MOD-PRDIRLON-ATTR-UPP              
029600       ELSE                                                               
029700         MOVE MFS-NUM-FAELT-FEL     TO MOD-PRDIRLON-ATTR-UPP              
029800         MOVE KOD-KORR-UPPLYSTA-FLT TO MED-IDMFSFEL                       
029900         CALL WMEDKONV USING MED-WMEDAREA                                 
030000         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
030100         MOVE NEJ TO INDATA-OK                                            
030200       END-IF                                                             
030300     END-IF                                                               
030400                                                                          
030500     IF MID-PRDMTRL NOT = ALL '+'                                         
030600       MOVE +6                      TO DEC-KVHELTAL                       
030700       MOVE +3                      TO DEC-KVDECIMAL                      
030800       MOVE MID-PRDMTRL             TO DEC-IDFRIDATA                      
030900                                                                          
031000       CALL WDECEDIT USING DEC-WDECAREA                                   
031100                                                                          
031200       IF DEC-KDSVAR-OK                                                   
031300         MOVE DEC-IDEDITDATA        TO WS-PRDMTRL                         
031400         MOVE MFS-NUM-FAELT-RAETT   TO MOD-PRDMTRL-ATTR-UPP               
031500       ELSE                                                               
031600         MOVE MFS-NUM-FAELT-FEL     TO MOD-PRDMTRL-ATTR-UPP               
031700         MOVE KOD-KORR-UPPLYSTA-FLT TO MED-IDMFSFEL                       
031800         CALL WMEDKONV USING MED-WMEDAREA                                 
031900         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
032000         MOVE NEJ TO INDATA-OK                                            
032100       END-IF                                                             
032200     END-IF                                                               
032300                                                                          
032400     IF MID-PROVRPAL NOT = ALL '+'                                        
032500       MOVE +4                      TO DEC-KVHELTAL                       
032600       MOVE +3                      TO DEC-KVDECIMAL                      
032700       MOVE MID-PROVRPAL            TO DEC-IDFRIDATA                      
032800                                                                          
032900       CALL WDECEDIT USING DEC-WDECAREA                                   
033000                                                                          
033100       IF DEC-KDSVAR-OK                                                   
033200         MOVE DEC-IDEDITDATA        TO WS-PROVRPAL                        
033300         MOVE MFS-NUM-FAELT-RAETT   TO MOD-PROVRPAL-ATTR-UPP              
033400       ELSE                                                               
033500         MOVE MFS-NUM-FAELT-FEL     TO MOD-PROVRPAL-ATTR-UPP              
033600         MOVE KOD-KORR-UPPLYSTA-FLT TO MED-IDMFSFEL                       
033700         CALL WMEDKONV USING MED-WMEDAREA                                 
033800         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
033900         MOVE NEJ TO INDATA-OK                                            
034000       END-IF                                                             
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 DB-RADERA-KDFORPGP SECTION.                                              
034500                                                                          
034600     MOVE WS-KDFORPGP-UPP          TO WS-KDFORPGP-UPP-NUM                 
034700     MOVE WS-KDFORPGP-UPP-NUM      TO W-KDFORPGP                          
034800                                                                          
034900     PERFORM IMS-GHU-WL513711                                             
035000     IF SEGMENT-FINNS                                                     
035100       PERFORM IMS-DLET-WL513711                                          
035200       PERFORM MFS-RENSA-UPP-RAD                                          
035300       PERFORM DBA-VISA-EFTER-BORTTAG                                     
035400       MOVE KOD-UPPDATERING-UTFORD TO MED-IDMFSINF                        
035500       CALL WMEDKONV USING MED-WMEDAREA                                   
035600       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
035700     ELSE                                                                 
035800       PERFORM MFS-ROER-EJ-BILD                                           
035900       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDFORPGP-ATTR-UPP               
036000       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDFORPGP-UPP                    
036100       MOVE KOD-URVAL-SAKNAS       TO MED-IDMFSFEL                        
036200       CALL WMEDKONV USING MED-WMEDAREA                                   
036300       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
036400     END-IF                                                               
036500     .                                                                    
036600                                                                          
036700 DBA-VISA-EFTER-BORTTAG SECTION.                                          
036800                                                                          
036900     PERFORM IMS-GU-WL513701                                              
037000     MOVE MID-KDFORPGP-SPAR-RAD1 TO WS-KDFORPGP-RAD1-NUM                  
037100     MOVE WS-KDFORPGP-RAD1-NUM   TO W-KDFORPGP                            
037200                                                                          
037300     PERFORM IMS-GET-WL513711                                             
037400     IF SEGMENT-FINNS                                                     
037500       PERFORM S01-TABELL-BEHANDLING                                      
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 DC-AENDRING-TILLAEGG SECTION.                                            
038100                                                                          
038200     MOVE WS-KDFORPGP-UPP      TO WS-KDFORPGP-UPP-NUM                     
038300     MOVE WS-KDFORPGP-UPP-NUM  TO W-KDFORPGP                              
038400                                                                          
038500     PERFORM IMS-GHU-WL513711                                             
038600     IF SEGMENT-FINNS                                                     
038700       PERFORM DCA-AENDRING                                               
038800     ELSE                                                                 
038900       PERFORM DCB-TILLAEGG                                               
039000     END-IF                                                               
039100                                                                          
039200     IF UPPDATERAT = JA                                                   
039300       PERFORM MFS-RENSA-UPP-RAD                                          
039400       PERFORM DCC-VISA-EFTER-UPPDATERING                                 
039500     END-IF                                                               
039600     .                                                                    
039700                                                                          
039800 DCA-AENDRING SECTION.                                                    
039900                                                                          
040000     IF 5138-PRDIRLON > 0 OR 5138-PRDMTRL > 0                             
040100             OR 5138-PROVRPAL > 0                                         
040200       MOVE JA TO KOSTNAD-FINNS                                           
040300     END-IF                                                               
040400                                                                          
040500     IF MID-PRDIRLON > 0 OR MID-PROVRPAL > 0 OR MID-PRDMTRL > 0           
040600       MOVE JA TO KOSTNAD-INMATAD                                         
040700     END-IF                                                               
040800                                                                          
040900     IF MID-PRDIRLON = 0 AND MID-PROVRPAL = 0 AND MID-PRDMTRL = 0         
041000       MOVE JA TO KOSTNAD-ZERO                                            
041100     END-IF                                                               
041200                                                                          
041300     IF INDATA-OK = JA                                                    
041400         MOVE WS-KDFORPGP-UPP-NUM TO 5138-KDFORPGP                        
041500                                                                          
041600         IF MID-PRDIRLON NOT = ALL '+'                                    
041700           MOVE WS-PRDIRLON   TO 5138-PRDIRLON                            
041800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDIRLON-ATTR(1)             
041900         END-IF                                                           
042000                                                                          
042100         IF MID-PROVRPAL NOT = ALL '+'                                    
042200           MOVE WS-PROVRPAL   TO 5138-PROVRPAL                            
042300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PROVRPAL-ATTR(1)             
042400         END-IF                                                           
042500                                                                          
042600         IF MID-PRDMTRL  NOT = ALL '+'                                    
042700           MOVE WS-PRDMTRL    TO 5138-PRDMTRL                             
042800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDMTRL-ATTR(1)              
042900         END-IF                                                           
043000                                                                          
043100         PERFORM IMS-REPL-WL513711                                        
043200         MOVE JA TO UPPDATERAT                                            
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600                                                                          
043700 DCB-TILLAEGG SECTION.                                                    
043800                                                                          
043900     IF MID-PRDIRLON = ALL '+' OR MID-PRDMTRL = ALL '+'                   
044000         OR MID-PROVRPAL = ALL '+'                                        
044100       PERFORM MFS-LAES-BILD-IGEN                                         
044200                                                                          
044300       IF MID-PRDIRLON = ALL '+'                                          
044400         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDIRLON-ATTR-UPP                  
044500       ELSE                                                               
044600         IF MID-PRDMTRL = ALL '+'                                         
044700           MOVE MFS-NUM-FAELT-FEL TO MOD-PRDMTRL-ATTR-UPP                 
044800         ELSE                                                             
044900           IF MID-PROVRPAL = ALL '+'                                      
045000             MOVE MFS-NUM-FAELT-FEL TO MOD-PROVRPAL-ATTR-UPP              
045100           END-IF                                                         
045200         END-IF                                                           
045300       END-IF                                                             
045400                                                                          
045500       MOVE KOD-KORR-UPPLYSTA-FLT TO MED-IDMFSFEL                         
045600       CALL WMEDKONV USING MED-WMEDAREA                                   
045700       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
045800     ELSE                                                                 
045900       MOVE WS-KDFORPGP-UPP-NUM   TO 5138-KDFORPGP                        
046000       MOVE WS-PRDIRLON           TO 5138-PRDIRLON                        
046100       MOVE WS-PROVRPAL           TO 5138-PROVRPAL                        
046200       MOVE WS-PRDMTRL            TO 5138-PRDMTRL                         
046300                                                                          
046400       PERFORM IMS-ISRT-WL513711                                          
046500       MOVE JA TO UPPDATERAT                                              
046600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFORPGP-ATTR(1)                 
046700                                     MOD-PRDIRLON-ATTR(1)                 
046800                                     MOD-PRDMTRL-ATTR(1)                  
046900                                     MOD-PROVRPAL-ATTR(1)                 
047000     END-IF                                                               
047100     .                                                                    
047200     SKIP2                                                                
047300                                                                          
047400 DCC-VISA-EFTER-UPPDATERING SECTION.                                      
047500                                                                          
047600     PERFORM IMS-GU-WL513701                                              
047700     MOVE WS-KDFORPGP-UPP-NUM TO W-KDFORPGP                               
047800                                                                          
047900     PERFORM IMS-GET-WL513711                                             
048000     IF SEGMENT-FINNS                                                     
048100       PERFORM S01-TABELL-BEHANDLING                                      
048200       MOVE KOD-UPPDATERING-UTFORD TO MED-IDMFSINF                        
048300       CALL WMEDKONV USING MED-WMEDAREA                                   
048400       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
048900 E-LAES-PF7 SECTION.                                                      
049000                                                                          
049100     PERFORM IMS-GU-WL513701                                              
049200     MOVE WS-KDFORPGP TO WS-KDFORPGP-NUM                                  
049300     MOVE WS-KDFORPGP-NUM TO W-KDFORPGP                                   
049400                                                                          
049500     PERFORM IMS-GET-WL513711                                             
049600     PERFORM S01-TABELL-BEHANDLING                                        
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 F-BLAEDDRA-PF8 SECTION.                                                  
050100                                                                          
050200     IF MID-KDFORPGP-SPAR NOT = ZERO                                      
050300       PERFORM IMS-GU-WL513701                                            
050400       MOVE MID-KDFORPGP-SPAR TO WS-KDFORPGP-SPAR-NUM                     
050500       MOVE WS-KDFORPGP-SPAR-NUM TO W-KDFORPGP                            
050600                                                                          
050700       PERFORM IMS-GNP-KVAL-WL513711                                      
050800       PERFORM S01-TABELL-BEHANDLING                                      
050900     ELSE                                                                 
051000       PERFORM MFS-ROER-EJ-BILD                                           
051100       MOVE KOD-SISTA-SIDAN  TO MED-IDMFSINF                              
051200       CALL WMEDKONV USING MED-WMEDAREA                                   
051300       MOVE MED-MFSINF       TO MOD-TEMFSINF                              
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 G-ENTER SECTION.                                                         
051900                                                                          
052000     IF MID-KDFORPGP-UPP NOT = ALL '+'                                    
052100       PERFORM MFS-LAES-BILD-IGEN                                         
052200       PERFORM MFS-LAES-IN-INMATNING                                      
052300       MOVE KOD-TRYCK-PF11   TO MED-IDMFSINF                              
052400       CALL WMEDKONV USING MED-WMEDAREA                                   
052500       MOVE MED-MFSINF       TO MOD-TEMFSINF                              
052600     ELSE                                                                 
052700       PERFORM IMS-GU-WL513701                                            
052800       MOVE WS-KDFORPGP TO WS-KDFORPGP-NUM                                
052900       MOVE WS-KDFORPGP-NUM TO W-KDFORPGP                                 
053000                                                                          
053100       PERFORM IMS-GET-WL513711                                           
053200       IF SEGMENT-FINNS                                                   
053300         PERFORM S01-TABELL-BEHANDLING                                    
053400       ELSE                                                               
053500         MOVE KOD-URVAL-SAKNAS      TO MED-IDMFSFEL                       
053600         CALL WMEDKONV USING MED-WMEDAREA                                 
053700         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
053800       END-IF                                                             
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300 S01-TABELL-BEHANDLING SECTION.                                           
054400                                                                          
054500     MOVE +1 TO IX                                                        
054600                                                                          
054700     PERFORM UNTIL IX > 12 OR SEGMENT-SAKNAS                              
054800                                                                          
054900       MOVE 5138-KDFORPGP        TO MOD-KDFORPGP-RAD(IX)                  
055000       MOVE 5138-PRDIRLON        TO MOD-PRDIRLON-RAD(IX)                  
055100       MOVE 5138-PRDMTRL         TO MOD-PRDMTRL-RAD(IX)                   
055200       MOVE 5138-PROVRPAL        TO MOD-PROVRPAL-RAD(IX)                  
055300       PERFORM IMS-GNP-WL513711                                           
055400       ADD +1 TO IX                                                       
055500     END-PERFORM                                                          
055600                                                                          
055700                                                                          
055800     MOVE MOD-KDFORPGP-RAD(1)    TO WS-KDFORPGP-RAD1                      
055900     MOVE KDFORPGP-RAD1          TO MOD-KDFORPGP-SPAR-RAD1                
056000                                                                          
056100     IF SEGMENT-FINNS                                                     
056200       MOVE 5138-KDFORPGP        TO WS-KDFORPGP-SPAR-NUM                  
056300       MOVE WS-KDFORPGP-SPAR-NUM TO MOD-KDFORPGP-SPAR                     
056400       MOVE KOD-MER-INFO-FINNS   TO MED-IDMFSINF                          
056500       CALL WMEDKONV USING MED-WMEDAREA                                   
056600       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
056700     ELSE                                                                 
056800       MOVE MFS-RENSA-FAELT      TO MOD-KDFORPGP-SPAR                     
056900       MOVE KOD-SISTA-SIDAN      TO MED-IDMFSINF                          
057000       CALL WMEDKONV USING MED-WMEDAREA                                   
057100       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 MFS-LAES-BILD-IGEN SECTION.                                              
057600                                                                          
057700     MOVE +1 TO IX                                                        
057800                                                                          
057900     PERFORM UNTIL IX > 12                                                
058000                                                                          
058100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFORPGP-RAD(IX)                     
058200                                 MOD-PRDIRLON-RAD(IX)                     
058300                                 MOD-PRDMTRL-RAD(IX)                      
058400                                 MOD-PROVRPAL-RAD(IX)                     
058500       ADD +1 TO IX                                                       
058600     END-PERFORM                                                          
058700                                                                          
058800     IF MID-KDFORPGP-UPP NOT = ALL '+'                                    
058900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFORPGP-UPP                         
059000     END-IF                                                               
059100                                                                          
059200     IF MID-PRDIRLON NOT = ALL '+'                                        
059300       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDIRLON-UPP                         
059400     END-IF                                                               
059500                                                                          
059600     IF MID-PRDMTRL NOT = ALL '+'                                         
059700       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDMTRL-UPP                          
059800     END-IF                                                               
059900                                                                          
060000     IF MID-PROVRPAL NOT = ALL '+'                                        
060100       MOVE MFS-ROER-EJ-FAELT TO MOD-PROVRPAL-UPP                         
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500                                                                          
060600 MFS-LAES-IN-INMATNING SECTION.                                           
060700                                                                          
060800     IF MID-KDFORPGP-UPP NOT = ALL '+'                                    
060900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFORPGP-ATTR-UPP                
061000     END-IF                                                               
061100                                                                          
061200     IF MID-PRDIRLON NOT = ALL '+'                                        
061300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDIRLON-ATTR-UPP                
061400     END-IF                                                               
061500                                                                          
061600     IF MID-PRDMTRL NOT = ALL '+'                                         
061700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDMTRL-ATTR-UPP                 
061800     END-IF                                                               
061900                                                                          
062000     IF MID-PROVRPAL NOT = ALL '+'                                        
062100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PROVRPAL-ATTR-UPP                
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 MFS-ROER-EJ-BILD SECTION.                                                
062600                                                                          
062700     MOVE +1 TO IX                                                        
062800                                                                          
062900     PERFORM UNTIL IX > 12                                                
063000                                                                          
063100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFORPGP-RAD(IX)                     
063200                                 MOD-PRDIRLON-RAD(IX)                     
063300                                 MOD-PRDMTRL-RAD(IX)                      
063400                                 MOD-PROVRPAL-RAD(IX)                     
063500       ADD +1 TO IX                                                       
063600     END-PERFORM                                                          
063700                                                                          
063800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDFORPGP-SPAR                          
063900                               MOD-TEMFSINF                               
064000     .                                                                    
064100     SKIP2                                                                
064200 MFS-RENSA-UPP-RAD SECTION.                                               
064300                                                                          
064400     MOVE MFS-RENSA-FAELT TO MOD-KDFORPGP-UPP                             
064500                             MOD-PRDIRLON-UPP                             
064600                             MOD-PROVRPAL-UPP                             
064700                             MOD-PRDMTRL-UPP                              
064800                             MOD-KDCMD                                    
064900     .                                                                    
065000     EJECT                                                                
065100                                                                          
065200* IMS SEKTIONER ******************************                            
065300     SKIP3                                                                
065400 IMS-GET-MSG SECTION.                                                     
065500                                                                          
065600     MOVE '  QC' TO GODK-STATUSKODER                                      
065700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065900     PERFORM IMS-STATUSKONTROLL                                           
066000     .                                                                    
066100     SKIP2                                                                
066200 IMS-INSERT-MSG SECTION.                                                  
066300                                                                          
066400     IF ENGLISH-TEXT                                                      
066500       MOVE '0' TO MFS-KDHUVOMR                                           
066600     END-IF                                                               
066700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
066800     MOVE SPACE TO GODK-STATUSKODER                                       
066900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067100     PERFORM IMS-STATUSKONTROLL                                           
067200     .                                                                    
067300     EJECT                                                                
067400 IMS-GU-WL513701 SECTION.                                                 
067500                                                                          
067600     STRING 'WL513701(WDGXKEY  =' W-WDGXKEY-HTYP-5137-X ')'               
067700            DELIMITED BY SIZE INTO SSA1                                   
067800     MOVE '  ' TO GODK-STATUSKODER                                        
067900     CALL CBLTDLI USING GU 5137-PCB DLI-IO-AREA SSA1                      
068000     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     SKIP2                                                                
068400 IMS-GET-WL513711 SECTION.                                                
068500                                                                          
068600     STRING 'WL513711(KDFORPGP>=' W-KDFORPGP-X ')'                        
068700            DELIMITED BY SIZE INTO SSA1                                   
068800     MOVE '  GE' TO GODK-STATUSKODER                                      
068900     CALL CBLTDLI USING GNP 5137-PCB DLI-IO-AREA SSA1                     
069000     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
069100     PERFORM IMS-STATUSKONTROLL                                           
069200     .                                                                    
069300     SKIP2                                                                
069400 IMS-GNP-WL513711 SECTION.                                                
069500                                                                          
069600     MOVE 'WL513711'   TO SSA1                                            
069700     MOVE '  GE' TO GODK-STATUSKODER                                      
069800     CALL CBLTDLI USING GNP 5137-PCB DLI-IO-AREA SSA1                     
069900     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
070000     PERFORM IMS-STATUSKONTROLL                                           
070100     .                                                                    
070200     EJECT                                                                
070300 IMS-GNP-KVAL-WL513711 SECTION.                                           
070400                                                                          
070500     STRING 'WL513711(KDFORPGP =' W-KDFORPGP-X ')'                        
070600            DELIMITED BY SIZE INTO SSA1                                   
070700     MOVE '  GE' TO GODK-STATUSKODER                                      
070800     CALL CBLTDLI USING GNP 5137-PCB DLI-IO-AREA SSA1                     
070900     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     SKIP2                                                                
071300 IMS-GHU-WL513711 SECTION.                                                
071400                                                                          
071500     STRING 'WL513701(WDGXKEY  =' W-WDGXKEY-HTYP-5137-X ')'               
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     STRING 'WL513711(KDFORPGP =' W-KDFORPGP-X ')'                        
071800            DELIMITED BY SIZE INTO SSA2                                   
071900     MOVE '  GE' TO GODK-STATUSKODER                                      
072000     CALL CBLTDLI USING GHU 5137-PCB DLI-IO-AREA SSA1 SSA2                
072100     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
072200     PERFORM IMS-STATUSKONTROLL                                           
072300     .                                                                    
072400     SKIP2                                                                
072500 IMS-REPL-WL513711 SECTION.                                               
072600                                                                          
072700     MOVE '  ' TO GODK-STATUSKODER                                        
072800     CALL CBLTDLI USING REPL 5137-PCB DLI-IO-AREA                         
072900     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUSKONTROLL                                           
073100     .                                                                    
073200     EJECT                                                                
073300                                                                          
073400 IMS-ISRT-WL513711 SECTION.                                               
073500                                                                          
073600     STRING 'WL513701(WDGXKEY  =' W-WDGXKEY-HTYP-5137-X ')'               
073700            DELIMITED BY SIZE INTO SSA1                                   
073800     MOVE 'WL513711' TO SSA2                                              
073900     MOVE '  II' TO GODK-STATUSKODER                                      
074000     CALL CBLTDLI USING ISRT 5137-PCB DLI-IO-AREA SSA1 SSA2               
074100     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074400     SKIP2                                                                
074500 IMS-DLET-WL513711 SECTION.                                               
074600                                                                          
074700     MOVE '  GE' TO GODK-STATUSKODER                                      
074800     CALL CBLTDLI USING DLET 5137-PCB DLI-IO-AREA                         
074900     MOVE 5137-STATUS-CODE TO STATUS-WS                                   
075000     PERFORM IMS-STATUSKONTROLL                                           
075100     .                                                                    
075200     EJECT                                                                
075300 IMS-STATUSKONTROLL SECTION.                                              
075400                                                                          
075500     SET STATUS-IX TO 1                                                   
075600     SEARCH GODK-STATUS AT END CALL FELLOG                                
075700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
075800     END-SEARCH                                                           
075900     .                                                                    
