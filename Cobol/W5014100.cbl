000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5014100.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500     DATE-WRITTEN.   AUG   89.                                            
000600*                                                                         
000700*    REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        MPP PROGRAM SOM INGÅR I PRISSÄTTNING FÖR EMBALLAGE               
001000*                                SYSTEMET.                                
001100*        MÖJLIGHETER: SÖKNING OCH UPPDATERING AV EN VISS FP-TYP           
001200*        PROGRAMMET ÄR KOPIERAT AV W4011200 /KJH.                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T141  W5T141U                                     
001600*        MID:         W5I14101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W5O14101                                            
002000                                                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77    IDPGM                     PIC X(8)    VALUE 'W5014100'.            
002800 77    JA                        PIC X       VALUE 'J'.                   
002900 77    NEJ                       PIC X       VALUE 'N'.                   
003000 77    INDATA-OK                 PIC X       VALUE 'J'.                   
003100 77    NYCKEL-OK                 PIC X       VALUE 'N'.                   
003200 77    UPPDATERAT                PIC X       VALUE 'N'.                   
003300 77    KOSTNAD-FINNS             PIC X       VALUE 'N'.                   
003400 77    KOSTNAD-ZERO              PIC X       VALUE 'N'.                   
003500 77    KOSTNAD-INMATAD           PIC X       VALUE 'N'.                   
003700 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003800 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +594 COMP SYNC.        
003900                                                                          
004000 77    WS-BEFT                   PIC X(2)    VALUE SPACE.                 
004100 77    WS-BEFT-NUM               PIC 9(2)    VALUE ZERO.                  
004200 77    WS-BEFT-UPP               PIC X(2)    VALUE SPACE.                 
004300 77    WS-BEFT-UPP-NUM           PIC 9(2)    VALUE ZERO.                  
004400 77    WS-BEFT-SPAR-NUM          PIC 9(2)    VALUE ZERO.                  
004500 77    WS-BEFT-RAD1-NUM          PIC 9(2)    VALUE ZERO.                  
004600                                                                          
004700 77    WS-PRDIRLON               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004800 77    WS-PROVRPAL               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004900 77    WS-PRDMTRL                PIC S9(6)V9(3)  VALUE +0 COMP-3.         
005000                                                                          
005100 01    WS-BEFT-RAD1.                                                      
005200   03    FILLER                  PIC X.                                   
005300   03    BEFT-RAD1               PIC X(2).                                
005400                                                                          
005500 01    WS-IDTRANS                PIC X(4).                                
005600   88    EGEN-BILD                           VALUE '5141'.                
005700     EJECT                                                                
005800                                                                          
005900 01    GENERELLA-SUBPROGRAM.                                              
006000   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006400                                                                          
006500*  03 FILLER  -COPY WDECAREA.                                             
006600     EJECT                                                                
006700***   PARAMETERAREA FÖR SUBPROGRAM WMEDKONV                               
006800*01 FILLER  -COPY WMEDAREA.                                               
006900     EJECT                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  KOD-KORR-UPPL-FLT       PIC X(3)    VALUE '001'.                 
007200     03  KOD-TRYCK-PF11          PIC X(3)    VALUE '003'.                 
007300     03  KOD-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
007310     03  KOD-PF11-INGET-INDATA   PIC X(3)    VALUE '011'.                 
007320     03  KOD-UPPDAT-UTFORD       PIC X(3)    VALUE '101'.                 
007321     03  KOD-MER-INFO-FINNS      PIC X(3)    VALUE '105'.                 
007330     03  KOD-SISTA-SIDAN         PIC X(3)    VALUE '106'.                 
007700     SKIP3                                                                
007900 01    NYCKLAR-TILL-DLI.                                                  
008000                                                                          
008100   03    W-WDGXKEY-HTYP-5131-X.                                           
008200     05    W-HTYP                PIC 9(4)    VALUE 5131.                  
008300     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
008400                                                                          
008500   03    W-BEFT-X.                                                        
008600     05    W-BEFT                PIC S9(3)   VALUE ZERO COMP-3.           
013700     EJECT                                                                
013800******************************************************************        
013900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014000*                                                                         
014100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
014200                                                                          
014300*01    MID -COPY W5I14101.                                                
014400     EJECT                                                                
014500*01    -COPY WMSGAREA                                                     
014600     EJECT                                                                
014700*  03    MOD -COPY W5O14101 -RED MSG-AREA.                                
014800     EJECT                                                                
014900*01    -COPY WMFSAREA                                                     
015000     EJECT                                                                
015100******************************************************************        
015200*                                                                         
015300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015400*                                                                         
015500 01    IMS-WS.                                                            
015600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
015700     SKIP3                                                                
015800*                        **** STATUS-KOD FRÅN IMS                         
015900   03    STATUS-WS               PIC XX.                                  
016000     88    SEGMENT-FINNS                     VALUE '  '.                  
016100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
016200     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
016300     SKIP3                                                                
016400   03    GODK-STATUSKODER.                                                
016500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
016600     SKIP3                                                                
016700 01    SSA1                      PIC X(164).                              
016800 01    SSA2                      PIC X(164).                              
016900     EJECT                                                                
017000*                            IMS FUNKTIONSKODER                           
017100*01    -COPY W0003                                                        
017200     EJECT                                                                
017300*                            DLI INPUT-OUTPUT AREA                        
017400 01    DLI-IO-AREA.                                                       
017500   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
017600     SKIP3                                                                
017700*  03    WL513111 -COPY WDGX5132  -RED IO-AREA.                           
017800     EJECT                                                                
017900*                                                                         
018000 LINKAGE SECTION.                                                         
018100*01    -COPY W0009     -PRE MSG-                                          
018200     EJECT                                                                
018300*01    -COPY W0008     -PRE 5131-                                         
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600 PROCEDURE DIVISION USING MSG-PCB 5131-PCB.                               
018700     ENTRY 'DLITCBL' USING MSG-PCB 5131-PCB.                              
018800                                                                          
018900 STYR SECTION.                                                            
019000     PERFORM IMS-GET-MSG                                                  
019100     IF SEGMENT-FINNS                                                     
019200       PERFORM A-INIT-INPUT                                               
019300       PERFORM B-SPARA-INPUT                                              
019400       PERFORM C-NYCKEL-KONTROLL                                          
019500       IF NYCKEL-OK = JA                                                  
019600         IF MFS-UPDATE                                                    
019700           PERFORM D-UPPDATERA                                            
019800         ELSE                                                             
019900           IF MFS-IDPFK = '7'                                             
020000             PERFORM E-LAES-PF7                                           
020100           ELSE                                                           
020200             IF MFS-IDPFK = '8'                                           
020300               PERFORM F-BLAEDDRA-PF8                                     
020400             ELSE                                                         
020500               PERFORM G-ENTER                                            
020600             END-IF                                                       
020700           END-IF                                                         
020800         END-IF                                                           
020900       END-IF                                                             
021000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
021100       PERFORM IMS-INSERT-MSG                                             
021200     END-IF                                                               
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 A-INIT-INPUT SECTION.                                                    
021900                                                                          
022000     IF MSG-DUBBLA-TRANSKODER                                             
022100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I14101                 
022200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022400       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
022500       MOVE MSG-IDPFK TO MFS-IDPFK                                        
022600     ELSE                                                                 
022700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I14101                  
022800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023000       MOVE SPACE TO MFS-KDTRTYP                                          
023100                     MFS-IDPFK                                            
023200     END-IF                                                               
023300                                                                          
023400     MOVE LOW-VALUE TO MSG-AREA                                           
023500     MOVE 'W5O14101' TO MFS-IDMOD                                         
023600     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023700     MOVE '5141' TO MOD-IDTRANS                                           
023800                                                                          
023900     IF MFS-KDMFSFOR = '1'                                                
024100       MOVE 'S  '         TO MED-IDSKYLT                                  
024200     ELSE                                                                 
024400       MOVE 'GB '         TO MED-IDSKYLT                                  
024500     END-IF                                                               
024600                                                                          
024700     MOVE MFS-RENSA-FAELT TO MOD-BEFT-IN                                  
024800                             MOD-BEFT-UPP                                 
024900                             MOD-TEMFSFEL                                 
025000                             MOD-TEMFSINF                                 
025100     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400 B-SPARA-INPUT SECTION.                                                   
025500                                                                          
025600     IF MID-BEFT-IN = ALL '+'                                             
025700       MOVE MID-BEFT-UT TO WS-BEFT                                        
025800       INSPECT WS-BEFT REPLACING LEADING SPACE BY ZERO                    
025900     ELSE                                                                 
026000       MOVE MID-BEFT-IN TO WS-BEFT                                        
026100     END-IF                                                               
026200                                                                          
026300     MOVE WS-BEFT TO MOD-BEFT-UT                                          
026400                                                                          
026500     IF NOT EGEN-BILD                                                     
026600       MOVE SPACE TO MOD-BEFT-UT                                          
026700                     WS-BEFT                                              
026800       MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                               
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 C-NYCKEL-KONTROLL SECTION.                                               
027400                                                                          
027500     IF WS-BEFT NUMERIC                                                   
027600       MOVE JA TO NYCKEL-OK                                               
027700     ELSE                                                                 
027800       IF EGEN-BILD                                                       
027900         MOVE KOD-URVAL-SAKNAS TO MED-IDMFSFEL                            
028000         CALL WMEDKONV USING MED-WMEDAREA                                 
028100         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
028300       END-IF                                                             
028400     END-IF                                                               
028500                                                                          
028600     IF MID-BEFT-UPP NOT = ALL '+'                                        
028700       MOVE MID-BEFT-UPP TO WS-BEFT-UPP                                   
028800       INSPECT WS-BEFT-UPP REPLACING LEADING SPACE BY ZERO                
028900     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200                                                                          
029300 D-UPPDATERA SECTION.                                                     
029400                                                                          
029500     PERFORM DA-INMATNING-KONTROLL                                        
029600                                                                          
029700     IF INDATA-OK = JA                                                    
029800       IF MID-KDCMD-DELETE                                                
029900         PERFORM DB-RADERA-BEFT                                           
030000       ELSE                                                               
030100         IF MID-KDCMD-INGENTING                                           
030200           IF MID-PRDIRLON = ALL '+' AND MID-PRDMTRL = ALL '+'            
030300               AND MID-PROVRPAL = ALL '+'                                 
030400             PERFORM MFS-ROER-EJ-BILD                                     
030410             MOVE KOD-PF11-INGET-INDATA   TO MED-IDMFSFEL                 
030420             CALL WMEDKONV USING MED-WMEDAREA                             
030430             MOVE MED-MFSFEL              TO MOD-TEMFSFEL                 
030600           ELSE                                                           
030700             PERFORM DC-AENDRING-TILLAEGG                                 
030800           END-IF                                                         
030900         ELSE                                                             
031000           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-ATTR                 
031100           MOVE MFS-ROER-EJ-FAELT       TO MOD-KDCMD                      
031200           MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                   
031300           CALL WMEDKONV USING MED-WMEDAREA                               
031400           MOVE MED-MFSFEL              TO MOD-TEMFSFEL                   
031500           PERFORM MFS-LAES-BILD-IGEN                                     
031600         END-IF                                                           
031700       END-IF                                                             
031800     ELSE                                                                 
031900       PERFORM MFS-LAES-BILD-IGEN                                         
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400 DA-INMATNING-KONTROLL SECTION.                                           
032500                                                                          
032600     IF MID-BEFT-UPP NOT = ALL '+' AND WS-BEFT-UPP NUMERIC                
032700       MOVE MFS-NUM-FAELT-RAETT TO MOD-BEFT-ATTR-UPP                      
032800     ELSE                                                                 
032900       MOVE MFS-NUM-FAELT-FEL TO MOD-BEFT-ATTR-UPP                        
033000       MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                       
033100       CALL WMEDKONV USING MED-WMEDAREA                                   
033200       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
033300       MOVE NEJ TO INDATA-OK                                              
033400     END-IF                                                               
033500                                                                          
033600     IF MID-PRDIRLON NOT = ALL '+'                                        
033700       MOVE +4           TO DEC-KVHELTAL                                  
033800       MOVE +3           TO DEC-KVDECIMAL                                 
033900       MOVE MID-PRDIRLON TO DEC-IDFRIDATA                                 
034000                                                                          
034100       CALL WDECEDIT USING DEC-WDECAREA                                   
034200                                                                          
034300       IF DEC-KDSVAR-OK                                                   
034400         MOVE DEC-IDEDITDATA TO WS-PRDIRLON                               
034500         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRDIRLON-ATTR-UPP                
034600       ELSE                                                               
034700         MOVE MFS-NUM-FAELT-FEL       TO MOD-PRDIRLON-ATTR-UPP            
034800         MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                     
034900         CALL WMEDKONV USING MED-WMEDAREA                                 
035000         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
035100         MOVE NEJ TO INDATA-OK                                            
035200       END-IF                                                             
035300     END-IF                                                               
035400                                                                          
035500     IF MID-PRDMTRL NOT = ALL '+'                                         
035600       MOVE +6           TO DEC-KVHELTAL                                  
035700       MOVE +3           TO DEC-KVDECIMAL                                 
035800       MOVE MID-PRDMTRL TO DEC-IDFRIDATA                                  
035900                                                                          
036000       CALL WDECEDIT USING DEC-WDECAREA                                   
036100                                                                          
036200       IF DEC-KDSVAR-OK                                                   
036300         MOVE DEC-IDEDITDATA TO WS-PRDMTRL                                
036400         MOVE MFS-NUM-FAELT-RAETT     TO MOD-PRDMTRL-ATTR-UPP             
036500       ELSE                                                               
036600         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDMTRL-ATTR-UPP                   
036700         MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                     
036800         CALL WMEDKONV USING MED-WMEDAREA                                 
036900         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
037000         MOVE NEJ TO INDATA-OK                                            
037100       END-IF                                                             
037200     END-IF                                                               
037300                                                                          
037400     IF MID-PROVRPAL NOT = ALL '+'                                        
037500       MOVE +4           TO DEC-KVHELTAL                                  
037600       MOVE +3           TO DEC-KVDECIMAL                                 
037700       MOVE MID-PROVRPAL TO DEC-IDFRIDATA                                 
037800                                                                          
037900       CALL WDECEDIT USING DEC-WDECAREA                                   
038000                                                                          
038100       IF DEC-KDSVAR-OK                                                   
038200         MOVE DEC-IDEDITDATA TO WS-PROVRPAL                               
038300         MOVE MFS-NUM-FAELT-RAETT TO MOD-PROVRPAL-ATTR-UPP                
038400       ELSE                                                               
038500         MOVE MFS-NUM-FAELT-FEL       TO MOD-PROVRPAL-ATTR-UPP            
038600         MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                     
038700         CALL WMEDKONV USING MED-WMEDAREA                                 
038800         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
038900         MOVE NEJ TO INDATA-OK                                            
039000       END-IF                                                             
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 DB-RADERA-BEFT SECTION.                                                  
039500                                                                          
039600     MOVE WS-BEFT-UPP TO WS-BEFT-UPP-NUM                                  
039700     MOVE WS-BEFT-UPP-NUM TO W-BEFT                                       
039800                                                                          
039900     PERFORM IMS-GHU-WL513111                                             
040000     IF SEGMENT-FINNS                                                     
040100       PERFORM IMS-DLET-WL513111                                          
040200       PERFORM MFS-RENSA-UPP-RAD                                          
040300       PERFORM DBA-VISA-EFTER-BORTTAG                                     
040310       MOVE KOD-UPPDAT-UTFORD     TO MED-IDMFSINF                         
040320       CALL WMEDKONV USING MED-WMEDAREA                                   
040330       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
040500     ELSE                                                                 
040600       PERFORM MFS-ROER-EJ-BILD                                           
040700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFT-ATTR-UPP                    
040800       MOVE MFS-ROER-EJ-FAELT     TO MOD-BEFT-UPP                         
040900       MOVE KOD-URVAL-SAKNAS      TO MED-IDMFSFEL                         
041000       CALL WMEDKONV USING MED-WMEDAREA                                   
041100       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
041300     END-IF                                                               
041400     .                                                                    
041500                                                                          
041600 DBA-VISA-EFTER-BORTTAG SECTION.                                          
041700                                                                          
041800     PERFORM IMS-GU-WL513101                                              
041900     MOVE MID-BEFT-SPAR-RAD1 TO WS-BEFT-RAD1-NUM                          
042000     MOVE WS-BEFT-RAD1-NUM TO W-BEFT                                      
042100                                                                          
042200     PERFORM IMS-GET-WL513111                                             
042300     IF SEGMENT-FINNS                                                     
042400       PERFORM S01-TABELL-BEHANDLING                                      
042500     END-IF                                                               
042600     .                                                                    
042700     EJECT                                                                
042800                                                                          
042900 DC-AENDRING-TILLAEGG SECTION.                                            
043000                                                                          
043100     MOVE WS-BEFT-UPP TO WS-BEFT-UPP-NUM                                  
043200     MOVE WS-BEFT-UPP-NUM TO W-BEFT                                       
043300                                                                          
043400     PERFORM IMS-GHU-WL513111                                             
043500     IF SEGMENT-FINNS                                                     
043600       PERFORM DCA-AENDRING                                               
043700     ELSE                                                                 
043800       PERFORM DCB-TILLAEGG                                               
043900     END-IF                                                               
044000                                                                          
044100     IF UPPDATERAT = JA                                                   
044200       PERFORM MFS-RENSA-UPP-RAD                                          
044300       PERFORM DCC-VISA-EFTER-UPPDATERING                                 
044400     END-IF                                                               
044500     .                                                                    
044600                                                                          
044700 DCA-AENDRING SECTION.                                                    
044800                                                                          
044900     IF 5132-PRDIRLON > 0 OR 5132-PRDMTRL > 0                             
045000             OR 5132-PROVRPAL > 0                                         
045100       MOVE JA TO KOSTNAD-FINNS                                           
045200     END-IF                                                               
045300                                                                          
045400     IF MID-PRDIRLON > 0 OR MID-PROVRPAL > 0 OR MID-PRDMTRL > 0           
045500       MOVE JA TO KOSTNAD-INMATAD                                         
045600     END-IF                                                               
045700                                                                          
045800     IF MID-PRDIRLON = 0 AND MID-PROVRPAL = 0 AND MID-PRDMTRL = 0         
045900       MOVE JA TO KOSTNAD-ZERO                                            
046000     END-IF                                                               
046100                                                                          
046200     IF INDATA-OK = JA                                                    
046300         MOVE WS-BEFT-UPP-NUM TO 5132-BEFT                                
046400                                                                          
046500         IF MID-PRDIRLON NOT = ALL '+'                                    
046600           MOVE WS-PRDIRLON   TO 5132-PRDIRLON                            
046700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDIRLON-ATTR(1)             
046800         END-IF                                                           
046900                                                                          
047000         IF MID-PROVRPAL NOT = ALL '+'                                    
047100           MOVE WS-PROVRPAL   TO 5132-PROVRPAL                            
047200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PROVRPAL-ATTR(1)             
047300         END-IF                                                           
047400                                                                          
047500         IF MID-PRDMTRL  NOT = ALL '+'                                    
047600           MOVE WS-PRDMTRL    TO 5132-PRDMTRL                             
047700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDMTRL-ATTR(1)              
047800         END-IF                                                           
047900                                                                          
048000         PERFORM IMS-REPL-WL513111                                        
048100         MOVE JA TO UPPDATERAT                                            
048200     END-IF                                                               
048300     .                                                                    
048400     EJECT                                                                
048500                                                                          
048600 DCB-TILLAEGG SECTION.                                                    
048700                                                                          
048800     IF MID-PRDIRLON = ALL '+' OR MID-PRDMTRL = ALL '+'                   
048900         OR MID-PROVRPAL = ALL '+'                                        
049000       PERFORM MFS-LAES-BILD-IGEN                                         
049100                                                                          
049200       IF MID-PRDIRLON = ALL '+'                                          
049300         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDIRLON-ATTR-UPP                  
049400       ELSE                                                               
049500         IF MID-PRDMTRL = ALL '+'                                         
049600           MOVE MFS-NUM-FAELT-FEL TO MOD-PRDMTRL-ATTR-UPP                 
049700         ELSE                                                             
049800           IF MID-PROVRPAL = ALL '+'                                      
049900             MOVE MFS-NUM-FAELT-FEL TO MOD-PROVRPAL-ATTR-UPP              
050000           END-IF                                                         
050100         END-IF                                                           
050200       END-IF                                                             
050300       MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                       
050400       CALL WMEDKONV USING MED-WMEDAREA                                   
050500       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
050600     ELSE                                                                 
050700       MOVE WS-BEFT-UPP-NUM       TO 5132-BEFT                            
050800       MOVE WS-PRDIRLON           TO 5132-PRDIRLON                        
050900       MOVE WS-PROVRPAL           TO 5132-PROVRPAL                        
051000       MOVE WS-PRDMTRL            TO 5132-PRDMTRL                         
051100                                                                          
051200       PERFORM IMS-ISRT-WL513111                                          
051300       MOVE JA TO UPPDATERAT                                              
051400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEFT-ATTR(1)                     
051500                                     MOD-PRDIRLON-ATTR(1)                 
051600                                     MOD-PRDMTRL-ATTR(1)                  
051700                                     MOD-PROVRPAL-ATTR(1)                 
051800     END-IF                                                               
051900     .                                                                    
052000     SKIP2                                                                
052100                                                                          
052200 DCC-VISA-EFTER-UPPDATERING SECTION.                                      
052300                                                                          
052400     PERFORM IMS-GU-WL513101                                              
052500     MOVE WS-BEFT-UPP-NUM TO W-BEFT                                       
052600     PERFORM IMS-GET-WL513111                                             
052700     IF SEGMENT-FINNS                                                     
052800       PERFORM S01-TABELL-BEHANDLING                                      
052810       MOVE KOD-UPPDAT-UTFORD     TO MED-IDMFSINF                         
052820       CALL WMEDKONV USING MED-WMEDAREA                                   
052830       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
053000     END-IF                                                               
053100     .                                                                    
053200     EJECT                                                                
053300                                                                          
053400 E-LAES-PF7 SECTION.                                                      
053500                                                                          
053600     PERFORM IMS-GU-WL513101                                              
053700     MOVE WS-BEFT TO WS-BEFT-NUM                                          
053800     MOVE WS-BEFT-NUM TO W-BEFT                                           
053900     PERFORM IMS-GET-WL513111                                             
054000     PERFORM S01-TABELL-BEHANDLING                                        
054100     .                                                                    
054200     EJECT                                                                
054300                                                                          
054400 F-BLAEDDRA-PF8 SECTION.                                                  
054500                                                                          
054600     IF MID-BEFT-SPAR NOT = ZERO                                          
054700       PERFORM IMS-GU-WL513101                                            
054800       MOVE MID-BEFT-SPAR TO WS-BEFT-SPAR-NUM                             
054900       MOVE WS-BEFT-SPAR-NUM TO W-BEFT                                    
055000       PERFORM IMS-GNP-KVAL-WL513111                                      
055100       PERFORM S01-TABELL-BEHANDLING                                      
055200     ELSE                                                                 
055300       PERFORM MFS-ROER-EJ-BILD                                           
055310       MOVE KOD-SISTA-SIDAN     TO MED-IDMFSINF                           
055320       CALL WMEDKONV USING MED-WMEDAREA                                   
055330       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800                                                                          
055900 G-ENTER SECTION.                                                         
056000                                                                          
056100     IF MID-BEFT-UPP NOT = ALL '+'                                        
056200       PERFORM MFS-LAES-BILD-IGEN                                         
056300       PERFORM MFS-LAES-IN-INMATNING                                      
056400       MOVE KOD-TRYCK-PF11   TO MED-IDMFSFEL                              
056500       CALL WMEDKONV USING MED-WMEDAREA                                   
056600       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
056700     ELSE                                                                 
056800       PERFORM IMS-GU-WL513101                                            
056900       MOVE WS-BEFT TO WS-BEFT-NUM                                        
057000       MOVE WS-BEFT-NUM TO W-BEFT                                         
057100                                                                          
057200       PERFORM IMS-GET-WL513111                                           
057300       IF SEGMENT-FINNS                                                   
057400         PERFORM S01-TABELL-BEHANDLING                                    
057500       ELSE                                                               
057600         MOVE KOD-URVAL-SAKNAS TO MED-IDMFSFEL                            
057700         CALL WMEDKONV USING MED-WMEDAREA                                 
057800         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400                                                                          
058500 S01-TABELL-BEHANDLING SECTION.                                           
058600                                                                          
058700     MOVE +1 TO IX                                                        
058800                                                                          
058900     PERFORM UNTIL IX > 12 OR SEGMENT-SAKNAS                              
059000       MOVE 5132-BEFT           TO MOD-BEFT-RAD(IX)                       
059100       MOVE 5132-PRDIRLON       TO MOD-PRDIRLON-RAD(IX)                   
059200       MOVE 5132-PRDMTRL        TO MOD-PRDMTRL-RAD(IX)                    
059300       MOVE 5132-PROVRPAL       TO MOD-PROVRPAL-RAD(IX)                   
059400       PERFORM IMS-GNP-WL513111                                           
059500       ADD +1 TO IX                                                       
059600     END-PERFORM                                                          
059700                                                                          
059800     MOVE MOD-BEFT-RAD(1)       TO WS-BEFT-RAD1                           
059900     MOVE BEFT-RAD1             TO MOD-BEFT-SPAR-RAD1                     
060000                                                                          
060100     IF SEGMENT-FINNS                                                     
060200       MOVE 5132-BEFT           TO WS-BEFT-SPAR-NUM                       
060300       MOVE WS-BEFT-SPAR-NUM    TO MOD-BEFT-SPAR                          
060310       MOVE KOD-MER-INFO-FINNS  TO MED-IDMFSINF                           
060320       CALL WMEDKONV USING MED-WMEDAREA                                   
060330       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
060500     ELSE                                                                 
060600       MOVE MFS-RENSA-FAELT TO MOD-BEFT-SPAR                              
060610       MOVE KOD-SISTA-SIDAN     TO MED-IDMFSINF                           
060620       CALL WMEDKONV USING MED-WMEDAREA                                   
060630       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 MFS-LAES-BILD-IGEN SECTION.                                              
061200                                                                          
061300     MOVE +1 TO IX                                                        
061400                                                                          
061500     PERFORM UNTIL IX > 12                                                
061600       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-RAD(IX)                         
061700                                 MOD-PRDIRLON-RAD(IX)                     
061800                                 MOD-PRDMTRL-RAD(IX)                      
061900                                 MOD-PROVRPAL-RAD(IX)                     
062000       ADD +1 TO IX                                                       
062100     END-PERFORM                                                          
062200                                                                          
062300     IF MID-BEFT-UPP NOT = ALL '+'                                        
062400       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-UPP                             
062500     END-IF                                                               
062600                                                                          
062700     IF MID-PRDIRLON NOT = ALL '+'                                        
062800       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDIRLON-UPP                         
062900     END-IF                                                               
063000                                                                          
063100     IF MID-PRDMTRL NOT = ALL '+'                                         
063200       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDMTRL-UPP                          
063300     END-IF                                                               
063400                                                                          
063500     IF MID-PROVRPAL NOT = ALL '+'                                        
063600       MOVE MFS-ROER-EJ-FAELT TO MOD-PROVRPAL-UPP                         
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 MFS-LAES-IN-INMATNING SECTION.                                           
064200                                                                          
064300     IF MID-BEFT-UPP NOT = ALL '+'                                        
064400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFT-ATTR-UPP                    
064500     END-IF                                                               
064600                                                                          
064700     IF MID-PRDIRLON NOT = ALL '+'                                        
064800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDIRLON-ATTR-UPP                
064900     END-IF                                                               
065000                                                                          
065100     IF MID-PRDMTRL NOT = ALL '+'                                         
065200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDMTRL-ATTR-UPP                 
065300     END-IF                                                               
065400                                                                          
065500     IF MID-PROVRPAL NOT = ALL '+'                                        
065600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PROVRPAL-ATTR-UPP                
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100 MFS-ROER-EJ-BILD SECTION.                                                
066200                                                                          
066300     MOVE +1 TO IX                                                        
066400                                                                          
066500     PERFORM UNTIL IX > 12                                                
066600       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-RAD(IX)                         
066700                                 MOD-PRDIRLON-RAD(IX)                     
066800                                 MOD-PRDMTRL-RAD(IX)                      
066900                                 MOD-PROVRPAL-RAD(IX)                     
067000       ADD +1 TO IX                                                       
067100     END-PERFORM                                                          
067200                                                                          
067300     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-SPAR                              
067400                               MOD-TEMFSINF                               
067500     .                                                                    
067600     SKIP2                                                                
067700 MFS-RENSA-UPP-RAD SECTION.                                               
067800                                                                          
067900     MOVE MFS-RENSA-FAELT TO MOD-BEFT-UPP                                 
068000                             MOD-PRDIRLON-UPP                             
068100                             MOD-PROVRPAL-UPP                             
068200                             MOD-PRDMTRL-UPP                              
068300                             MOD-KDCMD                                    
068400     .                                                                    
068500     EJECT                                                                
068600                                                                          
068700* IMS SEKTIONER ******************************                            
068800     SKIP3                                                                
068900 IMS-GET-MSG SECTION.                                                     
069000                                                                          
069100     MOVE '  QC' TO GODK-STATUSKODER                                      
069200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600     SKIP3                                                                
069700 IMS-INSERT-MSG SECTION.                                                  
069800                                                                          
069900     IF ENGLISH-TEXT                                                      
070000       MOVE '0' TO MFS-KDHUVOMR                                           
070100     END-IF                                                               
070200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070300     MOVE SPACE TO GODK-STATUSKODER                                       
070400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070600     PERFORM IMS-STATUSKONTROLL                                           
070700     .                                                                    
070800     EJECT                                                                
070900 IMS-GU-WL513101 SECTION.                                                 
071000                                                                          
071100     STRING 'WL513101(WDGXKEY  =' W-WDGXKEY-HTYP-5131-X ')'               
071200            DELIMITED BY SIZE INTO SSA1                                   
071300     MOVE '  ' TO GODK-STATUSKODER                                        
071400     CALL CBLTDLI USING GU 5131-PCB DLI-IO-AREA SSA1                      
071500     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     SKIP2                                                                
071900 IMS-GET-WL513111 SECTION.                                                
072000                                                                          
072100     STRING 'WL513111(BEFT    >=' W-BEFT-X ')'                            
072200            DELIMITED BY SIZE INTO SSA1                                   
072300     MOVE '  GE' TO GODK-STATUSKODER                                      
072400     CALL CBLTDLI USING GNP 5131-PCB DLI-IO-AREA SSA1                     
072500     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     SKIP2                                                                
072900 IMS-GNP-WL513111 SECTION.                                                
073000                                                                          
073100     MOVE 'WL513111  ' TO SSA1                                            
073200     MOVE '  GE' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING GNP 5131-PCB DLI-IO-AREA SSA1                     
073400     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     EJECT                                                                
073800 IMS-GNP-KVAL-WL513111 SECTION.                                           
073900                                                                          
074000     STRING 'WL513111(BEFT     =' W-BEFT-X ')'                            
074100            DELIMITED BY SIZE INTO SSA1                                   
074200     MOVE '  GE' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GNP 5131-PCB DLI-IO-AREA SSA1                     
074400     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP2                                                                
074800 IMS-GHU-WL513111 SECTION.                                                
074900                                                                          
075000     STRING 'WL513101(WDGXKEY  =' W-WDGXKEY-HTYP-5131-X ')'               
075100            DELIMITED BY SIZE INTO SSA1                                   
075200     STRING 'WL513111(BEFT     =' W-BEFT-X ')'                            
075300            DELIMITED BY SIZE INTO SSA2                                   
075400     MOVE '  GE' TO GODK-STATUSKODER                                      
075500     CALL CBLTDLI USING GHU 5131-PCB DLI-IO-AREA SSA1 SSA2                
075600     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     SKIP2                                                                
076000 IMS-REPL-WL513111 SECTION.                                               
076100                                                                          
076200     MOVE '  ' TO GODK-STATUSKODER                                        
076300     CALL CBLTDLI USING REPL 5131-PCB DLI-IO-AREA                         
076400     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700     EJECT                                                                
076800                                                                          
076900 IMS-ISRT-WL513111 SECTION.                                               
077000                                                                          
077100     STRING 'WL513101(WDGXKEY  =' W-WDGXKEY-HTYP-5131-X ')'               
077200            DELIMITED BY SIZE INTO SSA1                                   
077300     MOVE 'WL513111' TO SSA2                                              
077400     MOVE '  II' TO GODK-STATUSKODER                                      
077500     CALL CBLTDLI USING ISRT 5131-PCB DLI-IO-AREA SSA1 SSA2               
077600     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
077700     PERFORM IMS-STATUSKONTROLL                                           
077800     .                                                                    
077900     SKIP2                                                                
078000 IMS-DLET-WL513111 SECTION.                                               
078100                                                                          
078200     MOVE '  GE' TO GODK-STATUSKODER                                      
078300     CALL CBLTDLI USING DLET 5131-PCB DLI-IO-AREA                         
078400     MOVE 5131-STATUS-CODE TO STATUS-WS                                   
078500     PERFORM IMS-STATUSKONTROLL                                           
078600     .                                                                    
078700     SKIP2                                                                
078800 IMS-STATUSKONTROLL SECTION.                                              
078900                                                                          
079000     SET STATUS-IX TO 1                                                   
079100     SEARCH GODK-STATUS AT END CALL FELLOG                                
079200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
079300     END-SEARCH                                                           
079400     .                                                                    
