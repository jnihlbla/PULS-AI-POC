000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5014200.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500     DATE-WRITTEN.   SEPT  89.                                            
000600*                                                                         
000700*    REMARKS.                                                             
000800*    FUNKTION.                                                            
000900*        MPP PROGRAM SOM INGÅR I PRISSÄTTNING FÖR EMBALLAGE               
001000*                                SYSTEMET.                                
001100*        MÖJLIGHETER: SÖKNING OCH UPPDATERING AV EN VISS ARTIKEL          
001200*                                 OCH EVENTUELLA EXTRAKOSTNADER           
001300*        PROGRAMMET HETTE TIDIGARE W4011300 /KJH MAJ -95                  
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T142  W5T142U                                     
001700*        MID:         W5I14201                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W5O14201                                            
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77    IDPGM                     PIC X(8)    VALUE 'W5014200'.            
002900 77    JA                        PIC X       VALUE 'J'.                   
003000 77    NEJ                       PIC X       VALUE 'N'.                   
003100 77    INDATA-OK                 PIC X       VALUE 'J'.                   
003200 77    NYCKEL-OK                 PIC X       VALUE 'N'.                   
003300 77    UPPDATERAT                PIC X       VALUE 'N'.                   
003400 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +800 COMP SYNC.        
003600                                                                          
003700 77    WS-IDARTNR                PIC X(9)    VALUE SPACE.                 
003800 77    WS-IDARTNR-NUM            PIC 9(9)    VALUE ZERO.                  
003900 77    WS-IDARTNR-UPP            PIC X(9)    VALUE SPACE.                 
004000 77    WS-IDARTNR-UPP-NUM        PIC 9(9)    VALUE ZERO.                  
004100 77    WS-IDARTNR-SPAR-NUM       PIC 9(9)    VALUE ZERO.                  
004200 77    WS-IDARTNR-RAD1           PIC X(9)    VALUE SPACE.                 
004300 77    WS-IDARTNR-RAD1-NUM       PIC 9(9)    VALUE ZERO.                  
004400                                                                          
004500 77    WS-PRDIRLON               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004600 77    WS-PROVRPAL               PIC S9(4)V9(3)  VALUE +0 COMP-3.         
004700 77    WS-PRDMTRL                PIC S9(6)V9(3)  VALUE +0 COMP-3.         
004800                                                                          
004900 01    WS-IDTRANS                PIC X(4).                                
005000   88    EGEN-BILD                           VALUE '5142'.                
005100     EJECT                                                                
005200                                                                          
005300 01    GENERELLA-SUBPROGRAM.                                              
005400   03    CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500   03    FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600   03    WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005700   03    WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005710   03    W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005800                                                                          
005900*  03 FILLER  -COPY WDECAREA.                                             
006000   EJECT                                                                  
006100                                                                          
006200*  --- PARAMETRAR TILL SUBPGM W005INIT                                    
006300*  01 -COPY WMSGINIT                                                      
006400     EJECT                                                                
006410*  --- PARAMETRAR TILL SUBPGM WMEDKONV                                    
006420*  01 -COPY WMEDAREA.                                                     
006430     EJECT                                                                
006500 01    NYCKLAR-TILL-DLI.                                                  
006600                                                                          
006700   03    W-WDGXKEY-HTYP-5135-X.                                           
006800     05    W-HTYP                PIC 9(4)    VALUE 5135.                  
006900     05    FILLER                PIC X(26)   VALUE LOW-VALUE.             
007000                                                                          
007100   03    W-IDARTNR-WDGX-X.                                                
007200     05    W-IDARTNR-WDGX        PIC S9(9)   VALUE +0  COMP-3.            
007300     SKIP2                                                                
007400                                                                          
007500   03    W-IDARTNR-WDK6-X.                                                
007600     05    W-IDARTNR-WDK6        PIC S9(9)   VALUE +0  COMP-3.            
007700     SKIP2                                                                
007800 01  MESSAGE-CODES.                                                       
007900     03  KOD-KORR-UPPL-FLT       PIC X(3)    VALUE '001'.                 
008000     03  KOD-TRYCK-PF11          PIC X(3)    VALUE '003'.                 
008100     03  KOD-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008200     03  KOD-PF11-INGET-INDATA   PIC X(3)    VALUE '011'.                 
008300     03  KOD-UPPDAT-UTFORD       PIC X(3)    VALUE '101'.                 
008400     03  KOD-MER-INFO-FINNS      PIC X(3)    VALUE '105'.                 
008500     03  KOD-SISTA-SIDAN         PIC X(3)    VALUE '106'.                 
008600     EJECT                                                                
008700******************************************************************        
008800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008900*                                                                         
009000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
009100                                                                          
009200*01    MID -COPY W5I14201.                                                
009300     EJECT                                                                
009400*01    -COPY WMSGAREA                                                     
009500     EJECT                                                                
009600*  03    MOD -COPY W5O14201 -RED MSG-AREA.                                
009700     EJECT                                                                
009800*01    -COPY WMFSAREA                                                     
009900     EJECT                                                                
010000******************************************************************        
010100*                                                                         
010200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010300*                                                                         
010400 01    IMS-WS.                                                            
010500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
010600     SKIP3                                                                
010700*                        **** STATUS-KOD FRÅN IMS                         
010800   03    STATUS-WS               PIC XX.                                  
010900     88    SEGMENT-FINNS                     VALUE '  '.                  
011000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
011100     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
011200     SKIP3                                                                
011300   03    GODK-STATUSKODER.                                                
011400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011500     SKIP3                                                                
011600 01    SSA1                      PIC X(164).                              
011700 01    SSA2                      PIC X(164).                              
011800 01    SSA3                      PIC X(164).                              
011900     EJECT                                                                
012000*                            IMS FUNKTIONSKODER                           
012100*01    -COPY W0003                                                        
012200     EJECT                                                                
012300*                            DLI INPUT-OUTPUT AREA                        
012400 01    DLI-IO-AREA.                                                       
012500   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
012600     SKIP3                                                                
012700*  03    WL513511 -COPY WDGX5136  -RED IO-AREA.                           
012800     EJECT                                                                
012900*                                                                         
013000 01    DLI-IO-AREA-2.                                                     
013100   03    IO-AREA-2               PIC X(900)  VALUE SPACE.                 
013200     SKIP3                                                                
013300*  03    WLARTC11 -COPY WDK611        -RED IO-AREA-2.                     
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01    -COPY W0009     -PRE MSG-                                          
013700     EJECT                                                                
013800*01    -COPY W0008     -PRE USEA-                                         
013900     05  FILLER                  PIC X.                                   
013910     EJECT                                                                
013920*01    -COPY W0008     -PRE 5135-                                         
013930     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100*01    -COPY W0008     -PRE ARTC-                                         
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
014410                                  5135-PCB ARTC-PCB.                      
014500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014510                                   5135-PCB ARTC-PCB.                     
014600                                                                          
014700 STYR SECTION.                                                            
014800     PERFORM IMS-GET-MSG                                                  
014900     IF SEGMENT-FINNS                                                     
015000       PERFORM A-INIT-INPUT                                               
015100       PERFORM B-SPARA-INPUT                                              
015200       PERFORM C-NYCKEL-KONTROLL                                          
015300       IF NYCKEL-OK = JA                                                  
015400         IF EGEN-BILD                                                     
015410           IF MFS-UPDATE                                                  
015500             PERFORM D-UPPDATERA                                          
015600           ELSE                                                           
015700             IF MFS-IDPFK = '7'                                           
015800               PERFORM E-LAES-PF7                                         
015900             ELSE                                                         
016000               IF MFS-IDPFK = '8'                                         
016100                 PERFORM F-BLAEDDRA-PF8                                   
016200               ELSE                                                       
016300                 PERFORM G-ENTER                                          
016400               END-IF                                                     
016500             END-IF                                                       
016600           END-IF                                                         
016610         ELSE                                                             
016611           PERFORM H-VISA-BILD                                            
016620         END-IF                                                           
016700       END-IF                                                             
016800                                                                          
016900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
017000       PERFORM IMS-INSERT-MSG                                             
017100     END-IF                                                               
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT-INPUT SECTION.                                                    
017700                                                                          
017800     IF MSG-DUBBLA-TRANSKODER                                             
017900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I14201                 
018000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
018100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
018300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
018400     ELSE                                                                 
018500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I14201                  
018600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
018700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018800       MOVE SPACE TO MFS-KDTRTYP                                          
018900                     MFS-IDPFK                                            
019000     END-IF                                                               
019100                                                                          
019200     MOVE LOW-VALUE TO MSG-AREA                                           
019300     MOVE 'W5O14201' TO MFS-IDMOD                                         
019400     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
019500     MOVE '5142' TO MOD-IDTRANS                                           
019600                                                                          
019700     IF MFS-KDMFSFOR = '1'                                                
019800       MOVE 'S  '       TO MED-IDSKYLT                                    
019900     ELSE                                                                 
020000       MOVE 'GB '       TO MED-IDSKYLT                                    
020100     END-IF                                                               
020200                                                                          
020300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020400                             MOD-IDARTNR-UPP                              
020500                             MOD-TEMFSFEL                                 
020600                             MOD-TEMFSINF                                 
020700     .                                                                    
020800     EJECT                                                                
020900 B-SPARA-INPUT SECTION.                                                   
021010                                                                          
021020     MOVE ALL '+' TO MSGI-WMSGINIT                                        
021030     MOVE '001'             TO MSGI-KDCALL                                
021040     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021041     MOVE '5142'            TO MSGI-IDTRANS                               
021042     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021050                                                                          
021060     IF MFS-IDTRANS = '5142'                                              
021070     OR (MID-IDARTNR-IN NUMERIC                                           
021071     AND MID-IDARTNR-IN > ZERO)                                           
021080         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
021090     END-IF                                                               
021091     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021092     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
021093     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
021700                                                                          
021800     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
021900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
022000                                                                          
022600     .                                                                    
022700     EJECT                                                                
022800 C-NYCKEL-KONTROLL SECTION.                                               
022900                                                                          
023000     IF WS-IDARTNR NUMERIC                                                
023100       MOVE JA TO NYCKEL-OK                                               
023200     ELSE                                                                 
023300       IF EGEN-BILD                                                       
023400         MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                     
023500         CALL WMEDKONV USING MED-WMEDAREA                                 
023600         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
023700       END-IF                                                             
023800     END-IF                                                               
023900                                                                          
024000     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
024100       MOVE MID-IDARTNR-UPP TO WS-IDARTNR-UPP                             
024200       INSPECT WS-IDARTNR-UPP REPLACING LEADING SPACE BY ZERO             
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 D-UPPDATERA SECTION.                                                     
024700                                                                          
024800     IF MID-KDCMD-DELETE                                                  
024900       PERFORM DA-RADERA-IDARTNR                                          
025000     ELSE                                                                 
025100       IF MID-KDCMD-INGENTING                                             
025200         IF MID-PRDIRLON = ALL '+' AND MID-PRDMTRL = ALL '+'              
025300             AND MID-PROVRPAL = ALL '+' AND MID-FLFPTILL = '+'            
025400           PERFORM MFS-ROER-EJ-BILD                                       
025500           MOVE KOD-PF11-INGET-INDATA   TO MED-IDMFSFEL                   
025600           CALL WMEDKONV USING MED-WMEDAREA                               
025700           MOVE MED-MFSFEL              TO MOD-TEMFSFEL                   
025800         ELSE                                                             
025900           PERFORM DB-INMATNING-KONTROLL                                  
026000           IF INDATA-OK = JA                                              
026100             PERFORM DC-AENDRING-TILLAEGG                                 
026200           ELSE                                                           
026300             PERFORM MFS-LAES-BILD-IGEN                                   
026400           END-IF                                                         
026500         END-IF                                                           
026600       ELSE                                                               
026700         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR                        
026800         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD                              
026900         MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                     
027000         CALL WMEDKONV USING MED-WMEDAREA                                 
027100         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
027200         PERFORM MFS-LAES-BILD-IGEN                                       
027300       END-IF                                                             
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 DA-RADERA-IDARTNR SECTION.                                               
027800                                                                          
027900     IF MID-IDARTNR-UPP NOT = ALL '+' AND WS-IDARTNR-UPP NUMERIC          
028000       MOVE WS-IDARTNR-UPP TO WS-IDARTNR-UPP-NUM                          
028100       MOVE WS-IDARTNR-UPP-NUM TO W-IDARTNR-WDGX                          
028200                                                                          
028300       PERFORM IMS-GHU-WL513511                                           
028400       IF SEGMENT-FINNS                                                   
028500         PERFORM IMS-DLET-WL513511                                        
028600         PERFORM MFS-RENSA-UPP-RAD                                        
028700         PERFORM DAA-VISA-EFTER-BORTTAG                                   
028800         MOVE KOD-UPPDAT-UTFORD       TO MED-IDMFSINF                     
028900         CALL WMEDKONV USING MED-WMEDAREA                                 
029000         MOVE MED-MFSINF              TO MOD-TEMFSINF                     
029100       ELSE                                                               
029200         PERFORM MFS-ROER-EJ-BILD                                         
029300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR-UPP               
029400         MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPP                        
029500         MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                     
029600         CALL WMEDKONV USING MED-WMEDAREA                                 
029700         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
029800       END-IF                                                             
029900     ELSE                                                                 
030000       PERFORM MFS-ROER-EJ-BILD                                           
030100       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR-UPP                     
030200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPP                          
030300       MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                       
030400       CALL WMEDKONV USING MED-WMEDAREA                                   
030500       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 DAA-VISA-EFTER-BORTTAG SECTION.                                          
031000                                                                          
031100     PERFORM IMS-GU-WL513501                                              
031200     MOVE MID-IDARTNR-SPAR-RAD1 TO WS-IDARTNR-RAD1-NUM                    
031300     MOVE WS-IDARTNR-RAD1-NUM TO W-IDARTNR-WDGX                           
031400                                                                          
031500     PERFORM IMS-GET-WL513511                                             
031600     IF SEGMENT-FINNS                                                     
031700       PERFORM S01-TABELL-BEHANDLING                                      
031800     END-IF                                                               
031900     .                                                                    
032000     EJECT                                                                
032100 DB-INMATNING-KONTROLL SECTION.                                           
032200                                                                          
032300     IF MID-IDARTNR-UPP NOT = ALL '+' AND WS-IDARTNR-UPP NUMERIC          
032400       MOVE WS-IDARTNR-UPP TO WS-IDARTNR-UPP-NUM                          
032500       MOVE WS-IDARTNR-UPP-NUM TO W-IDARTNR-WDK6                          
032600                                                                          
032700       PERFORM IMS-GU-WLARTC01                                            
032800       IF SEGMENT-FINNS                                                   
032900         PERFORM IMS-GU-WLARTC11                                          
033000         IF SEGMENT-FINNS                                                 
033100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR-UPP               
033200         ELSE                                                             
033300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR-UPP                 
033400           MOVE KOD-URVAL-SAKNAS      TO MED-IDMFSFEL                     
033500           CALL WMEDKONV USING MED-WMEDAREA                               
033600           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
033700           MOVE NEJ TO INDATA-OK                                          
033800         END-IF                                                           
033900       ELSE                                                               
034000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR-UPP                   
034100         MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                     
034200         CALL WMEDKONV USING MED-WMEDAREA                                 
034300         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
034400         MOVE NEJ TO INDATA-OK                                            
034500       END-IF                                                             
034600     ELSE                                                                 
034700       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR-UPP                     
034800       MOVE KOD-KORR-UPPL-FLT       TO MED-IDMFSFEL                       
034900       CALL WMEDKONV USING MED-WMEDAREA                                   
035000       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
035100       MOVE NEJ TO INDATA-OK                                              
035200     END-IF                                                               
035300                                                                          
035400     IF MID-PRDIRLON NOT = ALL '+'                                        
035500       MOVE +4           TO DEC-KVHELTAL                                  
035600       MOVE +3           TO DEC-KVDECIMAL                                 
035700       MOVE MID-PRDIRLON TO DEC-IDFRIDATA                                 
035800       CALL WDECEDIT USING DEC-WDECAREA                                   
035900       IF DEC-KDSVAR-OK                                                   
036000         MOVE DEC-IDEDITDATA TO WS-PRDIRLON                               
036100         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRDIRLON-ATTR-UPP                
036200       ELSE                                                               
036300         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDIRLON-ATTR-UPP                  
036400         MOVE KOD-KORR-UPPL-FLT TO MED-IDMFSFEL                           
036500         CALL WMEDKONV USING MED-WMEDAREA                                 
036600         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
036700         MOVE NEJ TO INDATA-OK                                            
036800       END-IF                                                             
036900     END-IF                                                               
037000                                                                          
037100     IF MID-PRDMTRL NOT = ALL '+'                                         
037200       MOVE +6           TO DEC-KVHELTAL                                  
037300       MOVE +3           TO DEC-KVDECIMAL                                 
037400       MOVE MID-PRDMTRL TO DEC-IDFRIDATA                                  
037500       CALL WDECEDIT USING DEC-WDECAREA                                   
037600       IF DEC-KDSVAR-OK                                                   
037700         MOVE DEC-IDEDITDATA TO WS-PRDMTRL                                
037800         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRDMTRL-ATTR-UPP                 
037900       ELSE                                                               
038000         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDMTRL-ATTR-UPP                   
038100         MOVE KOD-KORR-UPPL-FLT TO MED-IDMFSFEL                           
038200         CALL WMEDKONV USING MED-WMEDAREA                                 
038300         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
038400         MOVE NEJ TO INDATA-OK                                            
038500       END-IF                                                             
038600     END-IF                                                               
038700                                                                          
038800     IF MID-PROVRPAL NOT = ALL '+'                                        
038900       MOVE +4           TO DEC-KVHELTAL                                  
039000       MOVE +3           TO DEC-KVDECIMAL                                 
039100       MOVE MID-PROVRPAL TO DEC-IDFRIDATA                                 
039200       CALL WDECEDIT USING DEC-WDECAREA                                   
039300       IF DEC-KDSVAR-OK                                                   
039400         MOVE DEC-IDEDITDATA TO WS-PROVRPAL                               
039500         MOVE MFS-NUM-FAELT-RAETT TO MOD-PROVRPAL-ATTR-UPP                
039600       ELSE                                                               
039700         MOVE MFS-NUM-FAELT-FEL TO MOD-PROVRPAL-ATTR-UPP                  
039800         MOVE KOD-KORR-UPPL-FLT TO MED-IDMFSFEL                           
039900         CALL WMEDKONV USING MED-WMEDAREA                                 
040000         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
040100         MOVE NEJ TO INDATA-OK                                            
040200       END-IF                                                             
040300     END-IF                                                               
040400                                                                          
040500     IF MID-FLFPTILL NOT = ALL '+'                                        
040600       IF MID-FLFPTILL = 'J' OR 'N' OR 'Y'                                
040700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFPTILL-ATTR-UPP               
040800       ELSE                                                               
040900         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPTILL-ATTR-UPP                 
041000         MOVE KOD-KORR-UPPL-FLT TO MED-IDMFSFEL                           
041100         CALL WMEDKONV USING MED-WMEDAREA                                 
041200         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
041300         MOVE NEJ TO INDATA-OK                                            
041400       END-IF                                                             
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 DC-AENDRING-TILLAEGG SECTION.                                            
041900                                                                          
042000     MOVE WS-IDARTNR-UPP TO WS-IDARTNR-UPP-NUM                            
042100     MOVE WS-IDARTNR-UPP-NUM TO W-IDARTNR-WDGX                            
042200     PERFORM IMS-GHU-WL513511                                             
042300     IF SEGMENT-FINNS                                                     
042400       PERFORM DCA-AENDRING                                               
042500     ELSE                                                                 
042600       PERFORM DCB-TILLAEGG                                               
042700     END-IF                                                               
042800     IF UPPDATERAT = JA                                                   
042900       PERFORM MFS-RENSA-UPP-RAD                                          
043000       PERFORM DCC-VISA-EFTER-UPPDATERING                                 
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 DCA-AENDRING SECTION.                                                    
043500                                                                          
043600     IF MID-PRDIRLON NOT = ALL '+'                                        
043700       MOVE WS-PRDIRLON   TO 5136-PRDIRLON                                
043800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDIRLON-ATTR(1)                 
043900     END-IF                                                               
044000                                                                          
044100     IF MID-PROVRPAL NOT = ALL '+'                                        
044200       MOVE WS-PROVRPAL   TO 5136-PROVRPAL                                
044300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PROVRPAL-ATTR(1)                 
044400     END-IF                                                               
044500                                                                          
044600     IF MID-PRDMTRL  NOT = ALL '+'                                        
044700       MOVE WS-PRDMTRL    TO 5136-PRDMTRL                                 
044800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRDMTRL-ATTR(1)                  
044900     END-IF                                                               
045000                                                                          
045100     IF MID-FLFPTILL NOT = ALL '+'                                        
045200       MOVE MID-FLFPTILL  TO 5136-FLFPTILL                                
045300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFPTILL-ATTR(1)                 
045400     END-IF                                                               
045500                                                                          
045600     PERFORM IMS-REPL-WL513511                                            
045700     MOVE JA TO UPPDATERAT                                                
045800     .                                                                    
045900     EJECT                                                                
046000 DCB-TILLAEGG SECTION.                                                    
046100                                                                          
046200     IF MID-PRDIRLON = ALL '+' OR MID-PRDMTRL = ALL '+'                   
046300         OR MID-PROVRPAL = ALL '+' OR MID-FLFPTILL = '+'                  
046400       PERFORM MFS-LAES-BILD-IGEN                                         
046500                                                                          
046600       IF MID-PRDIRLON = ALL '+'                                          
046700         MOVE MFS-NUM-FAELT-FEL TO MOD-PRDIRLON-ATTR-UPP                  
046800       ELSE                                                               
046900         IF MID-PRDMTRL = ALL '+'                                         
047000           MOVE MFS-NUM-FAELT-FEL TO MOD-PRDMTRL-ATTR-UPP                 
047100         ELSE                                                             
047200           IF MID-PROVRPAL = ALL '+'                                      
047300             MOVE MFS-NUM-FAELT-FEL TO MOD-PROVRPAL-ATTR-UPP              
047400           ELSE                                                           
047500             IF MID-FLFPTILL = ALL '+'                                    
047600               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPTILL-ATTR-UPP           
047700             END-IF                                                       
047800           END-IF                                                         
047900         END-IF                                                           
048000       END-IF                                                             
048100       MOVE KOD-KORR-UPPL-FLT TO MED-IDMFSFEL                             
048200       CALL WMEDKONV USING MED-WMEDAREA                                   
048300       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
048400     ELSE                                                                 
048500       MOVE WS-IDARTNR-UPP-NUM TO 5136-IDARTNR                            
048600       MOVE WS-PRDIRLON        TO 5136-PRDIRLON                           
048700       MOVE WS-PROVRPAL        TO 5136-PROVRPAL                           
048800       MOVE WS-PRDMTRL         TO 5136-PRDMTRL                            
048900       MOVE MID-FLFPTILL       TO 5136-FLFPTILL                           
049000                                                                          
049100       PERFORM IMS-ISRT-WL513511                                          
049200       MOVE JA TO UPPDATERAT                                              
049300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR(1)                  
049400                                     MOD-PRDIRLON-ATTR(1)                 
049500                                     MOD-PRDMTRL-ATTR(1)                  
049600                                     MOD-PROVRPAL-ATTR(1)                 
049700                                     MOD-FLFPTILL-ATTR(1)                 
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100 DCC-VISA-EFTER-UPPDATERING SECTION.                                      
050200                                                                          
050300     PERFORM IMS-GU-WL513501                                              
050400     MOVE WS-IDARTNR-UPP-NUM TO W-IDARTNR-WDGX                            
050500                                                                          
050600     PERFORM IMS-GET-WL513511                                             
050700     IF SEGMENT-FINNS                                                     
050800       PERFORM S01-TABELL-BEHANDLING                                      
050900       MOVE KOD-UPPDAT-UTFORD       TO MED-IDMFSINF                       
051000       CALL WMEDKONV USING MED-WMEDAREA                                   
051100       MOVE MED-MFSINF              TO MOD-TEMFSINF                       
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500 E-LAES-PF7 SECTION.                                                      
051600                                                                          
051700     PERFORM IMS-GU-WL513501                                              
051800     MOVE WS-IDARTNR TO WS-IDARTNR-NUM                                    
051900     MOVE WS-IDARTNR-NUM TO W-IDARTNR-WDGX                                
052000                                                                          
052100     PERFORM IMS-GET-WL513511                                             
052200     PERFORM S01-TABELL-BEHANDLING                                        
052300     .                                                                    
052400     EJECT                                                                
052500 F-BLAEDDRA-PF8 SECTION.                                                  
052600                                                                          
052700     IF MID-IDARTNR-SPAR NOT = ZERO                                       
052800       PERFORM IMS-GU-WL513501                                            
052900       MOVE MID-IDARTNR-SPAR TO WS-IDARTNR-SPAR-NUM                       
053000       MOVE WS-IDARTNR-SPAR-NUM TO W-IDARTNR-WDGX                         
053100                                                                          
053200       PERFORM IMS-GNP-KVAL-WL513511                                      
053300       PERFORM S01-TABELL-BEHANDLING                                      
053400     ELSE                                                                 
053500       PERFORM MFS-ROER-EJ-BILD                                           
053600       MOVE KOD-SISTA-SIDAN         TO MED-IDMFSINF                       
053700       CALL WMEDKONV USING MED-WMEDAREA                                   
053800       MOVE MED-MFSINF              TO MOD-TEMFSINF                       
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 G-ENTER SECTION.                                                         
054300                                                                          
054400     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
054500       PERFORM MFS-LAES-BILD-IGEN                                         
054600       PERFORM MFS-LAES-IN-INMATNING                                      
054700       MOVE KOD-TRYCK-PF11          TO MED-IDMFSFEL                       
054800       CALL WMEDKONV USING MED-WMEDAREA                                   
054900       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
055000     ELSE                                                                 
055100       PERFORM IMS-GU-WL513501                                            
055200       MOVE WS-IDARTNR TO WS-IDARTNR-NUM                                  
055300       MOVE WS-IDARTNR-NUM TO W-IDARTNR-WDGX                              
055400                                                                          
055500       PERFORM IMS-GET-WL513511                                           
055600       IF SEGMENT-FINNS                                                   
055700         PERFORM S01-TABELL-BEHANDLING                                    
055800       ELSE                                                               
055900         MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                     
056000         CALL WMEDKONV USING MED-WMEDAREA                                 
056100         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
056200       END-IF                                                             
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056510 H-VISA-BILD SECTION.                                                     
056520                                                                          
056591     PERFORM IMS-GU-WL513501                                              
056592     MOVE WS-IDARTNR TO WS-IDARTNR-NUM                                    
056593     MOVE WS-IDARTNR-NUM TO W-IDARTNR-WDGX                                
056594                                                                          
056595     PERFORM IMS-GET-WL513511                                             
056596     IF SEGMENT-FINNS                                                     
056597       PERFORM S01-TABELL-BEHANDLING                                      
056598     ELSE                                                                 
056599       MOVE KOD-URVAL-SAKNAS        TO MED-IDMFSFEL                       
056600       CALL WMEDKONV USING MED-WMEDAREA                                   
056601       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
056602     END-IF                                                               
056604     .                                                                    
056605     EJECT                                                                
056610 S01-TABELL-BEHANDLING SECTION.                                           
056700                                                                          
056800     MOVE +1 TO IX                                                        
056900                                                                          
057000     PERFORM UNTIL IX > 12 OR SEGMENT-SAKNAS                              
057100       MOVE 5136-IDARTNR         TO W-IDARTNR-WDK6                        
057200       PERFORM IMS-GU-WLARTC11                                            
057300       MOVE 5136-IDARTNR         TO MOD-IDARTNR-RAD(IX)                   
057400       IF SEGMENT-FINNS                                                   
057500         MOVE CLAG-BEFT          TO MOD-BEFT-RAD(IX)                      
057600       ELSE                                                               
057700         MOVE ZERO               TO MOD-BEFT-RAD(IX)                      
057800       END-IF                                                             
057900       MOVE 5136-PRDIRLON        TO MOD-PRDIRLON-RAD(IX)                  
058000       MOVE 5136-PRDMTRL         TO MOD-PRDMTRL-RAD(IX)                   
058100       MOVE 5136-PROVRPAL        TO MOD-PROVRPAL-RAD(IX)                  
058200       MOVE 5136-FLFPTILL        TO MOD-FLFPTILL-RAD(IX)                  
058300       ADD +1 TO IX                                                       
058400       PERFORM IMS-GNP-WL513511                                           
058500     END-PERFORM                                                          
058600                                                                          
058700     MOVE MOD-IDARTNR-RAD(1)     TO WS-IDARTNR-RAD1                       
058800     MOVE WS-IDARTNR-RAD1        TO MOD-IDARTNR-SPAR-RAD1                 
058900                                                                          
059000     IF SEGMENT-FINNS                                                     
059100       MOVE 5136-IDARTNR         TO WS-IDARTNR-SPAR-NUM                   
059200       MOVE WS-IDARTNR-SPAR-NUM  TO MOD-IDARTNR-SPAR                      
059300       MOVE KOD-MER-INFO-FINNS   TO MED-IDMFSINF                          
059400       CALL WMEDKONV USING MED-WMEDAREA                                   
059500       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
059600     ELSE                                                                 
059700       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-SPAR                           
059800       MOVE KOD-SISTA-SIDAN         TO MED-IDMFSINF                       
059900       CALL WMEDKONV USING MED-WMEDAREA                                   
060000       MOVE MED-MFSINF              TO MOD-TEMFSINF                       
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 MFS-LAES-BILD-IGEN SECTION.                                              
060500                                                                          
060600     MOVE +1 TO IX                                                        
060700                                                                          
060800     PERFORM UNTIL IX > 12                                                
060900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD(IX)                      
061000                                 MOD-BEFT-RAD(IX)                         
061100                                 MOD-PRDIRLON-RAD(IX)                     
061200                                 MOD-PRDMTRL-RAD(IX)                      
061300                                 MOD-PROVRPAL-RAD(IX)                     
061400                                 MOD-FLFPTILL-RAD(IX)                     
061500       ADD +1 TO IX                                                       
061600     END-PERFORM                                                          
061700                                                                          
061800     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
061900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPP                          
062000     END-IF                                                               
062100                                                                          
062200     IF MID-PRDIRLON NOT = ALL '+'                                        
062300       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDIRLON-UPP                         
062400     END-IF                                                               
062500                                                                          
062600     IF MID-PRDMTRL NOT = ALL '+'                                         
062700       MOVE MFS-ROER-EJ-FAELT TO MOD-PRDMTRL-UPP                          
062800     END-IF                                                               
062900                                                                          
063000     IF MID-PROVRPAL NOT = ALL '+'                                        
063100       MOVE MFS-ROER-EJ-FAELT TO MOD-PROVRPAL-UPP                         
063200     END-IF                                                               
063300                                                                          
063400     IF MID-FLFPTILL NOT = ALL '+'                                        
063500       MOVE MFS-ROER-EJ-FAELT TO MOD-FLFPTILL-UPP                         
063600     END-IF                                                               
063700                                                                          
063800     .                                                                    
063900     EJECT                                                                
064000 MFS-LAES-IN-INMATNING SECTION.                                           
064100                                                                          
064200     IF MID-IDARTNR-UPP NOT = ALL '+'                                     
064300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR-UPP                 
064400     END-IF                                                               
064500                                                                          
064600     IF MID-PRDIRLON NOT = ALL '+'                                        
064700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDIRLON-ATTR-UPP                
064800     END-IF                                                               
064900                                                                          
065000     IF MID-PRDMTRL NOT = ALL '+'                                         
065100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRDMTRL-ATTR-UPP                 
065200     END-IF                                                               
065300                                                                          
065400     IF MID-PROVRPAL NOT = ALL '+'                                        
065500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PROVRPAL-ATTR-UPP                
065600     END-IF                                                               
065700                                                                          
065800     IF MID-FLFPTILL NOT = ALL '+'                                        
065900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPTILL-ATTR-UPP                
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 MFS-ROER-EJ-BILD SECTION.                                                
066400                                                                          
066500     MOVE +1 TO IX                                                        
066600                                                                          
066700     PERFORM UNTIL IX > 12                                                
066800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD(IX)                      
066900                                 MOD-BEFT-RAD(IX)                         
067000                                 MOD-PRDIRLON-RAD(IX)                     
067100                                 MOD-PRDMTRL-RAD(IX)                      
067200                                 MOD-PROVRPAL-RAD(IX)                     
067300                                 MOD-FLFPTILL-RAD(IX)                     
067400       ADD +1 TO IX                                                       
067500     END-PERFORM                                                          
067600                                                                          
067700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-SPAR                           
067800                               MOD-TEMFSINF                               
067900     .                                                                    
068000     EJECT                                                                
068100 MFS-RENSA-UPP-RAD SECTION.                                               
068200                                                                          
068300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPP                              
068400                             MOD-PRDIRLON-UPP                             
068500                             MOD-PROVRPAL-UPP                             
068600                             MOD-PRDMTRL-UPP                              
068700                             MOD-FLFPTILL-UPP                             
068800                             MOD-KDCMD                                    
068900     .                                                                    
069000     EJECT                                                                
069100* IMS SEKTIONER ******************************                            
069200     SKIP3                                                                
069300 IMS-GET-MSG SECTION.                                                     
069400                                                                          
069500     MOVE '  QC' TO GODK-STATUSKODER                                      
069600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069800     PERFORM IMS-STATUSKONTROLL                                           
069900     .                                                                    
070000     SKIP2                                                                
070100 IMS-INSERT-MSG SECTION.                                                  
070200                                                                          
070300     IF ENGLISH-TEXT                                                      
070400       MOVE '0' TO MFS-KDHUVOMR                                           
070500     END-IF                                                               
070600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070700     MOVE SPACE TO GODK-STATUSKODER                                       
070800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     EJECT                                                                
071300 IMS-GU-WL513501 SECTION.                                                 
071400                                                                          
071500     STRING 'WL513501(WDGXKEY  =' W-WDGXKEY-HTYP-5135-X ')'               
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     MOVE '  ' TO GODK-STATUSKODER                                        
071800     CALL CBLTDLI USING GU 5135-PCB DLI-IO-AREA SSA1                      
071900     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     SKIP2                                                                
072300 IMS-GET-WL513511 SECTION.                                                
072400                                                                          
072500     STRING 'WL513511(IDARTNR >=' W-IDARTNR-WDGX-X ')'                    
072600            DELIMITED BY SIZE INTO SSA1                                   
072700     MOVE '  GE' TO GODK-STATUSKODER                                      
072800     CALL CBLTDLI USING GNP 5135-PCB DLI-IO-AREA SSA1                     
072900     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUSKONTROLL                                           
073100     .                                                                    
073200     SKIP2                                                                
073300 IMS-GNP-WL513511 SECTION.                                                
073400                                                                          
073500     MOVE 'WL513511  ' TO SSA1                                            
073600     MOVE '  GE' TO GODK-STATUSKODER                                      
073700     CALL CBLTDLI USING GNP 5135-PCB DLI-IO-AREA SSA1                     
073800     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
073900     PERFORM IMS-STATUSKONTROLL                                           
074000     .                                                                    
074100     EJECT                                                                
074200 IMS-GNP-KVAL-WL513511 SECTION.                                           
074300                                                                          
074400     STRING 'WL513511(IDARTNR  =' W-IDARTNR-WDGX-X ')'                    
074500            DELIMITED BY SIZE INTO SSA1                                   
074600     MOVE '  GE' TO GODK-STATUSKODER                                      
074700     CALL CBLTDLI USING GNP 5135-PCB DLI-IO-AREA SSA1                     
074800     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
074900     PERFORM IMS-STATUSKONTROLL                                           
075000     .                                                                    
075100     SKIP2                                                                
075200 IMS-GHU-WL513511 SECTION.                                                
075300                                                                          
075400     STRING 'WL513501(WDGXKEY  =' W-WDGXKEY-HTYP-5135-X ')'               
075500            DELIMITED BY SIZE INTO SSA1                                   
075600     STRING 'WL513511(IDARTNR  =' W-IDARTNR-WDGX-X ')'                    
075700            DELIMITED BY SIZE INTO SSA2                                   
075800     MOVE '  GE' TO GODK-STATUSKODER                                      
075900     CALL CBLTDLI USING GHU 5135-PCB DLI-IO-AREA SSA1 SSA2                
076000     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     .                                                                    
076300     SKIP2                                                                
076400 IMS-REPL-WL513511 SECTION.                                               
076500                                                                          
076600     MOVE '  ' TO GODK-STATUSKODER                                        
076700     CALL CBLTDLI USING REPL 5135-PCB DLI-IO-AREA                         
076800     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
076900     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
077100     EJECT                                                                
077200 IMS-ISRT-WL513511 SECTION.                                               
077300                                                                          
077400     STRING 'WL513501(WDGXKEY  =' W-WDGXKEY-HTYP-5135-X ')'               
077500            DELIMITED BY SIZE INTO SSA1                                   
077600     MOVE 'WL513511' TO SSA2                                              
077700     MOVE '  II' TO GODK-STATUSKODER                                      
077800     CALL CBLTDLI USING ISRT 5135-PCB DLI-IO-AREA SSA1 SSA2               
077900     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
078000     PERFORM IMS-STATUSKONTROLL                                           
078100     .                                                                    
078200     SKIP2                                                                
078300 IMS-DLET-WL513511 SECTION.                                               
078400                                                                          
078500     MOVE '  ' TO GODK-STATUSKODER                                        
078600     CALL CBLTDLI USING DLET 5135-PCB DLI-IO-AREA                         
078700     MOVE 5135-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     SKIP2                                                                
079100 IMS-GU-WLARTC01 SECTION.                                                 
079200                                                                          
079300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
079400            DELIMITED BY SIZE INTO SSA1                                   
079500     MOVE '  GE' TO GODK-STATUSKODER                                      
079600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
079700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
079800     PERFORM IMS-STATUSKONTROLL                                           
079900     .                                                                    
080000     EJECT                                                                
080100 IMS-GU-WLARTC11 SECTION.                                                 
080200                                                                          
080300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
080400            DELIMITED BY SIZE INTO SSA1                                   
080500     STRING 'WLARTC11(KDSEGKEY =1)'                                       
080600            DELIMITED BY SIZE INTO SSA2                                   
080700     MOVE '  GE' TO GODK-STATUSKODER                                      
080800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2               
080900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUSKONTROLL                                           
081100     .                                                                    
081200     EJECT                                                                
081300 IMS-STATUSKONTROLL SECTION.                                              
081400                                                                          
081500     SET STATUS-IX TO 1                                                   
081600     SEARCH GODK-STATUS AT END CALL FELLOG                                
081700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
081800     END-SEARCH                                                           
081900     .                                                                    
