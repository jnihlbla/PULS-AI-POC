000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4011100.                                                
000400 AUTHOR.         I BENGTSON.                                              
000500 DATE-WRITTEN.   JULI 1985.                                               
000510 DATE-COMPILED.                                                           
000600                                                                          
001200*      FUNKTION:        PROGRAMMET LISTAR OCH UPPDATERAR                  
001300*                       KOLLIKODSREGISTRET.                               
001400*        NYUPPLÄGG KAN BARA GÖRAS PÅ 1:STA RADEN PÅ BILDEN W4111          
001500*        ÄNDRING                     "                                    
001600*        BORTTAG                     "                                    
001700*                                                                         
001800*                                                                         
001900*      ANVÄNDARE:       PACKNINGSRAPPORTÖRERNA,                           
002000*                       LASTNINGSPERSONAL,                                
002100*                       ORDERKONTORET,                                    
002200*                       ANSV. FÖR UPPDAT. FÖRPACKNINGSAVD.                
002300*                                                                         
005800*    INDATA.                                                              
005900*        TRANSAKTION: W4T111                                              
006000*        MID:         W4I11101                                            
006100*                                                                         
006200*    UTDATA.                                                              
006300*        MOD:         W4O11101                                            
006300*    E-TRACKER: 5944411 INFO OM EMB-SPACE                                 
006400*    SKIP3                                                                
006500 ENVIRONMENT DIVISION.                                                    
006600     SKIP3                                                                
006700 DATA DIVISION.                                                           
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
006901                                                                          
006910*    -- CHECKED BY WY2000                                                 
007000 77  IDPGM                     PIC X(8)     VALUE 'W4011100'.             
007200 77  JA                        PIC X        VALUE 'J'.                    
007300 77  NEJ                       PIC X        VALUE 'N'.                    
007400 77  W-NYSIDA                  PIC X        VALUE '8'.                    
007500 77  W-BORJAN                  PIC X        VALUE '7'.                    
007600 77  KOLLIKOD-WS               PIC X(8)     VALUE SPACE.                  
007700 77  NYUPPLAEGG                PIC X        VALUE 'N'.                    
007800 77  BORTTAG                   PIC X        VALUE 'B'.                    
007810 77  BORTTAG-ENG               PIC X        VALUE 'D'.                    
007900 77  AENDRA                    PIC X        VALUE 'Ä'.                    
007910 77  AENDRA-ENG                PIC X        VALUE 'C'.                    
008000 77  INPUT-OK                  PIC X        VALUE 'J'.                    
008100 77  INDX                      PIC S9(9)    VALUE +0    COMP SYNC.        
008200 77  SPRAK-INDX                PIC S9(9)    VALUE +0    COMP SYNC.        
008300 77  MAX-LINE                  PIC S9(9)    VALUE +13   COMP SYNC.        
008400 77  MAX-MOD-LAENGD            PIC S9(4)    VALUE +1095 COMP SYNC.        
008500 77  WS-IDTRANS                PIC X(4).                                  
008600     88  WS-GODKAEND-BILD                   VALUE '4111'.                 
008700*                                                                         
008800 77  WS-DIKOLLIL                 PIC S9(5)   COMP-3.                      
008900 77  WS-DIKOLLIB                 PIC S9(3)   COMP-3.                      
009000 77  WS-DIKOLLIH                 PIC S9(3)   COMP-3.                      
009100 77  WS-VKTARA                   PIC S9(6)V9(1)   COMP-3.                 
009200*                                                                         
009300 01  WS-KDMATT                   PIC X.                                   
009400     88 US-MEASUREMENT           VALUE 'U'.                               
009500     88 SIS-MEASUREMENT          VALUE 'S'.                               
009600*                                                                         
009700 01    WS-EMB-KDKOLLI.                                                    
009800   03    WS-EMB-KDKOLLI-PRE      PIC X(1).                                
009900   03    FILLER                  PIC X(7).                                
010000*                                                                         
010100 01    NYCKLAR-TILL-DLI.                                                  
010200   03    W-KOLLIKOD-X.                                                    
010300     05    W-KOLLIKOD            PIC X(8).                                
010400*                                                                         
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010810     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010900     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
011000*                                                                         
011100*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
011200*                                                                         
011300 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
011400*01 -COPY WMSGINIT                                                        
011500     SKIP2                                                                
011600*                                                                         
011700 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
011800*    -COPY WWOMVAND                                                       
011900     SKIP2                                                                
012000                                                                          
012100 01  FILLER                      PIC X(16)  VALUE 'WDECAREA '.            
012200*01  -COPY WDECAREA                                                       
012300     EJECT                                                                
012400 01    MEDDELANDE.                                                        
012500   03    FEL1.                                                            
012600     05    FILLER                PIC X(40)   VALUE                        
012700             'KOLLIKOD SAKNAS                        '.                   
012800     05    FILLER                PIC X(40)   VALUE                        
012900             'CASE CODE MISSING                      '.                   
013000   03    FILLER REDEFINES FEL1.                                           
013100     05    FEL-1 OCCURS 2        PIC X(40).                               
013200                                                                          
013300   03    FEL2.                                                            
013400     05    FILLER                PIC X(40)   VALUE                        
013500             'FÖR UPPDATERING TRYCK PF11             '.                   
013600     05    FILLER                PIC X(40)   VALUE                        
013700             'FOR UPPDATE PRESS PF11                 '.                   
013800   03    FILLER REDEFINES FEL2.                                           
013900     05    FEL-2 OCCURS 2        PIC X(40).                               
014000                                                                          
014100   03    FEL4.                                                            
014200     05    FILLER                PIC X(40)   VALUE                        
014300             'KOLLIKOD FINNS I KOLLIKODSREGISTRET    '.                   
014400     05    FILLER                PIC X(40)   VALUE                        
014500             'CASE CODE IS IN THE CASE CODE DATABASE '.                   
014600   03    FILLER REDEFINES FEL4.                                           
014700     05    FEL-4 OCCURS 2        PIC X(40).                               
014800                                                                          
014900   03    FEL5.                                                            
015000     05    FILLER                PIC X(40)   VALUE                        
015100             'UPPLYSTA FÄLT FEL                      '.                   
015200     05    FILLER                PIC X(40)   VALUE                        
015300             'HIGHLIGHTED FIELDS WRONG               '.                   
015400   03    FILLER REDEFINES FEL5.                                           
015500     05    FEL-5 OCCURS 2        PIC X(40).                               
015600     EJECT                                                                
015700   03    MED1.                                                            
015800     05    FILLER                PIC X(40)   VALUE                        
015900             'KOLLIKODEN ÄR UPPLAGD                  '.                   
016000     05    FILLER                PIC X(40)   VALUE                        
016100             'CASE CODE REGISTERED                   '.                   
016200   03    FILLER REDEFINES MED1.                                           
016300     05    MED-1 OCCURS 2        PIC X(40).                               
016400                                                                          
016500   03    MED2.                                                            
016600     05    FILLER                PIC X(40)   VALUE                        
016700             'KOLLIKODEN ÄR BORTTAGEN                '.                   
016800     05    FILLER                PIC X(40)   VALUE                        
016900             'CASE CODE DELETED                      '.                   
017000   03    FILLER REDEFINES MED2.                                           
017100     05    MED-2 OCCURS 2        PIC X(40).                               
017200                                                                          
017300   03    MED3.                                                            
017400     05    FILLER                PIC X(40)   VALUE                        
017500             'KOLLIKODEN ÄR UPPDATERAD               '.                   
017600     05    FILLER                PIC X(40)   VALUE                        
017700             'CASE CODE OK                           '.                   
017800   03    FILLER REDEFINES MED3.                                           
017900     05    MED-3 OCCURS 2        PIC X(40).                               
018000     EJECT                                                                
018100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
018200                                                                          
018300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
018400     SKIP3                                                                
018500*01    MID -COPY W4I11101.                                                
018600     EJECT                                                                
018700*01    -COPY WMSGAREA                                                     
018800     EJECT                                                                
018900*  03    MOD -COPY W4O11101  -RED MSG-AREA.                               
019000     EJECT                                                                
019100*01    -COPY WMFSAREA                                                     
019200     EJECT                                                                
019300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019400                                                                          
019500 01    IMS-WS.                                                            
019600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
019700     SKIP3                                                                
019800*                        **** STATUS-KOD FRÅN IMS                         
019900   03    STATUS-WS               PIC XX.                                  
020000     88    SEGMENT-FINNS                    VALUE '  '.                   
020100     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
020200     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
020300     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
020400     SKIP3                                                                
020500   03    GODK-STATUSKODER.                                                
020600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
020700     SKIP3                                                                
020800 01    SSA1                      PIC X(64).                               
020900     EJECT                                                                
021000*                            IMS FUNKTIONSKODER                           
021100*01    -COPY W0003                                                        
021200     EJECT                                                                
021300*                            DLI INPUT-OUTPUT AREA                        
021400 01    DLI-IO-AREA.                                                       
021500   03    IO-AREA                 PIC X(096)  VALUE SPACE.                 
021600     SKIP3                                                                
021700*  03    WLEMBB01 -COPY WDK501 -PRE KOLLI-  -RED IO-AREA.                 
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000*01    -COPY W0009     -PRE MSG-                                          
022100     EJECT                                                                
022200*01    -COPY W0008     -PRE USEA-                                         
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01    -COPY W0008     -PRE EMBB-                                         
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800 PROCEDURE DIVISION   USING  MSG-PCB USEA-PCB EMBB-PCB.                   
022900     ENTRY 'DLITCBL'  USING  MSG-PCB USEA-PCB EMBB-PCB.                   
023000                                                                          
023100     PERFORM IMS-GET-MSG                                                  
023200     IF SEGMENT-FINNS                                                     
023300       PERFORM A-INIT-SPARA-INPUT                                         
023400       IF MFS-UPDATE                                                      
023500         PERFORM B-KOLLA-INPUT                                            
023600         IF INPUT-OK = JA                                                 
023700           PERFORM IMS-GET-EMBB-UPPDATE                                   
023800           EVALUATE TRUE                                                  
023900           WHEN MID-KDANDR = NYUPPLAEGG                                   
024000             PERFORM C-NYUPPLAEGG                                         
024100           WHEN MID-KDANDR = BORTTAG OR BORTTAG-ENG                       
024200             PERFORM D-BORTTAG                                            
024300           WHEN OTHER                                                     
024400             PERFORM E-AENDRA                                             
024500           END-EVALUATE                                                   
024600         ELSE                                                             
024700           MOVE FEL-5(SPRAK-INDX) TO MOD-MESSAGE-RAD1                     
024800         END-IF                                                           
024900       ELSE                                                               
025000         IF MID-KDKOLLI = ALL '+' AND MID-KDEMBTYP = ALL '+' AND          
025100            MID-DIKOLLIL = ALL '+' AND MID-DIKOLLIB = ALL '+' AND         
025200            MID-DIKOLLIH = ALL '+' AND MID-VKTARA   = ALL '+' AND         
025300            MID-KDKOLLID = ALL '+' AND MID-KVPALLAR = ALL '+' AND         
025400            MID-KVRAM    = ALL '+' AND MID-KVLOCK   = ALL '+' AND         
025400            MID-KVEMBSPA = ALL '+' AND                                    
025500            MID-KVINTPALL = ALL '+' AND MID-KDANDR  = ALL '+'             
025600           EVALUATE TRUE                                                  
025700           WHEN MFS-IDPFK = W-NYSIDA                                      
025800             MOVE MID-KDKOLLI-SPAR TO W-KOLLIKOD                          
025900           WHEN MFS-IDPFK = W-BORJAN                                      
026000             MOVE SPACE TO W-KOLLIKOD                                     
026100           WHEN OTHER                                                     
026200             MOVE KOLLIKOD-WS TO W-KOLLIKOD                               
026300           END-EVALUATE                                                   
026400         ELSE                                                             
026500           MOVE FEL-2(SPRAK-INDX) TO MOD-MESSAGE-RAD1                     
026600           PERFORM MFS-FAELT-RAETT                                        
026700         END-IF                                                           
026800         PERFORM IMS-GET-EMBB                                             
026900         PERFORM H-FLYTTA-TILL-MOD-FRAGA                                  
027000       END-IF                                                             
027100       IF NOT WS-GODKAEND-BILD                                            
027200           PERFORM I-RENSA-NYCKLAR                                        
027300       END-IF                                                             
027400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027500       PERFORM IMS-INSERT-MSG                                             
027600     END-IF                                                               
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT-SPARA-INPUT SECTION.                                              
028200                                                                          
028300     IF MSG-DUBBLA-TRANSKODER                                             
028400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I11101                 
028500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
028600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
028700       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
028800       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
028900     ELSE                                                                 
029000       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I11101                   
029100       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
029200       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
029300     END-IF                                                               
029400                                                                          
029500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029600     MOVE '001'             TO MSGI-KDCALL                                
029700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029800     MOVE '4111'            TO MSGI-IDTRANS                               
029900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030100     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
030101                                                                          
030110     IF  MSGI-IDLAND-SPR NOT = 'GB'                                       
030120       MOVE +1 TO SPRAK-INDX                                              
030130     ELSE                                                                 
030140       MOVE +2 TO SPRAK-INDX                                              
030150     END-IF                                                               
030200                                                                          
030300     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
030400     IF MID-KDKOLLI-IN = ALL '+'                                          
030500       MOVE MID-KDKOLLI-UT TO KOLLIKOD-WS                                 
030600     ELSE                                                                 
030700       MOVE MID-KDKOLLI-IN TO KOLLIKOD-WS                                 
030800       MOVE SPACE TO MFS-IDPFK                                            
030900     END-IF                                                               
031000     MOVE LOW-VALUE TO MSG-AREA                                           
031100     MOVE 'W4O111N1' TO MFS-IDMOD                                         
031200     MOVE '4111' TO MOD-IDTRANS                                           
031300     MOVE KOLLIKOD-WS TO MOD-KDKOLLI-UT                                   
031400     MOVE MFS-RENSA-FAELT TO MOD-KDKOLLI-IN                               
031500                             MOD-MESSAGE-RAD1                             
031600                             MOD-MESSAGE-RAD23                            
032200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOLLI-INP                            
032300                               MOD-KDEMBTYP-INP                           
032400                               MOD-DIKOLLIL-INP                           
032500                               MOD-DIKOLLIB-INP                           
032600                               MOD-DIKOLLIH-INP                           
032700                               MOD-VKTARA-INP                             
032800                               MOD-KDKOLLID-INP                           
032900                               MOD-KVPALLAR-INP                           
033000                               MOD-KVRAM-INP                              
033000                               MOD-KVEMBSPA-INP                           
033100                               MOD-KVLOCK-INP                             
033200                               MOD-KVINTPALL-INP                          
033300                               MOD-KDANDR-INP                             
033400     MOVE +1 TO INDX                                                      
033500     PERFORM UNTIL INDX NOT < MAX-LINE + 1                                
033600       MOVE MFS-ROER-EJ-FAELT TO MOD-RAD(INDX)                            
033700       ADD +1 TO INDX                                                     
033800     END-PERFORM                                                          
033900     .                                                                    
034000     EJECT                                                                
034100 B-KOLLA-INPUT SECTION.                                                   
034200                                                                          
034300     MOVE JA TO INPUT-OK                                                  
034400     IF MID-KDANDR = NYUPPLAEGG OR BORTTAG OR BORTTAG-ENG                 
034410                     OR AENDRA OR AENDRA-ENG                              
034500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDANDR-INP-ATTR                   
034600       IF MID-KDKOLLI = ALL '+'                                           
034700         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-INP-ATTR                  
034800         MOVE NEJ TO INPUT-OK                                             
034900       ELSE                                                               
035000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOLLI-INP-ATTR                
035100         MOVE MID-KDKOLLI TO W-KOLLIKOD                                   
035200       END-IF                                                             
035210                                                                          
035300       IF MID-KDEMBTYP NUMERIC                                            
035400         IF MID-KDEMBTYP = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8           
035410*OBS! OM FLER -KVEMBTYP TILLÅTS MÅSTE DETTA ÄVEN TAS OM HAND VID          
035420*UTSKRIFT AV FRAKTSEDEL.                                                  
035500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDEMBTYP-INP-ATTR              
035600         ELSE                                                             
035700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-INP-ATTR                
035800           MOVE NEJ TO INPUT-OK                                           
035900         END-IF                                                           
036000       ELSE                                                               
036100         IF MID-KDEMBTYP = ALL '+'                                        
036200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDEMBTYP-INP-ATTR              
036300         ELSE                                                             
036400           MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-INP-ATTR                
036500           MOVE NEJ TO INPUT-OK                                           
036600         END-IF                                                           
036700       END-IF                                                             
036710                                                                          
036800       IF MID-DIKOLLIL NUMERIC OR MID-DIKOLLIL = ALL '+'                  
036900         MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKOLLIL-INP-ATTR                
037000       ELSE                                                               
037100         MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-INP-ATTR                  
037200         MOVE NEJ TO INPUT-OK                                             
037300       END-IF                                                             
037310                                                                          
037400       IF MID-DIKOLLIB NUMERIC OR MID-DIKOLLIB = ALL '+'                  
037500         MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKOLLIB-INP-ATTR                
037600       ELSE                                                               
037700         MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-INP-ATTR                  
037800         MOVE NEJ TO INPUT-OK                                             
037900       END-IF                                                             
037910                                                                          
038000       IF MID-DIKOLLIH NUMERIC OR MID-DIKOLLIH = ALL '+'                  
038100         MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKOLLIH-INP-ATTR                
038200       ELSE                                                               
038300         MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-INP-ATTR                  
038400         MOVE NEJ TO INPUT-OK                                             
038500       END-IF                                                             
038510                                                                          
038600       IF MID-VKTARA = ALL '+'                                            
038700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-VKTARA-INP-ATTR                 
038800       ELSE                                                               
038900         MOVE MID-VKTARA TO DEC-IDFRIDATA                                 
039000         MOVE 4 TO DEC-KVHELTAL                                           
039100         MOVE 1 TO DEC-KVDECIMAL                                          
039200         CALL WDECEDIT USING DEC-WDECAREA                                 
039300         END-CALL                                                         
039310                                                                          
039400         IF DEC-KDSVAR-OK                                                 
039500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VKTARA-INP-ATTR               
039600         ELSE                                                             
039700           MOVE MFS-ALFA-FAELT-FEL TO MOD-VKTARA-INP-ATTR                 
039800           MOVE NEJ TO INPUT-OK                                           
039900         END-IF                                                           
040000       END-IF                                                             
040010*L=KOLLITS LÄNGD ÄR DJUPET                                                
040020*B=KOLLITS BREDD ÄR DJUPET                                                
040100       IF MID-KDKOLLID = 'B' OR 'L' OR MID-KDKOLLID = ALL '+'             
040200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOLLID-INP-ATTR               
040300       ELSE                                                               
040400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLID-INP-ATTR                 
040500         MOVE NEJ TO INPUT-OK                                             
040600       END-IF                                                             
040610                                                                          
040700       IF MID-KVPALLAR NUMERIC OR MID-KVPALLAR = ALL '+'                  
040800         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPALLAR-INP-ATTR                
040900       ELSE                                                               
041000         MOVE MFS-NUM-FAELT-FEL TO MOD-KVPALLAR-INP-ATTR                  
041100         MOVE NEJ TO INPUT-OK                                             
041200       END-IF                                                             
041210                                                                          
041300       IF MID-KVRAM NUMERIC OR MID-KVRAM = ALL '+'                        
041400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVRAM-INP-ATTR                   
041500       ELSE                                                               
041600         MOVE MFS-NUM-FAELT-FEL TO MOD-KVRAM-INP-ATTR                     
041700         MOVE NEJ TO INPUT-OK                                             
041800       END-IF                                                             
041810                                                                          
041300       IF MID-KVEMBSPA NUMERIC OR MID-KVEMBSPA = ALL '+'                  
041400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVEMBSPA-INP-ATTR                
041500       ELSE                                                               
041600         MOVE MFS-NUM-FAELT-FEL TO MOD-KVEMBSPA-INP-ATTR                  
041700         MOVE NEJ TO INPUT-OK                                             
041800       END-IF                                                             
041810                                                                          
041900       IF MID-KVLOCK NUMERIC OR MID-KVLOCK = ALL '+'                      
042000         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVLOCK-INP-ATTR                  
042100       ELSE                                                               
042200         MOVE MFS-NUM-FAELT-FEL TO MOD-KVLOCK-INP-ATTR                    
042300         MOVE NEJ TO INPUT-OK                                             
042400       END-IF                                                             
042410                                                                          
042500       IF MID-KVINTPALL NUMERIC OR MID-KVINTPALL = ALL '+'                
042600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINTPALL-INP-ATTR               
042700       ELSE                                                               
042800         MOVE MFS-NUM-FAELT-FEL TO MOD-KVINTPALL-INP-ATTR                 
042900         MOVE NEJ TO INPUT-OK                                             
043000       END-IF                                                             
043100     ELSE                                                                 
043200       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANDR-INP-ATTR                     
043300       MOVE NEJ TO INPUT-OK                                               
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 C-NYUPPLAEGG SECTION.                                                    
043800                                                                          
043900     IF SEGMENT-SAKNAS                                                    
044000       MOVE MID-KDKOLLI TO KOLLI-EMB-KDKOLLI                              
044100                           WS-EMB-KDKOLLI                                 
044200       IF MID-KDEMBTYP = ALL '+'                                          
044300         MOVE ZERO TO KOLLI-EMB-KDEMBTYP                                  
044400       ELSE                                                               
044500         MOVE MID-KDEMBTYP TO KOLLI-EMB-KDEMBTYP                          
044600       END-IF                                                             
044700                                                                          
044800       IF MID-DIKOLLIL = ALL '+'                                          
044900         MOVE ZERO TO KOLLI-EMB-DIKOLLIL                                  
045000       ELSE                                                               
045100         IF US-MEASUREMENT                                                
045200           MOVE MID-DIKOLLIL TO WS-DIKOLLIL                               
045300           COMPUTE KOLLI-EMB-DIKOLLIL =                                   
045400                   WS-DIKOLLIL * CONV-IN-TO-CM  END-COMPUTE               
045500         ELSE                                                             
045600           MOVE MID-DIKOLLIL TO KOLLI-EMB-DIKOLLIL                        
045700         END-IF                                                           
045800       END-IF                                                             
045900                                                                          
046000       IF MID-DIKOLLIB = ALL '+'                                          
046100         MOVE ZERO TO KOLLI-EMB-DIKOLLIB                                  
046200       ELSE                                                               
046300         IF US-MEASUREMENT                                                
046400           MOVE MID-DIKOLLIB TO WS-DIKOLLIB                               
046500           COMPUTE KOLLI-EMB-DIKOLLIB =                                   
046600                   WS-DIKOLLIB * CONV-IN-TO-CM  END-COMPUTE               
046700         ELSE                                                             
046800           MOVE MID-DIKOLLIB TO KOLLI-EMB-DIKOLLIB                        
046900         END-IF                                                           
047000       END-IF                                                             
047100                                                                          
047200       IF MID-DIKOLLIH = ALL '+'                                          
047300         MOVE ZERO TO KOLLI-EMB-DIKOLLIH                                  
047400       ELSE                                                               
047500         IF US-MEASUREMENT                                                
047600           MOVE MID-DIKOLLIH TO WS-DIKOLLIH                               
047700           COMPUTE KOLLI-EMB-DIKOLLIH =                                   
047800                   WS-DIKOLLIH * CONV-IN-TO-CM  END-COMPUTE               
047900         ELSE                                                             
048000           MOVE MID-DIKOLLIH TO KOLLI-EMB-DIKOLLIH                        
048100         END-IF                                                           
048200       END-IF                                                             
048300                                                                          
048400       IF MID-VKTARA  = ALL '+'                                           
048500         MOVE ZERO TO KOLLI-EMB-VKTARA                                    
048600       ELSE                                                               
048700         IF US-MEASUREMENT                                                
048800           COMPUTE KOLLI-EMB-VKTARA =                                     
048900                   DEC-IDEDITDATA * CONV-LB-TO-KG  END-COMPUTE            
049000         ELSE                                                             
049100           MOVE DEC-IDEDITDATA TO KOLLI-EMB-VKTARA                        
049200         END-IF                                                           
049300       END-IF                                                             
049400                                                                          
049500       IF MID-KDKOLLID = ALL '+'                                          
049600         MOVE SPACE TO KOLLI-EMB-KDKOLLID                                 
049700       ELSE                                                               
049800         MOVE MID-KDKOLLID TO KOLLI-EMB-KDKOLLID                          
049900       END-IF                                                             
050000       IF MID-KVPALLAR = ALL '+'                                          
050100         MOVE ZERO TO KOLLI-EMB-KVPALL                                    
050200       ELSE                                                               
050300         MOVE MID-KVPALLAR TO KOLLI-EMB-KVPALL                            
050400       END-IF                                                             
050500       IF MID-KVRAM = ALL '+'                                             
050600         MOVE ZERO TO KOLLI-EMB-KVRAM                                     
050700       ELSE                                                               
050800         MOVE MID-KVRAM TO KOLLI-EMB-KVRAM                                
050900       END-IF                                                             
050500       IF MID-KVEMBSPA = ALL '+'                                          
050600         MOVE ZERO TO KOLLI-EMB-KVEMBSPA                                  
050700       ELSE                                                               
050800         MOVE MID-KVEMBSPA TO KOLLI-EMB-KVEMBSPA                          
050900       END-IF                                                             
051000       IF MID-KVLOCK = ALL '+'                                            
051100         MOVE ZERO TO KOLLI-EMB-KVLOCK                                    
051200       ELSE                                                               
051300         MOVE MID-KVLOCK TO KOLLI-EMB-KVLOCK                              
051400       END-IF                                                             
051500       IF MID-KVINTPALL = ALL '+'                                         
051600         MOVE ZERO TO KOLLI-EMB-KVINTPALL                                 
051700       ELSE                                                               
051800         MOVE MID-KVINTPALL TO KOLLI-EMB-KVINTPALL                        
051900       END-IF                                                             
052000       IF WS-EMB-KDKOLLI-PRE = 'F'                                        
052100       OR WS-EMB-KDKOLLI-PRE = 'G'                                        
052200       OR WS-EMB-KDKOLLI-PRE = 'H'                                        
052300       OR WS-EMB-KDKOLLI-PRE = 'L'                                        
052400       OR WS-EMB-KDKOLLI-PRE = 'K'                                        
052500         MOVE WS-EMB-KDKOLLI-PRE TO KOLLI-EMB-EMBPROF                     
052600       ELSE                                                               
052700         MOVE SPACE TO KOLLI-EMB-EMBPROF                                  
052800       END-IF                                                             
052900       PERFORM IMS-INSERT-EMBB                                            
053000       PERFORM F-FLYTTA-TILL-MOD-UPPDATERING                              
053100       MOVE MED-1(SPRAK-INDX) TO MOD-MESSAGE-RAD23                        
053200     ELSE                                                                 
053300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-INP-ATTR                    
053400       MOVE FEL-4(SPRAK-INDX) TO MOD-MESSAGE-RAD1                         
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 D-BORTTAG SECTION.                                                       
053900                                                                          
054000     IF SEGMENT-FINNS                                                     
054100       PERFORM IMS-DELETE-EMBB                                            
054200       PERFORM F-FLYTTA-TILL-MOD-UPPDATERING                              
054300       MOVE MED-2(SPRAK-INDX) TO MOD-MESSAGE-RAD23                        
054400     ELSE                                                                 
054500       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-INP-ATTR                    
054600       MOVE FEL-1(SPRAK-INDX) TO MOD-MESSAGE-RAD1                         
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000 E-AENDRA SECTION.                                                        
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300       IF MID-KDEMBTYP NOT = ALL '+'                                      
055400         MOVE MID-KDEMBTYP TO KOLLI-EMB-KDEMBTYP                          
055500       END-IF                                                             
055600       IF MID-DIKOLLIL NOT = ALL '+'                                      
055700         IF US-MEASUREMENT                                                
055800           MOVE MID-DIKOLLIL TO WS-DIKOLLIL                               
055900           COMPUTE KOLLI-EMB-DIKOLLIL =                                   
056000                   WS-DIKOLLIL * CONV-IN-TO-CM  END-COMPUTE               
056100         ELSE                                                             
056200           MOVE MID-DIKOLLIL TO KOLLI-EMB-DIKOLLIL                        
056300         END-IF                                                           
056400       END-IF                                                             
056500                                                                          
056600       IF MID-DIKOLLIB NOT = ALL '+'                                      
056700         IF US-MEASUREMENT                                                
056800           MOVE MID-DIKOLLIB TO WS-DIKOLLIB                               
056900           COMPUTE KOLLI-EMB-DIKOLLIB =                                   
057000                   WS-DIKOLLIB * CONV-IN-TO-CM  END-COMPUTE               
057100         ELSE                                                             
057200           MOVE MID-DIKOLLIB TO KOLLI-EMB-DIKOLLIB                        
057300         END-IF                                                           
057400       END-IF                                                             
057500                                                                          
057600       IF MID-DIKOLLIH NOT = ALL '+'                                      
057700         IF US-MEASUREMENT                                                
057800           MOVE MID-DIKOLLIH TO WS-DIKOLLIH                               
057900           COMPUTE KOLLI-EMB-DIKOLLIH =                                   
058000                   WS-DIKOLLIH * CONV-IN-TO-CM  END-COMPUTE               
058100         ELSE                                                             
058200           MOVE MID-DIKOLLIH TO KOLLI-EMB-DIKOLLIH                        
058300         END-IF                                                           
058400       END-IF                                                             
058500                                                                          
058600       IF MID-VKTARA NOT = ALL '+'                                        
058700         IF US-MEASUREMENT                                                
058800           COMPUTE KOLLI-EMB-VKTARA =                                     
058900                   DEC-IDEDITDATA * CONV-LB-TO-KG  END-COMPUTE            
059000         ELSE                                                             
059100           MOVE DEC-IDEDITDATA TO KOLLI-EMB-VKTARA                        
059200         END-IF                                                           
059300       END-IF                                                             
059400                                                                          
059500       IF MID-KDKOLLID NOT = ALL '+'                                      
059600         MOVE MID-KDKOLLID TO KOLLI-EMB-KDKOLLID                          
059700       END-IF                                                             
059800       IF MID-KVPALLAR NOT = ALL '+'                                      
059900         MOVE MID-KVPALLAR TO KOLLI-EMB-KVPALL                            
060000       END-IF                                                             
060100       IF MID-KVRAM NOT = ALL '+'                                         
060200         MOVE MID-KVRAM TO KOLLI-EMB-KVRAM                                
060300       END-IF                                                             
060100       IF MID-KVEMBSPA NOT = ALL '+'                                      
060200         MOVE MID-KVEMBSPA TO KOLLI-EMB-KVEMBSPA                          
060300       END-IF                                                             
060400       IF MID-KVLOCK NOT = ALL '+'                                        
060500         MOVE MID-KVLOCK TO KOLLI-EMB-KVLOCK                              
060600       END-IF                                                             
060700       IF MID-KVINTPALL NOT = ALL '+'                                     
060800         MOVE MID-KVINTPALL TO KOLLI-EMB-KVINTPALL                        
060900       END-IF                                                             
061000       PERFORM IMS-REPLACE-EMBB                                           
061100       PERFORM F-FLYTTA-TILL-MOD-UPPDATERING                              
061200       MOVE MED-3(SPRAK-INDX) TO MOD-MESSAGE-RAD23                        
061300     ELSE                                                                 
061400       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-INP-ATTR                    
061500       MOVE FEL-1(SPRAK-INDX) TO MOD-MESSAGE-RAD1                         
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 F-FLYTTA-TILL-MOD-UPPDATERING SECTION.                                   
062000                                                                          
062100     MOVE +1 TO INDX                                                      
062200     PERFORM UNTIL INDX NOT < MAX-LINE + 1                                
062300       IF INDX = 1                                                        
062400         MOVE KOLLI-EMB-KDKOLLI   TO MOD-KDKOLLI(INDX)                    
062500         MOVE KOLLI-EMB-KDEMBTYP  TO MOD-KDEMBTYP(INDX)                   
062600*                                                                         
062700         MOVE KOLLI-EMB-DIKOLLIL  TO WS-DIKOLLIL                          
062800         MOVE KOLLI-EMB-DIKOLLIB  TO WS-DIKOLLIB                          
062900         MOVE KOLLI-EMB-DIKOLLIH  TO WS-DIKOLLIH                          
063000         MOVE KOLLI-EMB-VKTARA    TO WS-VKTARA                            
063100         PERFORM S01-EV-CONVERT-TO-US-MEASURE                             
063200         MOVE WS-DIKOLLIL         TO MOD-DIKOLLIL(INDX)                   
063300         MOVE WS-DIKOLLIB         TO MOD-DIKOLLIB(INDX)                   
063400         MOVE WS-DIKOLLIH         TO MOD-DIKOLLIH(INDX)                   
063500         MOVE WS-VKTARA           TO MOD-VKTARA(INDX)                     
063600*                                                                         
063700         MOVE KOLLI-EMB-KDKOLLID  TO MOD-KDKOLLID(INDX)                   
063800         MOVE KOLLI-EMB-KVPALL    TO MOD-KVPALLAR(INDX)                   
063900         MOVE KOLLI-EMB-KVRAM     TO MOD-KVRAM(INDX)                      
063900         MOVE KOLLI-EMB-KVEMBSPA  TO MOD-KVEMBSPA(INDX)                   
064000         MOVE KOLLI-EMB-KVLOCK    TO MOD-KVLOCK(INDX)                     
064100         MOVE KOLLI-EMB-KVINTPALL TO MOD-KVINTPALL(INDX)                  
064200         MOVE KOLLI-EMB-EMBPROF   TO MOD-EMBPROF(INDX)                    
064300       ELSE                                                               
064400         MOVE MFS-RENSA-FAELT     TO MOD-RAD(INDX)                        
064500       END-IF                                                             
064600       ADD +1 TO INDX                                                     
064700     END-PERFORM                                                          
064800     MOVE MFS-FORMATETS-ATTR TO MOD-KDKOLLI-INP-ATTR                      
064900                                MOD-KDEMBTYP-INP-ATTR                     
065000                                MOD-DIKOLLIL-INP-ATTR                     
065100                                MOD-DIKOLLIB-INP-ATTR                     
065200                                MOD-DIKOLLIH-INP-ATTR                     
065300                                MOD-VKTARA-INP-ATTR                       
065400                                MOD-KDKOLLID-INP-ATTR                     
065500                                MOD-KVPALLAR-INP-ATTR                     
065600                                MOD-KVRAM-INP-ATTR                        
065600                                MOD-KVEMBSPA-INP-ATTR                     
065700                                MOD-KVLOCK-INP-ATTR                       
065800                                MOD-KVINTPALL-INP-ATTR                    
065900                                MOD-KDANDR-INP-ATTR                       
066000     MOVE MFS-RENSA-FAELT TO MOD-KDKOLLI-INP                              
066100                             MOD-KDEMBTYP-INP                             
066200                             MOD-DIKOLLIL-INP                             
066300                             MOD-DIKOLLIB-INP                             
066400                             MOD-DIKOLLIH-INP                             
066500                             MOD-VKTARA-INP                               
066600                             MOD-KDKOLLID-INP                             
066700                             MOD-KVPALLAR-INP                             
066800                             MOD-KVRAM-INP                                
066800                             MOD-KVEMBSPA-INP                             
066900                             MOD-KVLOCK-INP                               
067000                             MOD-KVINTPALL-INP                            
067100                             MOD-KDANDR-INP                               
067200     .                                                                    
067300     EJECT                                                                
067400 H-FLYTTA-TILL-MOD-FRAGA SECTION.                                         
067500                                                                          
067600     IF SEGMENT-FINNS                                                     
067700       MOVE +1 TO INDX                                                    
067800       PERFORM UNTIL SEGMENT-SAKNAS OR SLUT-PA-BASEN OR                   
067900               (INDX NOT < MAX-LINE + 1)                                  
068000         MOVE KOLLI-EMB-KDKOLLI   TO MOD-KDKOLLI(INDX)                    
068100         MOVE KOLLI-EMB-KDEMBTYP  TO MOD-KDEMBTYP(INDX)                   
068200*                                                                         
068300         MOVE KOLLI-EMB-DIKOLLIL  TO WS-DIKOLLIL                          
068400         MOVE KOLLI-EMB-DIKOLLIB  TO WS-DIKOLLIB                          
068500         MOVE KOLLI-EMB-DIKOLLIH  TO WS-DIKOLLIH                          
068600         MOVE KOLLI-EMB-VKTARA    TO WS-VKTARA                            
068700         PERFORM S01-EV-CONVERT-TO-US-MEASURE                             
068800         MOVE WS-DIKOLLIL         TO MOD-DIKOLLIL(INDX)                   
068900         MOVE WS-DIKOLLIB         TO MOD-DIKOLLIB(INDX)                   
069000         MOVE WS-DIKOLLIH         TO MOD-DIKOLLIH(INDX)                   
069100         MOVE WS-VKTARA           TO MOD-VKTARA(INDX)                     
069200*                                                                         
069300         MOVE KOLLI-EMB-KDKOLLID  TO MOD-KDKOLLID(INDX)                   
069400         MOVE KOLLI-EMB-KVPALL    TO MOD-KVPALLAR(INDX)                   
069500         MOVE KOLLI-EMB-KVRAM     TO MOD-KVRAM(INDX)                      
069500         MOVE KOLLI-EMB-KVEMBSPA  TO MOD-KVEMBSPA(INDX)                   
069600         MOVE KOLLI-EMB-KVLOCK    TO MOD-KVLOCK(INDX)                     
069700         MOVE KOLLI-EMB-KVINTPALL TO MOD-KVINTPALL(INDX)                  
069800         MOVE KOLLI-EMB-EMBPROF   TO MOD-EMBPROF(INDX)                    
069900         ADD +1 TO INDX                                                   
070000         PERFORM IMS-GET-EMBB-NEXT                                        
070100       END-PERFORM                                                        
070200       IF SLUT-PA-BASEN                                                   
070300         MOVE SPACE TO MOD-KDKOLLI-SPAR                                   
070400         PERFORM UNTIL INDX NOT < MAX-LINE + 1                            
070500           MOVE MFS-RENSA-FAELT TO MOD-RAD(INDX)                          
070600           ADD +1 TO INDX                                                 
070700         END-PERFORM                                                      
070800       ELSE                                                               
070900         MOVE KOLLI-EMB-KDKOLLI TO MOD-KDKOLLI-SPAR                       
071000       END-IF                                                             
071100     ELSE                                                                 
071200       MOVE FEL-1(SPRAK-INDX) TO MOD-MESSAGE-RAD1                         
071300     END-IF                                                               
071400     .                                                                    
071500     SKIP2                                                                
071600 I-RENSA-NYCKLAR SECTION.                                                 
071700     MOVE MFS-RENSA-FAELT            TO MOD-KDKOLLI-UT                    
071800     .                                                                    
071900     SKIP2                                                                
072000 S01-EV-CONVERT-TO-US-MEASURE        SECTION.                             
072100                                                                          
072200     IF US-MEASUREMENT                                                    
072300       COMPUTE WS-DIKOLLIL = WS-DIKOLLIL * CONV-CM-TO-IN                  
072400       END-COMPUTE                                                        
072500                                                                          
072600       COMPUTE WS-DIKOLLIB = WS-DIKOLLIB * CONV-CM-TO-IN                  
072700       END-COMPUTE                                                        
072800                                                                          
072900       COMPUTE WS-DIKOLLIH = WS-DIKOLLIH * CONV-CM-TO-IN                  
073000       END-COMPUTE                                                        
073100                                                                          
073200       COMPUTE WS-VKTARA   = WS-VKTARA   * CONV-KG-TO-LB                  
073300       END-COMPUTE                                                        
073400     END-IF                                                               
073500     .                                                                    
073600     SKIP2                                                                
073700*                                                                         
073800 MFS-FAELT-RAETT SECTION.                                                 
073900                                                                          
074000     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDKOLLI-INP-ATTR                    
074100                                  MOD-VKTARA-INP-ATTR                     
074200                                  MOD-KDKOLLID-INP-ATTR                   
074300                                  MOD-KDANDR-INP-ATTR                     
074400     MOVE MFS-NUM-FAELT-RAETT TO  MOD-KDEMBTYP-INP-ATTR                   
074500                                  MOD-DIKOLLIL-INP-ATTR                   
074600                                  MOD-DIKOLLIB-INP-ATTR                   
074700                                  MOD-DIKOLLIH-INP-ATTR                   
074800                                  MOD-KVPALLAR-INP-ATTR                   
074900                                  MOD-KVRAM-INP-ATTR                      
074900                                  MOD-KVEMBSPA-INP-ATTR                   
075000                                  MOD-KVLOCK-INP-ATTR                     
075100                                  MOD-KVINTPALL-INP-ATTR                  
075200     EJECT                                                                
075300* IMS SEKTIONER                                                           
075400     SKIP3                                                                
075500     .                                                                    
075600 IMS-GET-MSG SECTION.                                                     
075700                                                                          
075800     MOVE '  QC' TO GODK-STATUSKODER                                      
075900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
076000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076100     PERFORM IMS-STATUSKONTROLL                                           
076200     SKIP3                                                                
076300     .                                                                    
076400 IMS-INSERT-MSG SECTION.                                                  
076500                                                                          
076600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
076700         MOVE '0' TO MFS-KDHUVOMR                                         
076800     END-IF                                                               
076900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077000     MOVE SPACE TO GODK-STATUSKODER                                       
077100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
077200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     SKIP3                                                                
077500     .                                                                    
077600 IMS-GET-EMBB SECTION.                                                    
077700                                                                          
077800     STRING 'WLEMBB01(KDKOLLI >=' W-KOLLIKOD-X ')'                        
077900            DELIMITED BY SIZE INTO SSA1                                   
078000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
078100     CALL CBLTDLI USING GN EMBB-PCB DLI-IO-AREA SSA1                      
078200     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
078300     PERFORM IMS-STATUSKONTROLL                                           
078400     .                                                                    
078500     EJECT                                                                
078600 IMS-GET-EMBB-NEXT SECTION.                                               
078700                                                                          
078800     STRING 'WLEMBB01 ' DELIMITED BY SIZE INTO SSA1                       
078900     MOVE '  GB' TO GODK-STATUSKODER                                      
079000     CALL CBLTDLI USING GN EMBB-PCB DLI-IO-AREA SSA1                      
079100     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUSKONTROLL                                           
079300     SKIP3                                                                
079400     .                                                                    
079500 IMS-GET-EMBB-UPPDATE SECTION.                                            
079600                                                                          
079700     STRING 'WLEMBB01(KDKOLLI  =' W-KOLLIKOD-X ')'                        
079800            DELIMITED BY SIZE INTO SSA1                                   
079900     MOVE '  GE' TO GODK-STATUSKODER                                      
080000     CALL CBLTDLI USING GHU EMBB-PCB DLI-IO-AREA SSA1                     
080100     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     SKIP3                                                                
080400     .                                                                    
080500 IMS-REPLACE-EMBB SECTION.                                                
080600                                                                          
080700     MOVE '  ' TO GODK-STATUSKODER                                        
080800     CALL CBLTDLI USING REPL EMBB-PCB DLI-IO-AREA                         
080900     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUSKONTROLL                                           
081100     .                                                                    
081200     EJECT                                                                
081300 IMS-INSERT-EMBB SECTION.                                                 
081400                                                                          
081500     STRING 'WLEMBB01 ' DELIMITED BY SIZE INTO SSA1                       
081600     MOVE '  ' TO GODK-STATUSKODER                                        
081700     CALL CBLTDLI USING ISRT EMBB-PCB DLI-IO-AREA SSA1                    
081800     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     SKIP3                                                                
082100     .                                                                    
082200 IMS-DELETE-EMBB SECTION.                                                 
082300                                                                          
082400     MOVE '  ' TO GODK-STATUSKODER                                        
082500     CALL CBLTDLI USING DLET EMBB-PCB DLI-IO-AREA                         
082600     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
082700     PERFORM IMS-STATUSKONTROLL                                           
082800     SKIP3                                                                
082900     .                                                                    
083000 IMS-STATUSKONTROLL SECTION.                                              
083100                                                                          
083200     SET STATUS-IX TO 1                                                   
083300     SEARCH GODK-STATUS                                                   
083400       AT END                                                             
083500         CALL FELLOG                                                      
083600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
083700     END-SEARCH                                                           
083800     .                                                                    
