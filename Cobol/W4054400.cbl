000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4054400.                                                
000400 AUTHOR.         FRANK THORBURN.                                          
000500     DATE-WRITTEN.   JULY  87.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET LÄSER TRANSPORTREGISTRET (LOGISKT                     
001100*        WLXXDN OCH WLXXDM, FYSISKT WDG2) OCH VISAR                       
001200*        VILKEN TRANSPORT ETT DISTRIKT HÖR TILL OCH                       
001300*        TILL VILKEN ADRESS I FÄRDIGLAGRET TRANSPORTEN                    
001400*        STYRS.                                                           
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T544                                              
001800*        MID:         W4I54401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O51301                                            
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W4054400'                             
003100                                 PIC X(8).                                
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
003500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
003600 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
003700 77  WS-IDSKEPPN                 PIC X(7)    VALUE SPACE.                 
003800 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004000 77  RAD-MAX-PLUS-1              PIC S9(9)   VALUE +15  COMP SYNC.        
004100 77  SW-NYCKLAR-OK               PIC X       VALUE 'J'.                   
004200 77  SW-LAS-4402                 PIC X       VALUE 'J'.                   
004300 77  EGEN-BILD                   PIC X(4)    VALUE '4544'.                
004400 77  WS-IDTRANS                  PIC X(4).                                
004500     88  WS-GODKAEND-BILD                    VALUE '4541' '4542'          
004600                                                   '4543' '4544'.         
004700     EJECT                                                                
004800 01  GENERELLA-SUBPROGRAM.                                                
004900     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
005000     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
005100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005200     EJECT                                                                
005300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005400*01 -COPY WMSGINIT                                                        
005500     EJECT                                                                
005600                                                                          
005700 01  NYCKLAR-TILL-DLI.                                                    
005800                                                                          
005900     03  W-4401-WDGXKEY-X.                                                
006000         05  FILLER                PIC X(4)  VALUE '4401'.                
006100         05  FILLER                PIC X(26) VALUE LOW-VALUE.             
006200                                                                          
006300     03  W-4402-KY4402-MIN-X.                                             
006400         05  W-4402-IDDC-MIN       PIC X(2).                              
006500         05  W-4402-IDDISTR-MIN    PIC S9(5)   COMP-3.                    
006600         05  W-4402-IDKUNDNR-MIN   PIC S9(7)   COMP-3.                    
006700         05  W-4402-KDFRAKT-MIN    PIC S9(3)   COMP-3.                    
006800         05  W-4402-KDORDKLX-MIN   PIC X(1).                              
006900                                                                          
007000     03  W-4402-KY4402-MAX-X.                                             
007100         05  W-4402-IDDC-MAX       PIC X(2).                              
007200         05  W-4402-IDDISTR-MAX    PIC S9(5)   COMP-3.                    
007300         05  W-4402-IDKUNDNR-MAX   PIC S9(7)   COMP-3.                    
007400         05  FILLER                PIC X(3)    VALUE HIGH-VALUE.          
007500                                                                          
007600     03  W-4405-WDGXKEY-X.                                                
007700         05  FILLER                PIC X(4)    VALUE '4405'.              
007800         05  FILLER                PIC X(26)   VALUE LOW-VALUE.           
007900                                                                          
008000     03  W-4406-WDGXKEY-X.                                                
008100         05  W-4406-IDTRPTNR       PIC S9(3)   COMP-3.                    
008200         05  W-4406-IDDC           PIC X(2).                              
008300         05  FILLER                PIC X(6)    VALUE LOW-VALUE.           
008400                                                                          
008500     03  W-4408-WDGXKEY-MIN-X.                                            
008600         05  W-4408-ADCLGEO-MIN.                                          
008700             07  W-4408-IDDC-MIN      PIC X(2).                           
008800             07  W-4408-ADFLGEO-MIN   PIC X(3).                           
008900         05  W-4408-ADFLOMR-MIN       PIC S9(3)   COMP-3.                 
009000         05  W-4408-ADRUTNIV-MIN      PIC S9(3)   COMP-3.                 
009100         05  FILLER                   PIC X     VALUE LOW-VALUE.          
009200                                                                          
009300     03  W-IDDC-B6-X.                                                     
009400         05 W-IDDC-B6                  PIC X(2).                          
009500                                                                          
009600     EJECT                                                                
009700 01  MEDDELANDE.                                                          
009800                                                                          
009900   03  FEL901.                                                            
010000                                                                          
010100     05 FILLER                   PIC X(40)                                
010200          VALUE '901 FEL NYCKEL'.                                         
010300     05 FILLER                   PIC X(40)                                
010400          VALUE '901 WRONG KEY'.                                          
010500   03  FILLER REDEFINES FEL901.                                           
010600     05  FEL-901                 PIC X(40)   OCCURS 2.                    
010700                                                                          
010800   03  FEL796.                                                            
010900                                                                          
011000     05 FILLER                   PIC X(40)                                
011100          VALUE '796 TRANSPORT SAKNAS'.                                   
011200     05 FILLER                   PIC X(40)                                
011300          VALUE '796 TRANSPORT IS MISSING'.                               
011400   03  FILLER REDEFINES FEL796.                                           
011500     05  FEL-796                 PIC X(40)   OCCURS 2.                    
011600                                                                          
011700   03  MED1.                                                              
011800     05 FILLER                   PIC X(40)                                
011900          VALUE 'TRYCK PF8 FÖR MER INFORMATION'.                          
012000     05 FILLER                   PIC X(40)                                
012100          VALUE 'PRESS PF8 FOR MORE LINES'.                               
012200   03  FILLER REDEFINES MED1.                                             
012300     05  MED-1                   PIC X(40)   OCCURS 2.                    
012400     EJECT                                                                
012500******************************************************************        
012600*                                                                         
012700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
013000     SKIP3                                                                
013100*01  MID -COPY W4I54401                                                   
013200     EJECT                                                                
013300*01  -COPY WMSGAREA                                                       
013400     EJECT                                                                
013500*  03  MOD -COPY W4O54401           -RED MSG-AREA.                        
013600     EJECT                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900******************************************************************        
014000*                                                                         
014100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300 01  IMS-WS.                                                              
014400   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
014500     SKIP3                                                                
014600*                        **** STATUS-KOD FRÅN IMS                         
014700   03  STATUS-WS                 PIC XX.                                  
014800     88  SEGMENT-FINNS                       VALUE '  '.                  
014900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015000     SKIP3                                                                
015100   03  GODK-STATUSKODER.                                                  
015200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01    SSA1                      PIC X(96).                               
015500 01    SSA2                      PIC X(96).                               
015600     EJECT                                                                
015700*                            IMS FUNKTIONSKODER                           
015800*01    -COPY W0003                                                        
015900     EJECT                                                                
016000*                            DLI INPUT-OUTPUT AREA                        
016100 01  DLI-IO-AREA.                                                         
016200   03  IO-AREA                   PIC X(70)   VALUE SPACE.                 
016300     SKIP3                                                                
016400*  03  WLXXDM11  -COPY WDGX4402                -RED IO-AREA.              
016500     EJECT                                                                
016600*  03  WLXXDN21  -COPY WDGX4408  -PRE 4408-    -RED IO-AREA.              
016700                                                                          
016800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016900 01   DLI-IO-AREA-B601.                                                   
017000*     03  -COPY WDB601                                                    
017100                                                                          
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400*01  -COPY W0009     -PRE MSG-                                            
017500     EJECT                                                                
017600*01  -COPY W0008     -PRE USEA-                                           
017700     05  FILLER                  PIC X.                                   
017800     SKIP2                                                                
017900*01  -COPY W0008     -PRE XXDM-                                           
018000     05  FILLER                  PIC X.                                   
018100     SKIP2                                                                
018200*01  -COPY W0008     -PRE XXDN-                                           
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500*01  -COPY W0008     -PRE WDB6-                                           
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800 PROCEDURE DIVISION USING MSG-PCB USEA-PCB XXDM-PCB XXDN-PCB              
018900                          WDB6-PCB.                                       
019000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXDM-PCB XXDN-PCB             
019100                           WDB6-PCB.                                      
019200                                                                          
019300     PERFORM IMS-GET-MSG                                                  
019400                                                                          
019500     IF SEGMENT-FINNS                                                     
019600         PERFORM A-INIT                                                   
019700                                                                          
019800         PERFORM B-KOLLA-NYCKLAR                                          
019900                                                                          
020000         IF SW-NYCKLAR-OK = JA                                            
020100             IF MFS-IDPFK = '7'                                           
020200                 PERFORM C-LAS-FORSTA                                     
020300             ELSE                                                         
020400                 IF MFS-IDPFK = '8'                                       
020500                     PERFORM D-LAS-NASTA                                  
020600                 ELSE                                                     
020700                     PERFORM E-LAS-SAMMA                                  
020800                 END-IF                                                   
020900             END-IF                                                       
021000             PERFORM F-VISA-TRANSPORT                                     
021100         ELSE                                                             
021200             MOVE FEL-901 (SPRAK-IX) TO MOD-TEMFSFEL                      
021300             PERFORM MFS-RENSA                                            
021400         END-IF                                                           
021500         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O54401 + 4                    
021600         PERFORM IMS-INSERT-MSG                                           
021700     END-IF                                                               
021800                                                                          
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INIT SECTION.                                                          
022400                                                                          
022500     IF MSG-DUBBLA-TRANSKODER                                             
022600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I54401               
022700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
022800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
022900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
023000         MOVE MSG-IDPFK TO MFS-IDPFK                                      
023100     ELSE                                                                 
023200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I54401                
023300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
023400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
023500         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
023600     END-IF                                                               
023700                                                                          
023800     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
023900*--------------- FÖRBEREDELSE FÖR BLÄDDRING MED PF-8                      
024000*    MOVE '8'     TO MFS-IDPFK                                            
024100                                                                          
024200     MOVE LOW-VALUE          TO MSG-AREA                                  
024300     MOVE 'W4O544N1'         TO MFS-IDMOD                                 
024400     MOVE EGEN-BILD          TO MOD-IDTRANS                               
024500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
024600                             MOD-IDKUNDNR-IN                              
024700                             MOD-IDDC-IN                                  
024800                             MOD-IDPRODNR-IN                              
024900                             MOD-IDSKEPPN-IN                              
025000                             MOD-TEMFSFEL                                 
025100                             MOD-TEMFSINF                                 
025200                                                                          
025300                                                                          
025400     IF MFS-IDTRANS NOT = EGEN-BILD                                       
025500         MOVE SPACE TO MFS-KDTRTYP                                        
025600         MOVE '7' TO MFS-IDPFK                                            
025700     END-IF                                                               
025800                                                                          
025900     .                                                                    
026000     EJECT                                                                
026100 B-KOLLA-NYCKLAR SECTION.                                                 
026200                                                                          
026300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026400     MOVE '001'             TO MSGI-KDCALL                                
026500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026600     MOVE '4544'            TO MSGI-IDTRANS                               
026700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026800     IF MFS-IDTRANS          = EGEN-BILD                                  
026900        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
027000        MOVE MID-IDSKEPPN-IN TO MSGI-IDSKEPPN                             
027100        MOVE MID-IDPRODNR-IN TO MSGI-IDPRODNR                             
027200        IF MID-IDKUNDNR-IN   =  SPACE                                     
027300           MOVE ZERO         TO MSGI-IDKUNDNR                             
027400        ELSE                                                              
027500           MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                          
027600        END-IF                                                            
027700     END-IF                                                               
027800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027900                                                                          
028000     IF MSGI-IDLAND-SPR = 'GB'                                            
028100         MOVE +2 TO SPRAK-IX                                              
028200     ELSE                                                                 
028300         MOVE +1 TO SPRAK-IX                                              
028400     END-IF                                                               
028500                                                                          
028600     MOVE JA             TO SW-NYCKLAR-OK                                 
028700                                                                          
028800     IF MID-IDDISTR-IN   NOT = ALL '+'                                    
028900         MOVE '7' TO MFS-IDPFK                                            
029000     END-IF                                                               
029100     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
029200                                                                          
029300     IF MID-IDKUNDNR-IN  NOT = ALL '+'                                    
029400         MOVE '7' TO MFS-IDPFK                                            
029500     END-IF                                                               
029600     MOVE MSGI-IDKUNDNR  TO WS-IDKUNDNR                                   
029700                                                                          
029800     IF MID-IDDC-IN = ALL '+'                                             
029900         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
030000     ELSE                                                                 
030100         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
030200         MOVE '7' TO MFS-IDPFK                                            
030300     END-IF                                                               
030400     PERFORM IMS-GU-WDB601                                                
030500                                                                          
030600     MOVE MSGI-IDPRODNR       TO WS-IDPRODNR                              
030700     INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE                  
030800     MOVE WS-IDPRODNR         TO MOD-IDPRODNR-UT                          
030900                                                                          
031000     MOVE MSGI-IDSKEPPN       TO WS-IDSKEPPN                              
031100     INSPECT WS-IDSKEPPN REPLACING LEADING ZERO BY SPACE                  
031200     MOVE WS-IDSKEPPN          TO MOD-IDSKEPPN-UT                         
031300                                                                          
031400     MOVE LOW-VALUE            TO W-4402-KY4402-MIN-X                     
031500     MOVE HIGH-VALUE           TO W-4402-KY4402-MAX-X                     
031600                                  W-4402-IDDC-MIN                         
031700                                  W-4402-IDDC-MAX                         
031800                                  W-4406-IDDC                             
031900                                                                          
032000     IF WS-IDDISTR NUMERIC                                                
032100         MOVE WS-IDDISTR       TO W-4402-IDDISTR-MIN                      
032200                                  W-4402-IDDISTR-MAX                      
032300     ELSE                                                                 
032400         MOVE NEJ              TO SW-NYCKLAR-OK                           
032500     END-IF                                                               
032600                                                                          
032700     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
032800     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
032900                                                                          
033000     IF DCS-KDDC = SPACE                                                  
033100         MOVE MSGI-IDDC        TO W-IDDC-B6                               
033200         PERFORM IMS-GU-WDB601                                            
033300     END-IF                                                               
033400                                                                          
033500     MOVE DCS-IDDC             TO W-4402-IDDC-MIN                         
033600                                  W-4402-IDDC-MAX                         
033700                                  W-4406-IDDC                             
033800                                  MOD-IDDC-UT                             
033900                                                                          
034000     IF WS-IDKUNDNR NUMERIC                                               
034100         IF WS-IDKUNDNR            = ALL ZERO                             
034200            CONTINUE                                                      
034300         ELSE                                                             
034400             MOVE WS-IDKUNDNR      TO W-4402-IDKUNDNR-MIN                 
034500                                      W-4402-IDKUNDNR-MAX                 
034600         END-IF                                                           
034700     ELSE                                                                 
034800        MOVE NEJ              TO SW-NYCKLAR-OK                            
034900     END-IF                                                               
035000                                                                          
035100     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
035200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
035300                                                                          
035400     .                                                                    
035500     EJECT                                                                
035600 C-LAS-FORSTA SECTION.                                                    
035700                                                                          
035800*--- NÄR MAN KOMMER HIT, LÄSER MAN ALLTID FRÅN FÖRSTA 4402-SEGM           
035900     CONTINUE                                                             
036000     .                                                                    
036100     EJECT                                                                
036200 D-LAS-NASTA SECTION.                                                     
036300                                                                          
036400     EVALUATE TRUE                                                        
036500     WHEN MID-ADFLOMR-NEXT NUMERIC AND                                    
036600            MID-ADFLOMR-NEXT > ZERO                                       
036700         PERFORM DA-FLYTTA-4408-NEXTFALT                                  
036800         PERFORM S01-FLYTTA-4402-FIRSTFALT                                
036900         MOVE NEJ        TO SW-LAS-4402                                   
037000     WHEN MID-IDTRPTNR-NEXT NUMERIC AND                                   
037100            MID-IDTRPTNR-NEXT > ZERO                                      
037200         PERFORM DB-FLYTTA-4402-NEXTFALT                                  
037300         MOVE JA TO SW-LAS-4402                                           
037400     WHEN OTHER                                                           
037500         MOVE JA TO SW-LAS-4402                                           
037600     END-EVALUATE                                                         
037700                                                                          
037800     .                                                                    
037900 DA-FLYTTA-4408-NEXTFALT SECTION.                                         
038000                                                                          
038100     MOVE MID-IDTRPTNR-NEXT              TO W-4406-IDTRPTNR               
038200     MOVE DCS-IDDC                       TO W-4406-IDDC                   
038300                                            W-4408-IDDC-MIN               
038400     MOVE MID-ADFLGEO-NEXT               TO W-4408-ADFLGEO-MIN            
038500     MOVE MID-ADFLOMR-NEXT               TO W-4408-ADFLOMR-MIN            
038600     MOVE MID-ADRUTNIV-NEXT              TO W-4408-ADRUTNIV-MIN           
038700     SKIP2                                                                
038800     .                                                                    
038900 DB-FLYTTA-4402-NEXTFALT SECTION.                                         
039000                                                                          
039100     MOVE MID-IDKUNDNR-NEXT              TO W-4402-IDKUNDNR-MIN           
039200     MOVE MID-KDFRAKT-NEXT               TO W-4402-KDFRAKT-MIN            
039300     MOVE MID-KDORDKLX-NEXT              TO W-4402-KDORDKLX-MIN           
039400     .                                                                    
039500     EJECT                                                                
039600 E-LAS-SAMMA SECTION.                                                     
039700                                                                          
039800     EVALUATE TRUE                                                        
039900     WHEN MID-ADFLOMR-FIRST NUMERIC AND                                   
040000            MID-ADFLOMR-FIRST > ZERO                                      
040100         PERFORM EA-FLYTTA-4408-FIRSTFALT                                 
040200         PERFORM S01-FLYTTA-4402-FIRSTFALT                                
040300         MOVE NEJ        TO SW-LAS-4402                                   
040400     WHEN MID-IDTRPTNR-FIRST NUMERIC AND                                  
040500            MID-IDTRPTNR-FIRST > ZERO                                     
040600         PERFORM S01-FLYTTA-4402-FIRSTFALT                                
040700         MOVE JA TO SW-LAS-4402                                           
040800     WHEN OTHER                                                           
040900         MOVE JA TO SW-LAS-4402                                           
041000     END-EVALUATE                                                         
041100                                                                          
041200     .                                                                    
041300 EA-FLYTTA-4408-FIRSTFALT SECTION.                                        
041400                                                                          
041500     MOVE MID-IDTRPTNR-FIRST             TO W-4406-IDTRPTNR               
041600     MOVE DCS-IDDC                       TO W-4406-IDDC                   
041700                                            W-4408-IDDC-MIN               
041800     MOVE MID-ADFLGEO-FIRST              TO W-4408-ADFLGEO-MIN            
041900     MOVE MID-ADFLOMR-FIRST              TO W-4408-ADFLOMR-MIN            
042000     MOVE MID-ADRUTNIV-FIRST             TO W-4408-ADRUTNIV-MIN           
042100     SKIP2                                                                
042200     .                                                                    
042300     EJECT                                                                
042400 F-VISA-TRANSPORT SECTION.                                                
042500                                                                          
042600     PERFORM IMS-LAS-GU-4401                                              
042700     PERFORM IMS-LAS-GNP-4402                                             
042800     IF SEGMENT-FINNS                                                     
042900         PERFORM FA-FLYTTA-TILL-4402-FIRSTFALT                            
043000         MOVE +1 TO RAD-IX                                                
043100                                                                          
043200         PERFORM UNTIL RAD-IX NOT < RAD-MAX-PLUS-1                        
043300             IF SEGMENT-FINNS                                             
043400                 PERFORM FB-LAGG-UT-4402-DATA                             
043500                 PERFORM IMS-LAS-GU-4405                                  
043600                 PERFORM IMS-LAS-GNP-4408-KVAL                            
043700                 PERFORM FC-FLYTTA-TILL-4408-FIRSTFALT                    
043800                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
043900                     RAD-IX NOT < RAD-MAX-PLUS-1                          
044000                     PERFORM FD-LAGG-UT-4408-DATA                         
044100                     ADD +1             TO RAD-IX                         
044200                     PERFORM IMS-LAS-GNP-4408-KVAL                        
044300                 END-PERFORM                                              
044400                 IF SEGMENT-FINNS                                         
044500                     PERFORM FE-FLYTTA-TILL-4408-NEXTFALT                 
044600                     MOVE MED-1 (SPRAK-IX)  TO MOD-TEMFSINF               
044700                     MOVE NEJ               TO SW-LAS-4402                
044800                 ELSE                                                     
044900                     MOVE SPACE             TO MOD-ADFLGEO-NEXT           
045000                     MOVE ZERO              TO MOD-ADFLOMR-NEXT           
045100                                               MOD-ADRUTNIV-NEXT          
045200                     MOVE JA                TO SW-LAS-4402                
045300                 END-IF                                                   
045400                 IF SW-LAS-4402 = JA                                      
045500                     PERFORM IMS-LAS-GNP-4402                             
045600                 END-IF                                                   
045700             ELSE                                                         
045800                 MOVE MFS-RENSA-FAELT TO MOD-RAD (RAD-IX)                 
045900                 ADD +1             TO RAD-IX                             
046000             END-IF                                                       
046100         END-PERFORM                                                      
046200         IF SEGMENT-FINNS AND SW-LAS-4402 = JA                            
046300             PERFORM FF-FLYTTA-TILL-4402-NEXTFALT                         
046400             MOVE MED-1 (SPRAK-IX)  TO MOD-TEMFSINF                       
046500         ELSE                                                             
046600             MOVE ZERO         TO MOD-IDKUNDNR-NEXT                       
046700                                  MOD-KDFRAKT-NEXT                        
046800                                  MOD-IDTRPTNR-NEXT                       
046900                                  MOD-KDORDKLX-NEXT                       
047000         END-IF                                                           
047100     ELSE                                                                 
047200         MOVE FEL-796 (SPRAK-IX)         TO MOD-TEMFSFEL                  
047300     END-IF                                                               
047400                                                                          
047500     .                                                                    
047600     EJECT                                                                
047700 FA-FLYTTA-TILL-4402-FIRSTFALT SECTION.                                   
047800     MOVE 4402-IDKUNDNR      TO MOD-IDKUNDNR-FIRST                        
047900     MOVE 4402-KDFRAKT       TO MOD-KDFRAKT-FIRST                         
048000     MOVE 4402-KDORDKLX      TO MOD-KDORDKLX-FIRST                        
048100     MOVE 4402-IDTRPTNR      TO MOD-IDTRPTNR-FIRST                        
048200     SKIP1                                                                
048300                                                                          
048400     .                                                                    
048500 FB-LAGG-UT-4402-DATA SECTION.                                            
048600                                                                          
048700     MOVE 4402-IDTRPTNR      TO MOD-IDTRPTNR (RAD-IX)                     
048800                                MOD-IDTRPTNR-FIRST                        
048900                                MOD-IDTRPTNR-NEXT                         
049000                                W-4406-IDTRPTNR                           
049100     MOVE 4402-IDKUNDNR      TO MOD-IDKUNDNR (RAD-IX)                     
049200     MOVE 4402-KDFRAKT       TO MOD-KDFRAKT (RAD-IX)                      
049300     MOVE 4402-KDORDKLX      TO MOD-KDORDKLX (RAD-IX)                     
049400     MOVE 4402-IDDC-CROSS    TO MOD-IDDC-CROSS (RAD-IX)                   
049500     MOVE 4402-TEFLNOTE      TO MOD-TEFLNOTE (RAD-IX)                     
049600     SKIP1                                                                
049700                                                                          
049800     .                                                                    
049900 FC-FLYTTA-TILL-4408-FIRSTFALT SECTION.                                   
050000                                                                          
050100     MOVE 4408-TRPTRUT-ADFLGEO       TO MOD-ADFLGEO-FIRST                 
050200     MOVE 4408-TRPTRUT-ADFLOMR       TO MOD-ADFLOMR-FIRST                 
050300     MOVE 4408-TRPTRUT-ADRUTNIV      TO MOD-ADRUTNIV-FIRST                
050400     SKIP1                                                                
050500                                                                          
050600     .                                                                    
050700 FD-LAGG-UT-4408-DATA SECTION.                                            
050800                                                                          
050900     MOVE 4408-TRPTRUT-ADFLGEO       TO MOD-ADFLGEO (RAD-IX)              
051000     MOVE SPACE                      TO MOD-ADFLGEO-NEXT                  
051100     MOVE 4408-TRPTRUT-ADFLOMR       TO MOD-ADFLOMR (RAD-IX)              
051200     MOVE ZERO                       TO MOD-ADFLOMR-NEXT                  
051300     MOVE 4408-TRPTRUT-ADRUTNIV      TO MOD-ADRUTNIV (RAD-IX)             
051400     MOVE ZERO                       TO MOD-ADRUTNIV-NEXT                 
051500                                                                          
051600     .                                                                    
051700     EJECT                                                                
051800 FE-FLYTTA-TILL-4408-NEXTFALT SECTION.                                    
051900                                                                          
052000     MOVE 4408-TRPTRUT-ADFLGEO       TO MOD-ADFLGEO-NEXT                  
052100     MOVE 4408-TRPTRUT-ADFLOMR       TO MOD-ADFLOMR-NEXT                  
052200     MOVE 4408-TRPTRUT-ADRUTNIV      TO MOD-ADRUTNIV-NEXT                 
052300     SKIP3                                                                
052400                                                                          
052500     .                                                                    
052600 FF-FLYTTA-TILL-4402-NEXTFALT SECTION.                                    
052700                                                                          
052800     MOVE 4402-IDKUNDNR      TO MOD-IDKUNDNR-NEXT                         
052900     MOVE 4402-KDFRAKT       TO MOD-KDFRAKT-NEXT                          
053000     MOVE 4402-KDORDKLX      TO MOD-KDORDKLX-NEXT                         
053100     MOVE 4402-IDTRPTNR      TO MOD-IDTRPTNR-NEXT                         
053200                                                                          
053300     .                                                                    
053400     EJECT                                                                
053500 MFS-RENSA SECTION.                                                       
053600                                                                          
053700     MOVE MFS-RENSA-FAELT            TO MOD-IDKUNDNR-NEXT                 
053800                                        MOD-KDFRAKT-NEXT                  
053900                                        MOD-KDORDKLX-NEXT                 
054000                                        MOD-IDTRPTNR-NEXT                 
054100                                        MOD-ADFLGEO-NEXT                  
054200                                        MOD-ADFLOMR-NEXT                  
054300                                        MOD-ADRUTNIV-NEXT                 
054400                                        MOD-IDKUNDNR-FIRST                
054500                                        MOD-KDFRAKT-FIRST                 
054600                                        MOD-KDORDKLX-FIRST                
054700                                        MOD-IDTRPTNR-FIRST                
054800                                        MOD-ADFLGEO-FIRST                 
054900                                        MOD-ADFLOMR-FIRST                 
055000                                        MOD-ADRUTNIV-FIRST                
055100                                                                          
055200     MOVE +1 TO RAD-IX                                                    
055300     PERFORM UNTIL RAD-IX NOT < RAD-MAX-PLUS-1                            
055400         MOVE MFS-RENSA-FAELT        TO MOD-RAD (RAD-IX)                  
055500         ADD +1 TO RAD-IX                                                 
055600     END-PERFORM                                                          
055700                                                                          
055800     .                                                                    
055900     EJECT                                                                
056000 S01-FLYTTA-4402-FIRSTFALT SECTION.                                       
056100                                                                          
056200     MOVE MID-IDKUNDNR-FIRST             TO W-4402-IDKUNDNR-MIN           
056300     MOVE MID-KDFRAKT-FIRST              TO W-4402-KDFRAKT-MIN            
056400     MOVE MID-KDORDKLX-FIRST             TO W-4402-KDORDKLX-MIN           
056500                                                                          
056600     EJECT                                                                
056700*IMS SEKTIONER                                                            
056800                                                                          
056900     .                                                                    
057000 IMS-GET-MSG SECTION.                                                     
057100                                                                          
057200     MOVE '  QC' TO GODK-STATUSKODER                                      
057300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
057400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     SKIP2                                                                
057700     .                                                                    
057800 IMS-INSERT-MSG SECTION.                                                  
057900                                                                          
058000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
058100       MOVE '0' TO MFS-KDHUVOMR                                           
058200     END-IF                                                               
058300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
058400     MOVE SPACE TO GODK-STATUSKODER                                       
058500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
058600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058700     PERFORM IMS-STATUSKONTROLL                                           
058800                                                                          
058900     .                                                                    
059000     EJECT                                                                
059100 IMS-LAS-GU-4401 SECTION.                                                 
059200                                                                          
059300     STRING 'WLXXDM01(WDGXKEY  =' W-4401-WDGXKEY-X ')'                    
059400            DELIMITED BY SIZE INTO SSA1                                   
059500     MOVE '  ' TO GODK-STATUSKODER                                        
059600     CALL CBLTDLI USING GU XXDM-PCB DLI-IO-AREA SSA1                      
059700     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
059800     PERFORM IMS-STATUSKONTROLL                                           
059900                                                                          
060000     SKIP2                                                                
060100     .                                                                    
060200 IMS-LAS-GNP-4402 SECTION.                                                
060300                                                                          
060400     STRING 'WLXXDM11(KY4402  >=' W-4402-KY4402-MIN-X                     
060500                    '&KY4402  <=' W-4402-KY4402-MAX-X ')'                 
060600            DELIMITED BY SIZE INTO SSA1                                   
060700     MOVE '  GE' TO GODK-STATUSKODER                                      
060800     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-AREA SSA1                     
060900     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
061000     PERFORM IMS-STATUSKONTROLL                                           
061100                                                                          
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-LAS-GU-4405 SECTION.                                                 
061500                                                                          
061600     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
061700            DELIMITED BY SIZE INTO SSA1                                   
061800     MOVE '  GE' TO GODK-STATUSKODER                                      
061900     CALL CBLTDLI USING GU XXDN-PCB DLI-IO-AREA SSA1                      
062000     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     SKIP2                                                                
062300                                                                          
062400     .                                                                    
062500 IMS-LAS-GNP-4408-KVAL SECTION.                                           
062600                                                                          
062700     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
062800            DELIMITED BY SIZE INTO SSA1                                   
062900     STRING 'WLXXDN21(WDGXKEY >=' W-4408-WDGXKEY-MIN-X ')'                
063000            DELIMITED BY SIZE INTO SSA2                                   
063100     MOVE '  GE' TO GODK-STATUSKODER                                      
063200     CALL CBLTDLI USING GNP XXDN-PCB DLI-IO-AREA SSA1 SSA2                
063300     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
063400     PERFORM IMS-STATUSKONTROLL                                           
063500                                                                          
063600     .                                                                    
063700     EJECT                                                                
063800 IMS-GU-WDB601    SECTION.                                                
063900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
064000          DELIMITED BY SIZE INTO SSA1                                     
064100     MOVE '  GE' TO GODK-STATUSKODER                                      
064200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
064300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     IF SEGMENT-SAKNAS                                                    
064600         MOVE SPACE TO DCS-KDDC                                           
064700     END-IF                                                               
064800     .                                                                    
064900 IMS-STATUSKONTROLL SECTION.                                              
065000                                                                          
065100     SET STATUS-IX TO 1                                                   
065200     SEARCH GODK-STATUS AT END CALL FELLOG                                
065300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
065400     END-SEARCH                                                           
065500     .                                                                    
