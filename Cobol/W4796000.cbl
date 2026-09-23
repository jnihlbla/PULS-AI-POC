000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4796000.                                                 
000300 AUTHOR.        SVANTE BJÖRKBERG.                                         
000400 DATE-WRITTEN.  JAN 1987.                                                 
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET LÄSER WDE6 MED SB.                                    
001100*        PROGRAMMET LÄSER WDE2.                                           
001200*        SUGER UT INFORMATION OCH SKAPAR ETT ANTAL FILER.                 
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800*                                                                         
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*                                                                         
002200     SELECT W47960    ASSIGN TO W47960D1.                                 
002300     SELECT W47961    ASSIGN TO W47960D2.                                 
002400     SELECT W47963    ASSIGN TO W47960D3.                                 
002500     SELECT W47964    ASSIGN TO W47960D4.                                 
002600     SELECT W47965    ASSIGN TO W47960D5.                                 
002700     SELECT W47966    ASSIGN TO W47960D6.                                 
002800*                                                                         
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47960                                                               
003500     LABEL RECORD    STANDARD                                             
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*01  E60-AREA  -COPY W479060   -L                                         
004000     SKIP3                                                                
004100 FD  W47961                                                               
004200     LABEL RECORD    STANDARD                                             
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS 0.                                                    
004500                                                                          
004600*01  E61-AREA  -COPY W479061   -L                                         
004700     SKIP3                                                                
004800 FD  W47963                                                               
004900     LABEL RECORD    STANDARD                                             
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*01  E63-AREA  -COPY W479063   -L                                         
005400     EJECT                                                                
005500 FD  W47964                                                               
005600     LABEL RECORD    STANDARD                                             
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000*01  E64-AREA  -COPY W479064   -L                                         
006100     SKIP3                                                                
006200 FD  W47965                                                               
006300     LABEL RECORD    STANDARD                                             
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS 0.                                                    
006600                                                                          
006700*01  AREA-002  -COPY W462001   -L                                         
006800     SKIP3                                                                
006900 FD  W47966                                                               
007000     LABEL RECORD    STANDARD                                             
007100     RECORDING       V                                                    
007200     BLOCK CONTAINS 0.                                                    
007300                                                                          
007400*01  W47966-AREA  -COPY W479A01   -L                                      
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900*                                                                         
008000 77  PROGRAM-NAMN                PIC X(6) VALUE 'W47960'.                 
008100                                                                          
008200 77  JA                          PIC X(1)    VALUE 'J'.                   
008300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
008400 77  ORDERNR-SAKNAS              PIC X(1).                                
008500 77  FL-SKRIV-E61                PIC X(1).                                
008600 77  FL-SKRIV-A11                PIC X(1).                                
008700 77  SPAR-IDDC                   PIC  X(2).                               
008800                                                                          
008900 01  W-IDUSER                    PIC X(8)    VALUE SPACE.                 
009000                                                                          
009100 01  DAGENS-DATUM.                                                        
009200   03  DAGENS-AAR                PIC 9(2).                                
009300   03  DAGENS-VECKA              PIC 9(2).                                
009400                                                                          
009500 01  FORRA-VECKA-DATUM.                                                   
009600   03  FORRA-AAR                 PIC 9(2).                                
009700   03  FORRA-VECKA               PIC 9(2).                                
009800   03  FORRA-VECKA-DAG           PIC 9(1).                                
009900                                                                          
010000 01  WS-ODELTIPACKN-AAWW.                                                 
010100   03  ODEL-AAR                PIC 9(2).                                  
010200   03  ODEL-VECKA              PIC 9(2).                                  
010300                                                                          
010400 01  LAST-DATUM-E61.                                                      
010500   03  LAST-AAR-E61            PIC 9(2).                                  
010600   03  LAST-VECKA-E61          PIC 9(2).                                  
010700                                                                          
010800 01  FAKT-DATUM-E60.                                                      
010900   03  FAKT-AAR-E60            PIC 9(2).                                  
011000   03  FAKT-VECKA-E60          PIC 9(2).                                  
011100                                                                          
011200 01  FAKT-DATUM-E61.                                                      
011300   03  FAKT-AAR-E61            PIC 9(2).                                  
011400   03  FAKT-VECKA-E61          PIC 9(2).                                  
011500                                                                          
011600 01  FAKT-DATUM-E64.                                                      
011700   03  FAKT-AAR-E64            PIC 9(2).                                  
011800   03  FAKT-VECKA-E64          PIC 9(2).                                  
011900   03  FAKT-VECKA-E64-DAG      PIC 9(1).                                  
012000                                                                          
012100 01  FAKT-DATUM-A01.                                                      
012200   03  FAKT-AAR-A01            PIC 9(2).                                  
012300   03  FAKT-VECKA-A01          PIC 9(2).                                  
012400                                                                          
012500 01  LAST-DATUM-A01.                                                      
012600   03  LAST-AAR-A01            PIC 9(2).                                  
012700   03  LAST-VECKA-A01          PIC 9(2).                                  
012800     SKIP3                                                                
012900 01  DYNAMISKA-SUBPROGRAM.                                                
013000*                                                                         
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
013400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013600     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
013700                                                                          
013800 01  TEST-IDDISTR                PIC 9(5) COMP-3 VALUE ZERO.              
013900     SKIP3                                                                
014000*01  FILLER -COPY WWDIS130    -RED TEST-IDDISTR                           
014001     EJECT                                                                
014010*01  -COPY WWDCKONS                                                       
014020     EJECT                                                                
014100 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
014200     SKIP2                                                                
014300*01          -COPY W479060  -PRE E60-                                     
014400     EJECT                                                                
014500*01          -COPY W479061  -PRE E61-                                     
014600     EJECT                                                                
014700*01          -COPY W479063  -PRE E63-                                     
014800     EJECT                                                                
014900*01          -COPY W479064  -PRE E64-                                     
015000     EJECT                                                                
015100*01          -COPY W462001                                                
015200     EJECT                                                                
015300*01          -COPY W479A01  -PRE A01-                                     
015400     EJECT                                                                
015500*01          -COPY W479A11  -PRE A11-                                     
015600     EJECT                                                                
015700*01  -COPY W460DIS1                                                       
015800     EJECT                                                                
015900 01  FILLER                      PIC X(08)   VALUE 'DATUM   '.            
016000     SKIP2                                                                
016100*01  -COPY WDATAREA                                                       
016200     EJECT                                                                
016300*   ---- PARAMETER TILL DATUMKORT                                         
016400                                                                          
016500 01  DATUMKORT-ID                PIC X(06) VALUE 'WDATUM'.                
016600                                                                          
016700*01  -COPY WDATKORT                                                       
016800     EJECT                                                                
016900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
017000                                                                          
017100 01 NYCKLAR-TILL-DLI.                                                     
017200    03 W-WDE4F1KY-MIN-X.                                                  
017300      05 W-IDPRODNR-MIN    PIC S9(7) VALUE +0 COMP-3.                     
017400      05 W-IDKOLLI-MIN     PIC S9(5) VALUE +0 COMP-3.                     
017500      05 FILLER            PIC X(22) VALUE LOW-VALUE.                     
017600                                                                          
017700    03 W-WDE4F1KY-MAX-X.                                                  
017800      05 W-IDPRODNR-MAX    PIC S9(7) VALUE +0 COMP-3.                     
017900      05 W-IDKOLLI-MAX     PIC S9(5) VALUE +0 COMP-3.                     
018000      05 FILLER            PIC X(22) VALUE HIGH-VALUE.                    
018100                                                                          
018200    03 W-WDQ3DSEQ-MIN-X.                                                  
018300      05 W-Q3DSEQ-IDPRODNR-MIN    PIC S9(7) COMP-3.                       
018400      05 W-Q3DSEQ-IDPLKLST-MIN    PIC S9(3) VALUE 000 COMP-3.             
018500    03 W-WDQ3DSEQ-MAX-X.                                                  
018600      05 W-Q3DSEQ-IDPRODNR-MAX    PIC S9(7) COMP-3.                       
018700      05 W-Q3DSEQ-IDPLKLST-MAX    PIC S9(3) VALUE 999 COMP-3.             
018800                                                                          
018900    03 W-IDGMT-MIN-X.                                                     
019000       05 W-IDDISTR-WDB2-MIN     PIC S9(5)    COMP-3.                     
019100       05 W-IDKUNDNR-WDB2-MIN    PIC S9(7)    COMP-3.                     
019200                                                                          
019300    03 W-IDGMT-MAX-X.                                                     
019400       05 W-IDDISTR-WDB2-MAX     PIC S9(5)    COMP-3.                     
019500       05 W-IDKUNDNR-WDB2-MAX    PIC S9(7)    COMP-3.                     
019600                                                                          
019700    03 W-IDSHIPM-X.                                                       
019800       05 W-IDSHIPM              PIC 9(7).                                
019900                                                                          
020000 01  IMS-WS.                                                              
020100                                                                          
020200     03  STATUS-WS                 PIC X(2).                              
020300        88  SEGMENT-FINNS                    VALUE '  '.                  
020400        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
020500        88  SEGMENT-SLUT                     VALUE 'GB'.                  
020600                                                                          
020700     03  GODK-STATUSKODER.                                                
020800         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
020900                                                                          
021000     SKIP3                                                                
021100 01  SSA1                        PIC X(150).                              
021200     EJECT                                                                
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*01  -COPY W0005    -PRE POSTSUM-                                         
021700     EJECT                                                                
021800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA1'.              
021900 01  DLI-IO-AREA1.                                                        
022000     03  IO-AREA             PIC X(352).                                  
022100     SKIP3                                                                
022200*    03  WDE601    -COPY WDE601    -RED IO-AREA                           
022300     EJECT                                                                
022400*    03  WDE611    -COPY WDE611    -RED IO-AREA                           
022500     EJECT                                                                
022600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA2'.              
022700 01  DLI-IO-AREA2.                                                        
022800     03  IO-AREA             PIC X(192).                                  
022900     SKIP3                                                                
023000*    03  WDQ301    -COPY WDQ301    -RED IO-AREA                           
023100     EJECT                                                                
023200 01  FILLER                  PIC X(16) VALUE 'WDB201-AREA'.               
023300 01  DLI-IO-AREA-WDB201.                                                  
023400     03  WLGMTA01.                                                        
023500*        05  -COPY WDB201                                                 
023600     EJECT                                                                
023700 01  FILLER                  PIC X(16) VALUE 'WDE4F1-AREA'.               
023800 01  DLI-IO-E4F1.                                                         
023900*    03  -COPY WDE4F1                                                     
024000     SKIP3                                                                
024100 01  FILLER                  PIC X(16) VALUE 'WDE201-AREA'.               
024200 01  DLI-IO-WDE201.                                                       
024300*    03  -COPY WDE201                                                     
024400     EJECT                                                                
024500 LINKAGE SECTION.                                                         
024600     SKIP3                                                                
024700*01  -COPY W0008   -PRE WDE6-                                             
024800         05  FILLER          PIC X(1).                                    
024900     SKIP3                                                                
025000*01  -COPY W0008   -PRE WDE4F-                                            
025100         05  FILLER          PIC X(1).                                    
025200     SKIP3                                                                
025300*01  -COPY W0008   -PRE ORQD-                                             
025400         05  FILLER          PIC X(1).                                    
025500     EJECT                                                                
025600*01  -COPY W0008   -PRE GMTA-                                             
025700         05  FILLER          PIC X(1).                                    
025800     EJECT                                                                
025900*01  -COPY W0008   -PRE WDE2-                                             
026000         05  FILLER          PIC X(1).                                    
026100     EJECT                                                                
026200 PROCEDURE DIVISION  USING WDE6-PCB WDE4F-PCB ORQD-PCB GMTA-PCB           
026300                           WDE2-PCB.                                      
026400     ENTRY 'DLITCBL' USING WDE6-PCB WDE4F-PCB ORQD-PCB GMTA-PCB           
026500                           WDE2-PCB.                                      
026600                                                                          
026700     PERFORM A-INIT                                                       
026800     PERFORM IMS-GET-WDE6                                                 
026900                                                                          
027000     PERFORM UNTIL SEGMENT-SLUT                                           
027100                                                                          
027200       EVALUATE WDE6-SEG-NAME-FB                                          
027300         WHEN 'WDE601'                                                    
027400           PERFORM WDE601-SKAPA-E60                                       
027500           PERFORM WDE601-SKAPA-E61                                       
027600           PERFORM WDE601-SKAPA-E63                                       
027700           PERFORM WDE601-SKAPA-E64                                       
027800           PERFORM WDE601-SKRIV-002                                       
027900           PERFORM WDE601-SKRIV-A01                                       
028000           PERFORM WDE601-SKAPA-A11                                       
028100           MOVE JA       TO ORDERNR-SAKNAS                                
028200                                                                          
028300         WHEN 'WDE611'                                                    
028400           PERFORM WDE611-SKRIV-E60                                       
028500           PERFORM WDE611-SKRIV-E61                                       
028600           PERFORM WDE611-SKAPA-E63                                       
028700           PERFORM WDE611-SKAPA-E64                                       
028800           PERFORM WDE611-SKRIV-A11                                       
028900                                                                          
029000           MOVE KOLLI-IDKOLLI       TO W-IDKOLLI-MIN                      
029100                                       W-IDKOLLI-MAX                      
029200           PERFORM IMS-GU-WDE4F1                                          
029300           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
029400             PERFORM WDE4F-SKAPA-E60                                      
029500             PERFORM WDE4F-SKAPA-E61                                      
029600             PERFORM WDE4F-SKRIV-E63                                      
029700             PERFORM WDE4F-SKRIV-E64                                      
029800             MOVE NEJ      TO ORDERNR-SAKNAS                              
029900             PERFORM IMS-GN-WDE4F1                                        
030000           END-PERFORM                                                    
030100                                                                          
030200       END-EVALUATE                                                       
030300                                                                          
030400       PERFORM IMS-GET-WDE6                                               
030500     END-PERFORM                                                          
030600                                                                          
030700     PERFORM Z-FINIT                                                      
030800     MOVE ZERO TO RETURN-CODE                                             
030900     GOBACK.                                                              
031000     EJECT                                                                
031100                                                                          
031200 A-INIT SECTION.                                                          
031300     OPEN OUTPUT W47960                                                   
031400                 W47961                                                   
031500                 W47963                                                   
031600                 W47964                                                   
031700                 W47965                                                   
031800                 W47966                                                   
031900                                                                          
032000     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
032100     MOVE ZERO             TO SPAR-IDDC                                   
032200                              WS-ODELTIPACKN-AAWW                         
032300                                                                          
032400     CALL DATKORT  USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT              
032500                                                                          
032600     MOVE D-AAR            TO DAGENS-AAR                                  
032700     MOVE D-VECKA          TO DAGENS-VECKA                                
032800                                                                          
032900     IF DAGENS-VECKA > 01                                                 
033000       MOVE D-AAR            TO FORRA-AAR                                 
033100       COMPUTE FORRA-VECKA = DAGENS-VECKA - 1                             
033200       MOVE 7                TO FORRA-VECKA-DAG                           
033300     ELSE                                                                 
033400       MOVE '00000'          TO FORRA-VECKA-DATUM                         
033500     END-IF                                                               
033600                                                                          
033700     MOVE LOW-VALUE                TO W-IDGMT-MIN-X                       
033800     MOVE HIGH-VALUE               TO W-IDGMT-MAX-X                       
033900                                                                          
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 WDE601-SKAPA-E60       SECTION.                                          
034400     MOVE VORD-IDDC                 TO E60-IDDC                           
034500     IF VORD-IDDC-EXP > SPACE                                             
034510       IF VORD-IDDC-EXP = WC-CDC-SE                                       
034600         MOVE VORD-IDDC-EXP         TO E60-IDDC                           
034700       END-IF                                                             
034710     END-IF                                                               
034800     MOVE VORD-IDDISTR              TO E60-IDDISTR                        
034900     MOVE VORD-IDKUNDNR             TO E60-IDKUNDNR                       
035000     MOVE VORD-IDPRODNR             TO E60-IDPRODNR                       
035100                                       W-Q3DSEQ-IDPRODNR-MIN              
035200                                       W-Q3DSEQ-IDPRODNR-MAX              
035300                                       W-IDPRODNR-MIN                     
035400                                       W-IDPRODNR-MAX                     
035500     MOVE VORD-KDORDKL              TO E60-KDORDKL                        
035600     MOVE VORD-KDFRAKT              TO E60-KDFRAKT                        
035700     MOVE VORD-FLDIRLEV             TO E60-FLDIRLEV.                      
035800     EJECT                                                                
035900                                                                          
036000 WDE601-SKAPA-E61       SECTION.                                          
036100     MOVE NEJ                           TO FL-SKRIV-E61                   
036200                                                                          
036300     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                                
036400       IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT  AND                           
036500          VORD-KVKOLLI = VORD-KVKOLLI-LAST                                
036600*                                                                         
036700         MOVE VORD-TIFAKT-SK            TO DAT-I-TIDATUM                  
036800         PERFORM S03-CALL-RDATCONV-AAMMDD                                 
036900                                                                          
037000         IF DAT-KDSVAR-OK                                                 
037100           MOVE DAT-TIAA-VECKA          TO FAKT-AAR-E61                   
037200           MOVE DAT-TIVV                TO FAKT-VECKA-E61                 
037300         ELSE                                                             
037400           MOVE '0000'                  TO FAKT-DATUM-E61                 
037500         END-IF                                                           
037600*                                                                         
037700         MOVE VORD-TILASTN-SK           TO DAT-I-TIDATUM                  
037800         PERFORM S03-CALL-RDATCONV-AAMMDD                                 
037900                                                                          
038000         IF DAT-KDSVAR-OK                                                 
038100           MOVE DAT-TIAA-VECKA          TO LAST-AAR-E61                   
038200           MOVE DAT-TIVV                TO LAST-VECKA-E61                 
038300         ELSE                                                             
038400           MOVE '0000'                  TO LAST-DATUM-E61                 
038500         END-IF                                                           
038600*                                                                         
038700         IF DAGENS-DATUM = FAKT-DATUM-E61  OR                             
038800            DAGENS-DATUM = LAST-DATUM-E61                                 
038900           MOVE VORD-IDDC               TO E61-IDDC                       
039000           IF VORD-IDDC-EXP > SPACE                                       
039100             MOVE VORD-IDDC-EXP         TO E61-IDDC                       
039200           END-IF                                                         
039300           MOVE VORD-IDDISTR            TO E61-IDDISTR                    
039400           MOVE VORD-IDKUNDNR           TO E61-IDKUNDNR                   
039500           MOVE VORD-IDPRODNR           TO E61-IDPRODNR                   
039600           MOVE VORD-KDORDKL            TO E61-KDORDKL                    
039700           MOVE VORD-KDFRAKT            TO E61-KDFRAKT                    
039800           MOVE VORD-KDORDLOT           TO E61-KDORDLOT                   
039900           MOVE VORD-IDLOTNR            TO E61-IDLOTNR                    
040000           MOVE VORD-DABEGPAC (3:6)     TO E61-TIBEGPAC                   
040100           MOVE 0                       TO E61-TIORDREG                   
040200           MOVE VORD-TIREGTID           TO E61-TIREGTID                   
040300           MOVE VORD-TIUTSKR            TO E61-TIUTSKR                    
040400           MOVE VORD-TIUTSTID           TO E61-TIUTSTID                   
040500           MOVE VORD-FLDIRLEV           TO E61-FLDIRLEV                   
040600                                                                          
040700           MOVE JA                      TO FL-SKRIV-E61                   
040800         END-IF                                                           
040900       END-IF                                                             
041000     END-IF.                                                              
041100     EJECT                                                                
041200                                                                          
041300 WDE601-SKAPA-E63       SECTION.                                          
041400     MOVE VORD-IDPRODNR             TO E63-IDPRODNR.                      
041500     EJECT                                                                
041600                                                                          
041700 WDE601-SKAPA-E64       SECTION.                                          
041800     MOVE VORD-IDPRODNR             TO E64-IDPRODNR.                      
041900     EJECT                                                                
042000                                                                          
042100 WDE601-SKRIV-002       SECTION.                                          
042200     MOVE VORD-IDDISTR          TO DIS1-IDDISTR TEST-IDDISTR              
042300                                                                          
042400     CALL W460DIS1 USING DIS1-W460DIS1                                    
042500                                                                          
042600     IF DIS130-NOAC OR DIS1-KDSVAR = JA                                   
042700       IF VORD-KVORDRAD     = VORD-KVORDRAD-PACK   AND                    
042800          VORD-KVKOLLI      = VORD-KVKOLLI-FAKT    AND                    
042900          VORD-KVKOLLI      = VORD-KVKOLLI-LAST                           
043000         CONTINUE                                                         
043100       ELSE                                                               
043200                                                                          
043300         MOVE '002'               TO ORDER-IDPTYP                         
043400         MOVE VORD-IDPRODNR       TO ORDER-IDPRODNR                       
043500         MOVE VORD-IDDISTR        TO ORDER-IDDISTR                        
043600         MOVE VORD-IDKUNDNR       TO ORDER-IDKUNDNR                       
043700         MOVE ZERO                TO ORDER-IDORDNR                        
043800                                     ORDER-TIORDREG                       
043900                                                                          
044000         WRITE AREA-002  FROM ORDER-W462001                               
044100                                                                          
044200         MOVE 'W47965'     TO POSTSUM-FDNAMN                              
044300         MOVE 'W47960D5'   TO POSTSUM-DDNAMN2                             
044400         MOVE '002'        TO POSTSUM-TRANSTYP                            
044500                                                                          
044600         CALL POSTSUM USING POSTSUM-PARM                                  
044700       END-IF                                                             
044800     END-IF.                                                              
044900     EJECT                                                                
045000                                                                          
045100 WDE601-SKRIV-A01       SECTION.                                          
045200     MOVE NEJ                           TO FL-SKRIV-A11                   
045300                                                                          
045400     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                                
045500       IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT  AND                           
045600          VORD-KVKOLLI = VORD-KVKOLLI-LAST  AND                           
045700          VORD-KVKOLLI = VORD-KVKOLPAC                                    
045800*                                                                         
045900         IF VORD-KVKOLPAC  = ZERO                                         
046000           MOVE VORD-TIPACKN-SK           TO DAT-I-TIDATUM                
046100         ELSE                                                             
046200           MOVE VORD-TIFAKT-SK            TO DAT-I-TIDATUM                
046300         END-IF                                                           
046400                                                                          
046500         PERFORM S03-CALL-RDATCONV-AAMMDD                                 
046600                                                                          
046700         IF DAT-KDSVAR-OK                                                 
046800           MOVE DAT-TIAA-VECKA          TO FAKT-AAR-A01                   
046900           MOVE DAT-TIVV                TO FAKT-VECKA-A01                 
047000         ELSE                                                             
047100           MOVE '0000'                  TO FAKT-DATUM-A01                 
047200         END-IF                                                           
047300*                                                                         
047400         MOVE VORD-TILASTN-SK           TO DAT-I-TIDATUM                  
047500         PERFORM S03-CALL-RDATCONV-AAMMDD                                 
047600                                                                          
047700         IF DAT-KDSVAR-OK                                                 
047800           MOVE DAT-TIAA-VECKA          TO LAST-AAR-A01                   
047900           MOVE DAT-TIVV                TO LAST-VECKA-A01                 
048000         ELSE                                                             
048100           MOVE '0000'                  TO LAST-DATUM-A01                 
048200         END-IF                                                           
048300*                                                                         
048400         IF DAGENS-DATUM = FAKT-DATUM-A01  OR                             
048500            DAGENS-DATUM = LAST-DATUM-A01                                 
048600           MOVE SPACE     TO W-IDUSER                                     
048700           PERFORM S04-SKRIV-W47966-AREA                                  
048800         ELSE                                                             
048900           PERFORM IMS-GU-WDQ3DSEQ-ORQA01                                 
049000           PERFORM UNTIL SEGMENT-SAKNAS OR                                
049100                         SEGMENT-SLUT                                     
049200             IF ODEL-IDPRODNR = VORD-IDPRODNR                             
049300               MOVE ODEL-TIPACKN    TO DAT-I-TIDATUM                      
049400               PERFORM S03-CALL-RDATCONV-AAMMDD                           
049500               IF DAT-KDSVAR-OK                                           
049600                 IF DAT-TIVV = DAGENS-VECKA                               
049700                   MOVE ODEL-IDUSER  TO W-IDUSER                          
049800                   PERFORM S04-SKRIV-W47966-AREA                          
049900                 END-IF                                                   
050000               END-IF                                                     
050100             END-IF                                                       
050200             PERFORM IMS-GN-WDQ3DSEQ-ORQA01                               
050300           END-PERFORM                                                    
050400         END-IF                                                           
050500       END-IF                                                             
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 WDE601-SKAPA-A11       SECTION.                                          
051000     IF FL-SKRIV-A11 = JA                                                 
051100       MOVE 'A11'                   TO A11-IDPTYP                         
051200       MOVE VORD-IDDISTR            TO A11-IDDISTR                        
051300       MOVE VORD-IDKUNDNR           TO A11-IDKUNDNR                       
051400       MOVE VORD-IDPRODNR           TO A11-IDPRODNR                       
051500       MOVE VORD-IDDC               TO A11-IDDC                           
051600                                       SPAR-IDDC                          
051700       MOVE SPACE                   TO A11-IDKUNDRF                       
051800       MOVE ZERO                    TO A11-IDPURAD                        
051900     END-IF.                                                              
052000     EJECT                                                                
052100                                                                          
052200 WDE611-SKRIV-E60       SECTION.                                          
052300     MOVE KOLLI-TIFAKT              TO DAT-I-TIDATUM                      
052400     PERFORM S03-CALL-RDATCONV-AAMMDD                                     
052500                                                                          
052600     IF DAT-KDSVAR-OK                                                     
052700       MOVE DAT-TIAA-VECKA          TO FAKT-AAR-E60                       
052800       MOVE DAT-TIVV                TO FAKT-VECKA-E60                     
052900     ELSE                                                                 
053000       MOVE '0000'                  TO FAKT-DATUM-E60                     
053100     END-IF                                                               
053200                                                                          
053300     IF FAKT-DATUM-E60 = DAGENS-DATUM                                     
053400       MOVE KOLLI-IDKOLLI           TO E60-IDKOLLI                        
053500       MOVE KOLLI-IDTRPTNR          TO E60-IDTRPTNR                       
053600       MOVE KOLLI-ADFLGEO           TO E60-ADFLGEO                        
053700       MOVE KOLLI-ADFLOMR           TO E60-ADFLOMR                        
053800       MOVE KOLLI-ADRUTNIV          TO E60-ADRUTNIV                       
053900       MOVE KOLLI-ADVMODUL          TO E60-ADVMODUL                       
054000       MOVE KOLLI-KDEMBTYP          TO E60-KDEMBTYP                       
054100       MOVE KOLLI-DIKOLLIL          TO E60-DIKOLLIL                       
054200       MOVE KOLLI-DIKOLLIB          TO E60-DIKOLLIB                       
054300       MOVE KOLLI-DIKOLLIH          TO E60-DIKOLLIH                       
054400       MOVE KOLLI-KDKOLLI           TO E60-KDKOLLI                        
054500       MOVE KOLLI-KVORDRAD          TO E60-KVORDRAD                       
054600       MOVE KOLLI-VKORDBTO-KOLLI    TO E60-VKORDBTO-KOLLI                 
054700       MOVE KOLLI-VKORDNTO-KOLLI    TO E60-VKORDNTO-KOLLI                 
054800       MOVE KOLLI-VLORDBTO-KOLLI    TO E60-VLORDBTO-KOLLI                 
054900       MOVE KOLLI-SUORDV-KOLLI      TO E60-SUORDV-KOLLI                   
055000       MOVE KOLLI-SUORDV-LOC        TO E60-SUORDV-LOC                     
055100       MOVE KOLLI-SUORDV-LOCPREL    TO E60-SUORDV-LOCPREL                 
055200       MOVE KOLLI-KDVALISO          TO E60-KDVALISO                       
055300       MOVE 'E60'                   TO E60-IDPTYP                         
055400                                                                          
055500       IF ORDERNR-SAKNAS = NEJ                                            
055600         PERFORM S01-SKRIV-E60                                            
055700       END-IF                                                             
055800     END-IF.                                                              
055900     EJECT                                                                
056000                                                                          
056100 WDE611-SKRIV-E61       SECTION.                                          
056200     IF FL-SKRIV-E61 = JA AND KOLLI-IDKOLLI < 99000                       
056300       MOVE KOLLI-IDKOLLI           TO E61-IDKOLLI                        
056400       MOVE KOLLI-IDTRPTNR          TO E61-IDTRPTNR                       
056500       MOVE KOLLI-TIPACKN           TO E61-TIPACKN                        
056600       MOVE KOLLI-TIPACTID          TO E61-TIPACTID                       
056700       MOVE KOLLI-TIFAKT            TO E61-TIFAKT                         
056800       MOVE KOLLI-TIFAKTID          TO E61-TIFAKTID                       
056900       MOVE KOLLI-TILASTN           TO E61-TILASTN                        
057000       MOVE KOLLI-TILASTID          TO E61-TILASTID                       
057100       MOVE 'E61'                   TO E61-IDPTYP                         
057200                                                                          
057300       IF ORDERNR-SAKNAS = NEJ                                            
057400         PERFORM S02-SKRIV-E61                                            
057500       END-IF                                                             
057600     END-IF.                                                              
057700     EJECT                                                                
057800                                                                          
057900 WDE611-SKAPA-E63       SECTION.                                          
058000     MOVE KOLLI-IDFAKT                TO E63-IDFAKT                       
058100     MOVE KOLLI-KDKOLSTA              TO E63-KDKOLSTA                     
058200     .                                                                    
058300     EJECT                                                                
058400                                                                          
058500 WDE611-SKAPA-E64       SECTION.                                          
058600     MOVE KOLLI-TIFAKT          TO DAT-I-TIDATUM                          
058700     PERFORM S03-CALL-RDATCONV-AAMMDD                                     
058800                                                                          
058900     IF DAT-KDSVAR-OK                                                     
059000       MOVE DAT-TIAA-VECKA      TO FAKT-AAR-E64                           
059100       MOVE DAT-TIVV            TO FAKT-VECKA-E64                         
059200       MOVE DAT-TID             TO FAKT-VECKA-E64-DAG                     
059300     ELSE                                                                 
059400       MOVE '00000'              TO FAKT-DATUM-E64                        
059500     END-IF                                                               
059600                                                                          
059700     IF FAKT-DATUM-E64(1:4) = DAGENS-DATUM                                
059800     OR FAKT-DATUM-E64      = FORRA-VECKA-DATUM                           
059900       MOVE KOLLI-IDDISTR         TO TEST-IDDISTR                         
060000       MOVE KOLLI-TIFAKT          TO E64-TIFAKT-BILLIT                    
060100       MOVE KOLLI-TILASTN         TO E64-TILASTN-BILLIT                   
060200       MOVE ZERO                  TO E64-TIFAKT                           
060300       MOVE ZERO                  TO E64-TILASTN                          
060400       IF KOLLI-IDSHIPM > ZERO                                            
060500         MOVE KOLLI-IDSHIPM       TO W-IDSHIPM                            
060600         PERFORM IMS-GU-WDE201                                            
060700         IF SEGMENT-FINNS                                                 
060800           MOVE BILL-IDSHIPM      TO E64-IDSHIPM                          
060900           MOVE BILL-TISKEPPN     TO E64-TISKEPPN                         
061000         ELSE                                                             
061100           MOVE ZERO              TO E64-IDSHIPM                          
061200           MOVE ZERO              TO E64-TISKEPPN                         
061300         END-IF                                                           
061400       ELSE                                                               
061500         MOVE ZERO                TO E64-IDSHIPM                          
061600         MOVE ZERO                TO E64-TISKEPPN                         
061700       END-IF                                                             
061800       MOVE KOLLI-IDKOLLI         TO E64-IDKOLLI                          
061900       MOVE KOLLI-TIPACKN         TO E64-TIPACKN                          
062000       MOVE KOLLI-IDFAKT          TO E64-IDFAKT                           
062100       MOVE KOLLI-KDKOLLI         TO E64-KDKOLLI                          
062200       MOVE KOLLI-KVORDRAD        TO E64-KVORDRAD                         
062300       MOVE KOLLI-SUORDV-KOLLI    TO E64-SUORDV-KOLLI                     
062400       MOVE KOLLI-SUORDV-LOC      TO E64-SUORDV-LOC                       
062500       MOVE KOLLI-SUORDV-LOCPREL  TO E64-SUORDV-LOCPREL                   
062600       MOVE KOLLI-KDVALISO        TO E64-KDVALISO                         
062700       MOVE KOLLI-VKORDBTO-KOLLI  TO E64-VKORDBTO-KOLLI                   
062800       MOVE KOLLI-VKORDNTO-KOLLI  TO E64-VKORDNTO-KOLLI                   
062900     END-IF.                                                              
063000     EJECT                                                                
063100                                                                          
063200 WDE611-SKRIV-A11       SECTION.                                          
063300     IF  FL-SKRIV-A11 = JA                                                
063400     AND KOLLI-IDKOLLI < 99000                                            
063500       MOVE SPAR-IDDC               TO A11-IDDC                           
063600       MOVE KOLLI-IDKOLLI           TO A11-IDKOLLI                        
063700       MOVE KOLLI-ADFLGEO           TO A11-ADFLGEO                        
063800       MOVE KOLLI-ADFLOMR           TO A11-ADFLOMR                        
063900       MOVE KOLLI-ADRUTNIV          TO A11-ADRUTNIV                       
064000       MOVE KOLLI-ADVMODUL          TO A11-ADVMODUL                       
064100       MOVE KOLLI-TIPACKN           TO A11-TIPACKN                        
064200       MOVE KOLLI-TIFAKT            TO A11-TIFAKT                         
064300       MOVE KOLLI-TILASTN           TO A11-TILASTN                        
064400       MOVE KOLLI-IDFAKT            TO A11-IDFAKT                         
064500       MOVE KOLLI-KVORDRAD          TO A11-KVORDRAD                       
064600       MOVE KOLLI-VKORDBTO-KOLLI    TO A11-VKORDBTO-KOLLI                 
064700       MOVE KOLLI-VLORDBTO-KOLLI    TO A11-VLORDBTO-KOLLI                 
064800       MOVE KOLLI-IDPLOCK           TO A11-IDPLOCK                        
064900       MOVE KOLLI-KDKOLLI           TO A11-KDKOLLI                        
065000       MOVE ZERO                    TO A11-IDARTNR                        
065100                                                                          
065200       WRITE W47966-AREA  FROM A11-W479A11                                
065300                                                                          
065400       MOVE 'W47966'     TO POSTSUM-FDNAMN                                
065500       MOVE 'W47960D6'   TO POSTSUM-DDNAMN2                               
065600       MOVE 'A11'        TO POSTSUM-TRANSTYP                              
065700                                                                          
065800       CALL POSTSUM USING POSTSUM-PARM                                    
065900     END-IF.                                                              
066000     EJECT                                                                
066100                                                                          
066200 WDE4F-SKAPA-E60               SECTION.                                   
066300     IF ORDERNR-SAKNAS = JA   AND                                         
066400        FAKT-VECKA-E60 = DAGENS-VECKA AND                                 
066500        FAKT-AAR-E60   = DAGENS-AAR                                       
066600       MOVE SEQF-IDKUNDRF             TO E60-IDKUNDRF                     
066700       PERFORM S01-SKRIV-E60                                              
066800     END-IF.                                                              
066900     EJECT                                                                
067000                                                                          
067100 WDE4F-SKAPA-E61               SECTION.                                   
067200     IF ORDERNR-SAKNAS = JA   AND                                         
067300        FL-SKRIV-E61   = JA                                               
067400       MOVE SEQF-IDKUNDRF             TO E61-IDKUNDRF                     
067500       PERFORM S02-SKRIV-E61                                              
067600     END-IF.                                                              
067700     EJECT                                                                
067800                                                                          
067900 WDE4F-SKRIV-E63               SECTION.                                   
068000     MOVE 'E63'                  TO E63-IDPTYP                            
068100     MOVE SEQF-IDDISTR           TO E63-IDDISTR                           
068200     MOVE SEQF-IDKUNDNR          TO E63-IDKUNDNR                          
068300     MOVE SEQF-IDKUNDRF          TO E63-IDKUNDRF                          
068400     MOVE SEQF-IDPURAD           TO E63-IDRADNR-KO                        
068500     MOVE SEQF-KVLEVART          TO E63-KVLEVART                          
068600                                                                          
068700     WRITE E63-AREA  FROM E63-W479063                                     
068800                                                                          
068900     MOVE 'W47963'     TO POSTSUM-FDNAMN                                  
069000     MOVE 'W47960D3'   TO POSTSUM-DDNAMN2                                 
069100     MOVE 'E63'        TO POSTSUM-TRANSTYP                                
069200                                                                          
069300     CALL POSTSUM USING POSTSUM-PARM.                                     
069400     EJECT                                                                
069500                                                                          
069600 WDE4F-SKRIV-E64               SECTION.                                   
069700     IF FAKT-DATUM-E64(1:4) = DAGENS-DATUM                                
069800     OR FAKT-DATUM-E64      = FORRA-VECKA-DATUM                           
069900       MOVE 'E64'                  TO E64-IDPTYP                          
070000       MOVE SEQF-IDDISTR           TO E64-IDDISTR                         
070100       MOVE SEQF-IDKUNDNR          TO E64-IDKUNDNR                        
070200       MOVE SEQF-IDKUNDRF          TO E64-IDKUNDRF                        
070300       MOVE SEQF-IDPURAD           TO E64-IDRADNR-KO                      
070400       MOVE SEQF-KVLEVART          TO E64-KVLEVART2                       
070500                                                                          
070600       MOVE SEQF-IDDISTR           TO W-IDDISTR-WDB2-MIN                  
070700                                      W-IDDISTR-WDB2-MAX                  
070800       MOVE SEQF-IDKUNDNR          TO W-IDKUNDNR-WDB2-MIN                 
070900       PERFORM IMS-GET-GMTA01-FOERSTA                                     
071000                                                                          
071100       MOVE 57                     TO E64-IDFTG                           
071200       MOVE GMT-IDDC-RET           TO E64-IDDC-RET                        
071300                                                                          
071400       WRITE E64-AREA  FROM E64-W479064                                   
071500                                                                          
071600       MOVE 'W47964'     TO POSTSUM-FDNAMN                                
071700       MOVE 'W47960D4'   TO POSTSUM-DDNAMN2                               
071800       MOVE 'E64'        TO POSTSUM-TRANSTYP                              
071900                                                                          
072000       CALL POSTSUM USING POSTSUM-PARM                                    
072100     END-IF.                                                              
072200     EJECT                                                                
072300                                                                          
072400 Z-FINIT  SECTION.                                                        
072500     CLOSE W47960                                                         
072600           W47961                                                         
072700           W47963                                                         
072800           W47964                                                         
072900           W47965                                                         
073000           W47966                                                         
073100                                                                          
073200     MOVE 'S'          TO POSTSUM-OPKOD                                   
073300     CALL POSTSUM USING POSTSUM-PARM.                                     
073400     EJECT                                                                
073500                                                                          
073600 S01-SKRIV-E60                 SECTION.                                   
073700     WRITE E60-AREA  FROM E60-W479060                                     
073800                                                                          
073900     MOVE 'W47960'     TO POSTSUM-FDNAMN                                  
074000     MOVE 'W47960D1'   TO POSTSUM-DDNAMN2                                 
074100     MOVE 'E60'        TO POSTSUM-TRANSTYP                                
074200                                                                          
074300     CALL POSTSUM USING POSTSUM-PARM.                                     
074400     EJECT                                                                
074500                                                                          
074600 S02-SKRIV-E61                 SECTION.                                   
074700     WRITE E61-AREA  FROM E61-W479061                                     
074800                                                                          
074900     MOVE 'W47961'     TO POSTSUM-FDNAMN                                  
075000     MOVE 'W47960D2'   TO POSTSUM-DDNAMN2                                 
075100     MOVE 'E61'        TO POSTSUM-TRANSTYP                                
075200                                                                          
075300     CALL POSTSUM USING POSTSUM-PARM.                                     
075400     EJECT                                                                
075500                                                                          
075600 S03-CALL-RDATCONV-AAMMDD        SECTION.                                 
075700     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
075800                                                                          
075900     CALL WDATKONV USING DAT-KDDATFORM                                    
076000                         DAT-I-TIDATUM                                    
076100                         DAT-O-TIDATUM                                    
076200                         DAT-KDSVAR.                                      
076300     EJECT                                                                
076400                                                                          
076500 S04-SKRIV-W47966-AREA  SECTION.                                          
076600     MOVE 'A01'                   TO A01-IDPTYP                           
076700     MOVE VORD-IDDISTR            TO A01-IDDISTR                          
076800     MOVE VORD-IDKUNDNR           TO A01-IDKUNDNR                         
076900     MOVE VORD-IDPRODNR           TO A01-IDPRODNR                         
077000     MOVE VORD-IDDC               TO A01-IDDC                             
077100     MOVE VORD-KDFRAKT            TO A01-KDFRAKT                          
077200     MOVE VORD-KDORDKL            TO A01-KDORDKL                          
077300     MOVE VORD-DABEGPAC (3:6)     TO A01-TIBEGPAC                         
077400     MOVE ZERO                    TO A01-KDPERSON                         
077500     MOVE VORD-KDORDLOT           TO A01-KDORDLOT                         
077600     MOVE VORD-IDLOTNR            TO A01-IDLOTNR                          
077700     MOVE VORD-KVORDRAD           TO A01-KVORDRAD                         
077800     MOVE VORD-VLORDNTO           TO A01-VLORDNTO                         
077900     MOVE VORD-VKORDNTO           TO A01-VKORDNTO                         
078000     MOVE W-IDUSER                TO A01-IDUSER                           
078100                                                                          
078200     MOVE ZERO                    TO A01-IDKOLLI                          
078300                                     A01-IDARTNR                          
078400                                     A01-IDPURAD                          
078500                                     A01-TIORDREG                         
078600     MOVE SPACE                   TO A01-IDKUNDRF                         
078700                                     A01-BEVARREF                         
078800                                     A01-BEGMT                            
078900                                     A01-ADGMT                            
079000                                     A01-BEGMRK                           
079100                                     A01-BELAGINS-GRP                     
079200                                                                          
079300     WRITE W47966-AREA  FROM A01-W479A01                                  
079400                                                                          
079500     MOVE 'W47966'     TO POSTSUM-FDNAMN                                  
079600     MOVE 'W47960D6'   TO POSTSUM-DDNAMN2                                 
079700     MOVE 'A01'        TO POSTSUM-TRANSTYP                                
079800                                                                          
079900     CALL POSTSUM USING POSTSUM-PARM                                      
080000                                                                          
080100     MOVE JA                      TO FL-SKRIV-A11                         
080200     .                                                                    
080300     EJECT                                                                
080400*         * I M S  S E C T I O N                                          
080500                                                                          
080600 IMS-GET-WDE6             SECTION.                                        
080700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
080800     CALL CBLTDLI USING GN WDE6-PCB DLI-IO-AREA1                          
080900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUSKONTROLL.                                          
081100     SKIP3                                                                
081200                                                                          
081300 IMS-GU-WDE4F1                 SECTION.                                   
081400     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
081500                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
081600            DELIMITED BY SIZE INTO SSA1                                   
081700     MOVE '  GE' TO GODK-STATUSKODER                                      
081800     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
081900     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
082000     PERFORM IMS-STATUSKONTROLL                                           
082100     .                                                                    
082200     SKIP3                                                                
082300                                                                          
082400 IMS-GN-WDE4F1                 SECTION.                                   
082500     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
082600                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
082700            DELIMITED BY SIZE INTO SSA1                                   
082800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
082900     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-E4F1 SSA1                     
083000     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     EJECT                                                                
083400                                                                          
083500 IMS-GU-WDQ3DSEQ-ORQA01        SECTION.                                   
083600     STRING 'WLORQA01(WDQ3DSEQ=>' W-WDQ3DSEQ-MIN-X                        
083700                    '&WDQ3DSEQ=<' W-WDQ3DSEQ-MAX-X ')'                    
083800                     DELIMITED BY SIZE INTO SSA1                          
083900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
084000     CALL CBLTDLI USING GU ORQD-PCB DLI-IO-AREA2 SSA1                     
084100     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
084200     PERFORM IMS-STATUSKONTROLL                                           
084300     .                                                                    
084400     SKIP3                                                                
084500                                                                          
084600 IMS-GN-WDQ3DSEQ-ORQA01      SECTION.                                     
084700     STRING 'WLORQA01(WDQ3DSEQ=>' W-WDQ3DSEQ-MIN-X                        
084800                    '&WDQ3DSEQ=<' W-WDQ3DSEQ-MAX-X ')'                    
084900                     DELIMITED BY SIZE INTO SSA1                          
085000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
085100     CALL CBLTDLI USING GN ORQD-PCB DLI-IO-AREA2 SSA1                     
085200     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     SKIP3                                                                
085600                                                                          
085700 IMS-GET-GMTA01-FOERSTA         SECTION.                                  
085800     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
085900                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
086000            DELIMITED BY SIZE INTO SSA1                                   
086100                                                                          
086200     MOVE '    ' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1               
086400     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     EJECT                                                                
086800                                                                          
086900 IMS-GU-WDE201                 SECTION.                                   
087000     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
087100            DELIMITED BY SIZE INTO SSA1                                   
087200     MOVE '  GE ' TO GODK-STATUSKODER                                     
087300     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE201 SSA1                    
087400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     SKIP3                                                                
087800                                                                          
087900 IMS-STATUSKONTROLL       SECTION.                                        
088000     SET STATUS-IX TO 1                                                   
088100     SEARCH GODK-STATUS AT END CALL FELLOG                                
088200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089000     END-SEARCH.                                                          
