000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4066900.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/05/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BILL OF LADING. ADRESS, KOLLIKOD OCH VIKT KAN UPPDATERAS         
000900*        M.H.A DETTA PROGRAM                                              
001000*                                                                         
001100*        PROGRAMMET LÄSER/UPPDATERAR WLORQI (WDQ2)                        
001200*        PROGRAMMET LÄSER/UPPDATERAR WL4463 (WDGX)                        
001300*        PROGRAMMET LÄSER            WL1165 (WDGX)                        
001500*        PROGRAMMET LÄSER            WDB6                                 
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T669 W4T669U                                      
001900*        MID:         W4I66901                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O66901                                            
002300*                                                                         
002400* CHANGE LOG:                                                             
002500*                                                                         
002600*      14/05/05 - REDDY RAHUL     - MOVED THE BUSINESS LOGIC TO           
002700*                                   NEW PROGRAM W4066910.                 
002800*                                   ETRACKER 10228352 - TORONTO.          
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4066900'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
004900 77  MAX-KVRADER                 PIC S9(4)  VALUE +6    COMP SYNC.        
005000                                                                          
005100 01  ALL-SPACE.                                                           
005200     03  FILLER                  PIC X(80)  VALUE SPACE.                  
005300 01  ALL-PLUS.                                                            
005400     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
005500                                                                          
005600*    --- GENERELLA ARBETSFÄLT                                             
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '4669'.                
006200     88  GODK-MID                            VALUE '4661' '4662'          
006300                                                   '4663' '4664'          
006400                                                   '4665' '4666'          
006500                                                   '4667' '4668'          
006600                                                   '4669'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800*                                                                         
006900     EJECT                                                                
007000*      --- VALID IDDC CODES                                               
007100*                                                                         
007200*01    -COPY WWDC99                                                       
007300       EJECT                                                              
007400                                                                          
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007800     03  W4066910                PIC X(8)    VALUE 'W4066910'.            
007900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
008400*01 -COPY WL01MCNV                                                        
008500     SKIP3                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008900     SKIP3                                                                
009000*01 -COPY WMSGINIT                                                        
009100     EJECT                                                                
009200*    --- REQU AREA                                                        
009300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009400 01  REQU-AREA.                                                           
009500*    03 -COPY WZ01REQU                                                    
009600*    03 -COPY W40669I1                                                    
009700*    --- RESP AREA                                                        
009800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009900 01  RESP-AREA.                                                           
010000*    03 -COPY WZ01RESP                                                    
010100*    03 -COPY W40669O1                                                    
010200     EJECT                                                                
010300     SKIP3                                                                
010400     EJECT                                                                
010500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010600*                                                                         
010700 01  SPAR-AREA.                                                           
010800     03  SPAR-IDTRANS             PIC X(4)    VALUE '4669'.               
010900     03  FILLER           PIC X(16) VALUE 'ENTER-NYCKLAR'.                
011000     03  SPAR-KY4468-ENTER.                                               
011100         05  W-IDDISTR-ENTER      PIC S9(5)   VALUE ZERO  COMP-3.         
011200         05  W-IDKUNDNR-ENTER     PIC S9(7)   VALUE ZERO  COMP-3.         
011300         05  W-IDKUNDRF-ENTER     PIC X(10)   VALUE SPACE.                
011400         05  W-IDPRODNR-ENTER     PIC S9(7)   VALUE ZERO  COMP-3.         
011500         05  W-IDKOLLI-ENTER      PIC S9(5)   VALUE ZERO  COMP-3.         
011600     03  FILLER           PIC X(16) VALUE 'NEXT-NYCKLAR'.                 
011700     03  SPAR-KY4468-NEXT.                                                
011800         05  W-IDDISTR-NEXT       PIC S9(5)   VALUE ZERO  COMP-3.         
011900         05  W-IDKUNDNR-NEXT      PIC S9(7)   VALUE ZERO  COMP-3.         
012000         05  W-IDKUNDRF-NEXT      PIC X(10)   VALUE SPACE.                
012100         05  W-IDPRODNR-NEXT      PIC S9(7)   VALUE ZERO  COMP-3.         
012200         05  W-IDKOLLI-NEXT       PIC S9(5)   VALUE ZERO  COMP-3.         
012300     EJECT                                                                
012400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W4I66901                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W4O66901                                                 
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600*    --- STATUS-KOD FRÅN IMS                                              
014700 01  STATUS-WS                   PIC XX.                                  
014800     88  SEGMENT-FINNS                       VALUE '  '.                  
014900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015100     SKIP2                                                                
015200 01  GODK-STATUSKODER.                                                    
015300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900                                                                          
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200*01  -COPY W0009   -PRE MSG-                                              
016300 01  USEA-PCB                    PIC X.                                   
016400 01  ORQI-PCB                    PIC X.                                   
016500 01  4463-PCB                    PIC X.                                   
016600 01  1165-PCB                    PIC X.                                   
016800 01  WDB6-PCB                    PIC X.                                   
016900                                                                          
017000     EJECT                                                                
017100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORQI-PCB 4463-PCB             
017200     1165-PCB WDB6-PCB.                                                   
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORQI-PCB 4463-PCB             
017500     1165-PCB WDB6-PCB.                                                   
017600                                                                          
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FINNS                                                     
017900       PERFORM A-INIT                                                     
018000       PERFORM B-INIT-KEYS                                                
018100       PERFORM C-INIT-REQU                                                
018200       IF MFS-UPDATE                                                      
018300         SET REQU-UPDATE         TO TRUE                                  
018400         PERFORM E-SAMMA-SIDA                                             
018500       ELSE                                                               
018600         IF MFS-FIRST                                                     
018700           SET REQU-FIRST        TO TRUE                                  
018800           PERFORM MFS-RENSA-FAELT-IN                                     
018900         ELSE                                                             
019000           IF MFS-NEXT                                                    
019100             SET REQU-NEXT       TO TRUE                                  
019200             PERFORM D-NAESTA-SIDA                                        
019300           ELSE                                                           
019400             SET REQU-QUERY      TO TRUE                                  
019500             PERFORM E-SAMMA-SIDA                                         
019600           END-IF                                                         
019700         END-IF                                                           
019800       END-IF                                                             
019900       PERFORM F-CALL-BIZ-LOGIC-W4066910                                  
020000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O66901 + 4                      
020100       PERFORM IMS-INSERT-MSG                                             
020200     END-IF                                                               
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900                                                                          
021000     IF MSG-DUBBLA-TRANSKODER                                             
021100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66901                 
021200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021400     ELSE                                                                 
021500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66901                  
021600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021800     END-IF                                                               
021900                                                                          
022000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022300                                                                          
022400     MOVE LOW-VALUE TO MSG-AREA                                           
022500     MOVE 'W4O669N1' TO MFS-IDMOD                                         
022600     MOVE '4669' TO MOD-IDTRANS                                           
022700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
022800                                                                          
022900     IF EGEN-MID OR HELP-MID                                              
023000       CONTINUE                                                           
023100     ELSE                                                                 
023200       MOVE SPACE TO MFS-KDTRTYP                                          
023300       MOVE '7' TO MFS-IDPFK                                              
023400     END-IF                                                               
023500     PERFORM MFS-RENSA-FAELT-UT                                           
023600     .                                                                    
023700     EJECT                                                                
023800 B-INIT-KEYS SECTION.                                                     
023900                                                                          
024000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024100     MOVE '001'             TO MSGI-KDCALL                                
024200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024400     MOVE '4669'            TO MSGI-IDTRANS                               
024500     IF EGEN-MID                                                          
024600         MOVE MID-TISKEPPN-IN    TO MSGI-TISKEPPN                         
024700         MOVE MID-IDTRPTNR-IN    TO MSGI-IDTRPTNR                         
024800         MOVE MID-IDLBBET-IN     TO MSGI-IDLBBET                          
024900         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
025000         MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                         
025100     ELSE                                                                 
025200       IF MID-TISKEPPN-IN NUMERIC                                         
025300         MOVE MID-TISKEPPN-IN TO MSGI-TISKEPPN                            
025400       END-IF                                                             
025500       IF MID-IDTRPTNR-IN NUMERIC                                         
025600         MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                            
025700       END-IF                                                             
025800       IF MID-IDDISTR-IN NUMERIC                                          
025900         MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                             
026000       END-IF                                                             
026100       IF MID-IDKUNDNR-IN NUMERIC                                         
026200         MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                            
026300       END-IF                                                             
026400       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
026500     END-IF                                                               
026600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026700     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
026800     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
026900     MOVE '101'                  TO REQU-IDMSGVER                         
027000                                                                          
027100     MOVE MFS-RENSA-FAELT   TO MOD-TISKEPPN-IN                            
027200                               MOD-IDTRPTNR-IN                            
027300                               MOD-IDLBBET-IN                             
027400                               MOD-IDDISTR-IN                             
027500                               MOD-IDKUNDNR-IN                            
027600                                                                          
027700     IF MID-TISKEPPN-IN NOT = ALL '+'                                     
027800       MOVE '7'         TO MFS-IDPFK                                      
027900       MOVE SPACE       TO MFS-KDTRTYP                                    
028000     END-IF                                                               
028100                                                                          
028200     IF MID-IDTRPTNR-IN NOT = ALL '+'                                     
028300       MOVE '7'         TO MFS-IDPFK                                      
028400       MOVE SPACE       TO MFS-KDTRTYP                                    
028500     END-IF                                                               
028600                                                                          
028700     IF MID-IDLBBET-IN NOT = ALL '+'                                      
028800       MOVE '7'         TO MFS-IDPFK                                      
028900       MOVE SPACE       TO MFS-KDTRTYP                                    
029000     END-IF                                                               
029100                                                                          
029200     IF MID-IDDISTR-IN NOT = ALL '+'                                      
029300       MOVE '7'         TO MFS-IDPFK                                      
029400       MOVE SPACE       TO MFS-KDTRTYP                                    
029500     END-IF                                                               
029600                                                                          
029700     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
029800       MOVE '7'         TO MFS-IDPFK                                      
029900       MOVE SPACE       TO MFS-KDTRTYP                                    
030000     END-IF                                                               
030100                                                                          
030200     MOVE MSGI-TISKEPPN          TO REQU-TISKEPPN-KEY                     
030300     MOVE MSGI-IDTRPTNR          TO REQU-IDTRPTNR-KEY                     
030400     MOVE MSGI-IDLBBET           TO REQU-IDLBBET-KEY                      
030500     MOVE MSGI-IDDISTR           TO REQU-IDDISTR-KEY                      
030600     MOVE MSGI-IDKUNDNR          TO REQU-IDKUNDNR-KEY                     
030700     MOVE MSGI-IDDC              TO REQU-IDDC-KEY                         
030800                                                                          
030900     IF GODK-MID                                                          
031000       MOVE MSGI-TISKEPPN        TO MOD-TISKEPPN-UT                       
031100       INSPECT MOD-TISKEPPN-UT REPLACING LEADING ZERO BY SPACE            
031200                                                                          
031300       MOVE MSGI-IDTRPTNR        TO MOD-IDTRPTNR-UT                       
031400       INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE            
031500                                                                          
031600       MOVE MSGI-IDLBBET         TO MOD-IDLBBET-UT                        
031700       INSPECT MOD-IDLBBET-UT REPLACING LEADING ZERO BY SPACE             
031800                                                                          
031900       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
032000       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
032100                                                                          
032200       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
032300       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
032400                                                                          
032500       MOVE MSGI-IDDC            TO MOD-IDDC-UT                           
032600       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
032700     ELSE                                                                 
032800       MOVE MFS-RENSA-FAELT      TO MOD-TISKEPPN-UT                       
032900                                    MOD-IDTRPTNR-UT                       
033000                                    MOD-IDLBBET-UT                        
033100                                    MOD-IDDISTR-UT                        
033200                                    MOD-IDKUNDNR-UT                       
033300                                    MOD-IDDC-UT                           
033400     END-IF                                                               
033500                                                                          
033600     .                                                                    
033700     EJECT                                                                
033800 C-INIT-REQU SECTION.                                                     
033900                                                                          
034000     MOVE MAX-KVRADER            TO REQU-KVRADER                          
034100                                                                          
034200     IF MID-BEGMT-RAD1-IN = ALL '+'                                       
034300       MOVE ALL-PLUS             TO REQU-BEGMT-RAD1-UPD                   
034400     ELSE                                                                 
034500       MOVE MID-BEGMT-RAD1-IN    TO REQU-BEGMT-RAD1-UPD                   
034600     END-IF                                                               
034700     MOVE MID-BEGMT-RAD1-UT      TO REQU-BEGMT-RAD1-UT                    
034800     IF MID-BEGMT-RAD2-IN = ALL '+'                                       
034900       MOVE ALL-PLUS             TO REQU-BEGMT-RAD2-UPD                   
035000     ELSE                                                                 
035100       MOVE MID-BEGMT-RAD2-IN    TO REQU-BEGMT-RAD2-UPD                   
035200     END-IF                                                               
035300     MOVE MID-BEGMT-RAD2-UT      TO REQU-BEGMT-RAD2-UT                    
035400     IF MID-ADGMT-GATA-IN = ALL '+'                                       
035500       MOVE ALL-PLUS             TO REQU-ADGMT-GATA-UPD                   
035600     ELSE                                                                 
035700       MOVE MID-ADGMT-GATA-IN    TO REQU-ADGMT-GATA-UPD                   
035800     END-IF                                                               
035900     MOVE MID-ADGMT-GATA-UT      TO REQU-ADGMT-GATA-UT                    
036000     IF MID-ADGMT-PADR-IN = ALL '+'                                       
036100       MOVE ALL-PLUS             TO REQU-ADGMT-PADR-UPD                   
036200     ELSE                                                                 
036300       MOVE MID-ADGMT-PADR-IN    TO REQU-ADGMT-PADR-UPD                   
036400     END-IF                                                               
036500     MOVE MID-ADGMT-PADR-UT      TO REQU-ADGMT-PADR-UT                    
036600     IF MID-KDCMD-NY = ALL '+'                                            
036700       MOVE ALL-PLUS             TO REQU-KDCMD-UPD                        
036800     ELSE                                                                 
036900       MOVE MID-KDCMD-NY         TO REQU-KDCMD-UPD                        
037000     END-IF                                                               
037100     IF MID-IDORDNR7-NY = ALL '+'                                         
037200       MOVE ALL-PLUS             TO REQU-IDORDNR7-UPD                     
037300     ELSE                                                                 
037400       MOVE MID-IDORDNR7-NY      TO REQU-IDORDNR7-UPD                     
037500     END-IF                                                               
037600     IF MID-IDPRODNR-NY = ALL '+'                                         
037700       MOVE ALL-PLUS             TO REQU-IDPRODNR-UPD                     
037800     ELSE                                                                 
037900       MOVE MID-IDPRODNR-NY      TO REQU-IDPRODNR-UPD                     
038000     END-IF                                                               
038100     IF MID-IDKOLLI-NY = ALL '+'                                          
038200       MOVE ALL-PLUS             TO REQU-IDKOLLI-UPD                      
038300     ELSE                                                                 
038400       MOVE MID-IDKOLLI-NY       TO REQU-IDKOLLI-UPD                      
038500     END-IF                                                               
038600     IF MID-IDPSN-NY = ALL '+'                                            
038700       MOVE ALL-PLUS             TO REQU-IDPSN-UPD                        
038800     ELSE                                                                 
038900       MOVE MID-IDPSN-NY         TO REQU-IDPSN-UPD                        
039000     END-IF                                                               
039100     IF MID-KDORDKL-NY = ALL '+'                                          
039200       MOVE ALL-PLUS             TO REQU-KDORDKL-UPD                      
039300     ELSE                                                                 
039400       MOVE MID-KDORDKL-NY       TO REQU-KDORDKL-UPD                      
039500     END-IF                                                               
039600     IF MID-KDKOLLI-NY = ALL '+'                                          
039700       MOVE ALL-PLUS             TO REQU-KDKOLLI-UPD                      
039800     ELSE                                                                 
039900       MOVE MID-KDKOLLI-NY       TO REQU-KDKOLLI-UPD                      
040000     END-IF                                                               
040100     IF MID-VKORDBTO-KOLLI-NY = ALL '+'                                   
040200       MOVE ALL-PLUS             TO REQU-VKORDBTO-KOLLI-UPD               
040300     ELSE                                                                 
040400       MOVE MID-VKORDBTO-KOLLI-NY                                         
040500                                 TO REQU-VKORDBTO-KOLLI-UPD               
040600     END-IF                                                               
040700                                                                          
040800     PERFORM                                                              
040900     VARYING INDX FROM +1 BY +1                                           
041000       UNTIL INDX > MAX-INDX                                              
041100       IF MID-CMD (INDX) = ALL '+'                                        
041200         MOVE ALL-PLUS           TO REQU-CMD-LINE      (INDX)             
041300       ELSE                                                               
041400         MOVE MID-CMD (INDX)     TO REQU-CMD-LINE      (INDX)             
041500       END-IF                                                             
041600       MOVE MID-IDORDNR7 (INDX)  TO REQU-IDORDNR7-LINE (INDX)             
041700       MOVE MID-IDPRODNR (INDX)  TO REQU-IDPRODNR-LINE (INDX)             
041800       MOVE MID-IDKOLLI  (INDX)  TO REQU-IDKOLLI-LINE  (INDX)             
041900       MOVE MID-IDPSN    (INDX)  TO REQU-IDPSN-LINE    (INDX)             
042000       MOVE MID-KDORDKL  (INDX)  TO REQU-KDORDKL-LINE  (INDX)             
042100       MOVE MID-KDKOLLI  (INDX)  TO REQU-KDKOLLI-LINE  (INDX)             
042200       MOVE MID-VKORDBTO-KOLLI (INDX)                                     
042300                                 TO REQU-VKORDBTO-KOLLI-LINE(INDX)        
042400     END-PERFORM                                                          
042500     .                                                                    
042600     EJECT                                                                
042700 D-NAESTA-SIDA SECTION.                                                   
042800                                                                          
042900     IF SPAR-IDTRANS = '4669'                                             
043000       MOVE W-IDDISTR-NEXT   TO REQU-IDDISTR-START                        
043100       MOVE W-IDKUNDNR-NEXT  TO REQU-IDKUNDNR-START                       
043200       MOVE W-IDKUNDRF-NEXT  TO REQU-IDKUNDRF-START                       
043300       MOVE W-IDPRODNR-NEXT  TO REQU-IDPRODNR-START                       
043400       MOVE W-IDKOLLI-NEXT   TO REQU-IDKOLLI-START                        
043500       PERFORM MFS-RENSA-FAELT-IN                                         
043600     ELSE                                                                 
043700       PERFORM MFS-RENSA-FAELT-IN                                         
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 E-SAMMA-SIDA SECTION.                                                    
044200                                                                          
044300     IF SPAR-IDTRANS = '4669' OR '0551'                                   
044400       MOVE W-IDDISTR-ENTER  TO REQU-IDDISTR-START                        
044500       MOVE W-IDKUNDNR-ENTER TO REQU-IDKUNDNR-START                       
044600       MOVE W-IDKUNDRF-ENTER TO REQU-IDKUNDRF-START                       
044700       MOVE W-IDPRODNR-ENTER TO REQU-IDPRODNR-START                       
044800       MOVE W-IDKOLLI-ENTER  TO REQU-IDKOLLI-START                        
044900     ELSE                                                                 
045000       PERFORM MFS-RENSA-FAELT-IN                                         
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 F-CALL-BIZ-LOGIC-W4066910 SECTION.                                       
045500                                                                          
045600     CALL W4066910 USING REQU-AREA RESP-AREA MAX-KVRADER                  
045700                         ORQI-PCB  4463-PCB  1165-PCB                     
045800                         WDB6-PCB                                         
045900                                                                          
046000     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
046100        RESP-IDMSG-INFO  NOT = SPACE                                      
046200       PERFORM FA-SET-MSG-AND-HILIGHT                                     
046300     END-IF                                                               
046400     PERFORM FB-MOVE-RESP-TO-MOD                                          
046500                                                                          
046600* SAVE START AND NEXT KEYS IN PROFILE DB                                  
046700                                                                          
046800     MOVE RESP-IDDISTR-START     TO W-IDDISTR-ENTER                       
046900     MOVE RESP-IDKUNDNR-START    TO W-IDKUNDNR-ENTER                      
047000     MOVE RESP-IDKUNDRF-START    TO W-IDKUNDRF-ENTER                      
047100     MOVE RESP-IDPRODNR-START    TO W-IDPRODNR-ENTER                      
047200     MOVE RESP-IDKOLLI-START     TO W-IDKOLLI-ENTER                       
047300                                                                          
047400     MOVE RESP-IDDISTR-NEXT      TO W-IDDISTR-NEXT                        
047500     MOVE RESP-IDKUNDNR-NEXT     TO W-IDKUNDNR-NEXT                       
047600     MOVE RESP-IDKUNDRF-NEXT     TO W-IDKUNDRF-NEXT                       
047700     MOVE RESP-IDPRODNR-NEXT     TO W-IDPRODNR-NEXT                       
047800     MOVE RESP-IDKOLLI-NEXT      TO W-IDKOLLI-NEXT                        
047900                                                                          
048000     MOVE '002'                  TO MSGI-KDCALL                           
048100     MOVE '4669'                 TO SPAR-IDTRANS                          
048200     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
048300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048400     .                                                                    
048500     EJECT                                                                
048600 FA-SET-MSG-AND-HILIGHT SECTION.                                          
048700                                                                          
048800     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
048900     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
049000     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
049100                                                                          
049200     CALL WL01MCNV USING MCNV-AREA                                        
049300     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
049400     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
049500     .                                                                    
049600     EJECT                                                                
049700 FB-MOVE-RESP-TO-MOD SECTION.                                             
049800                                                                          
049900     IF RESP-BEGMT-RAD1-UPD = SPACE                                       
050000       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD1-IN                     
050100     ELSE                                                                 
050200       IF RESP-BEGMT-RAD1-UPD = ALL '+'                                   
050300         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD1-IN                     
050400       ELSE                                                               
050500         MOVE RESP-BEGMT-RAD1-UPD                                         
050600                                 TO MOD-BEGMT-RAD1-IN                     
050700       END-IF                                                             
050800     END-IF                                                               
050900                                                                          
051000     IF RESP-BEGMT-RAD1-UT = SPACE                                        
051100       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD1-UT                     
051200     ELSE                                                                 
051300       IF RESP-BEGMT-RAD1-UT = ALL '+'                                    
051400         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD1-UT                     
051500       ELSE                                                               
051600         MOVE RESP-BEGMT-RAD1-UT                                          
051700                                 TO MOD-BEGMT-RAD1-UT                     
051800       END-IF                                                             
051900     END-IF                                                               
052000                                                                          
052100     IF RESP-BEGMT-RAD2-UPD = SPACE                                       
052200       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD2-IN                     
052300     ELSE                                                                 
052400       IF RESP-BEGMT-RAD2-UPD = ALL '+'                                   
052500         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD2-IN                     
052600       ELSE                                                               
052700         MOVE RESP-BEGMT-RAD2-UPD                                         
052800                                 TO MOD-BEGMT-RAD2-IN                     
052900       END-IF                                                             
053000     END-IF                                                               
053100                                                                          
053200     IF RESP-BEGMT-RAD2-UT = SPACE                                        
053300       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD2-UT                     
053400     ELSE                                                                 
053500       IF RESP-BEGMT-RAD2-UT = ALL '+'                                    
053600         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD2-UT                     
053700       ELSE                                                               
053800         MOVE RESP-BEGMT-RAD2-UT                                          
053900                                 TO MOD-BEGMT-RAD2-UT                     
054000       END-IF                                                             
054100     END-IF                                                               
054200                                                                          
054300     IF RESP-ADGMT-GATA-UPD = SPACE                                       
054400       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-GATA-IN                     
054500     ELSE                                                                 
054600       IF RESP-ADGMT-GATA-UPD = ALL '+'                                   
054700         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-GATA-IN                     
054800       ELSE                                                               
054900         MOVE RESP-ADGMT-GATA-UPD                                         
055000                                 TO MOD-ADGMT-GATA-IN                     
055100       END-IF                                                             
055200     END-IF                                                               
055300                                                                          
055400     IF RESP-ADGMT-GATA-UT = SPACE                                        
055500       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-GATA-UT                     
055600     ELSE                                                                 
055700       IF RESP-ADGMT-GATA-UT = ALL '+'                                    
055800         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-GATA-UT                     
055900       ELSE                                                               
056000         MOVE RESP-ADGMT-GATA-UT                                          
056100                                 TO MOD-ADGMT-GATA-UT                     
056200       END-IF                                                             
056300     END-IF                                                               
056400                                                                          
056500     IF RESP-ADGMT-PADR-UPD = SPACE                                       
056600       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-PADR-IN                     
056700     ELSE                                                                 
056800       IF RESP-ADGMT-PADR-UPD = ALL '+'                                   
056900         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-PADR-IN                     
057000       ELSE                                                               
057100         MOVE RESP-ADGMT-PADR-UPD                                         
057200                                 TO MOD-ADGMT-PADR-IN                     
057300       END-IF                                                             
057400     END-IF                                                               
057500                                                                          
057600     IF RESP-ADGMT-PADR-UT = SPACE                                        
057700       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-PADR-UT                     
057800     ELSE                                                                 
057900       IF RESP-ADGMT-PADR-UT = ALL '+'                                    
058000         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-PADR-UT                     
058100       ELSE                                                               
058200         MOVE RESP-ADGMT-PADR-UT                                          
058300                                 TO MOD-ADGMT-PADR-UT                     
058400       END-IF                                                             
058500     END-IF                                                               
058600                                                                          
058700     MOVE RESP-KDCMD-UPD-ATTR    TO MOD-KDCMD-NY-ATTR                     
058800     IF RESP-KDCMD-UPD = SPACE                                            
058900       MOVE MFS-RENSA-FAELT      TO MOD-KDCMD-NY                          
059000     ELSE                                                                 
059100       IF RESP-KDCMD-UPD = ALL '+'                                        
059200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-NY                          
059300       ELSE                                                               
059400         MOVE RESP-KDCMD-UPD                                              
059500                                 TO MOD-KDCMD-NY                          
059600       END-IF                                                             
059700     END-IF                                                               
059800                                                                          
059900     MOVE RESP-IDORDNR7-UPD-ATTR TO MOD-IDORDNR7-NY-ATTR                  
060000     IF RESP-IDORDNR7-UPD = SPACE                                         
060100       MOVE MFS-RENSA-FAELT      TO MOD-IDORDNR7-NY                       
060200     ELSE                                                                 
060300       IF RESP-IDORDNR7-UPD = ALL '+'                                     
060400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDORDNR7-NY                       
060500       ELSE                                                               
060600         MOVE RESP-IDORDNR7-UPD                                           
060700                                 TO MOD-IDORDNR7-NY                       
060800       END-IF                                                             
060900     END-IF                                                               
061000                                                                          
061100     MOVE RESP-IDPRODNR-UPD-ATTR TO MOD-IDPRODNR-NY-ATTR                  
061200     IF RESP-IDPRODNR-UPD = SPACE                                         
061300       MOVE MFS-RENSA-FAELT      TO MOD-IDPRODNR-NY                       
061400     ELSE                                                                 
061500       IF RESP-IDPRODNR-UPD = ALL '+'                                     
061600         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRODNR-NY                       
061700       ELSE                                                               
061800         MOVE RESP-IDPRODNR-UPD                                           
061900                                 TO MOD-IDPRODNR-NY                       
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     MOVE RESP-IDKOLLI-UPD-ATTR  TO MOD-IDKOLLI-NY-ATTR                   
062400     IF RESP-IDKOLLI-UPD = SPACE                                          
062500       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-NY                        
062600     ELSE                                                                 
062700       IF RESP-IDKOLLI-UPD = ALL '+'                                      
062800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-NY                        
062900       ELSE                                                               
063000         MOVE RESP-IDKOLLI-UPD                                            
063100                                 TO MOD-IDKOLLI-NY                        
063200       END-IF                                                             
063300     END-IF                                                               
063400                                                                          
063500     MOVE RESP-IDPSN-UPD-ATTR    TO MOD-IDPSN-NY-ATTR                     
063600     IF RESP-IDPSN-UPD = SPACE                                            
063700       MOVE MFS-RENSA-FAELT      TO MOD-IDPSN-NY                          
063800     ELSE                                                                 
063900       IF RESP-IDPSN-UPD = ALL '+'                                        
064000         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPSN-NY                          
064100       ELSE                                                               
064200         MOVE RESP-IDPSN-UPD                                              
064300                                 TO MOD-IDPSN-NY                          
064400       END-IF                                                             
064500     END-IF                                                               
064600                                                                          
064700     MOVE RESP-KDORDKL-UPD-ATTR  TO MOD-KDORDKL-NY-ATTR                   
064800     IF RESP-KDORDKL-UPD = SPACE                                          
064900       MOVE MFS-RENSA-FAELT      TO MOD-KDORDKL-NY                        
065000     ELSE                                                                 
065100       IF RESP-KDORDKL-UPD = ALL '+'                                      
065200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDORDKL-NY                        
065300       ELSE                                                               
065400         MOVE RESP-KDORDKL-UPD                                            
065500                                 TO MOD-KDORDKL-NY                        
065600       END-IF                                                             
065700     END-IF                                                               
065800                                                                          
065900     MOVE RESP-KDKOLLI-UPD-ATTR  TO MOD-KDKOLLI-NY-ATTR                   
066000     IF RESP-KDKOLLI-UPD = SPACE                                          
066100       MOVE MFS-RENSA-FAELT      TO MOD-KDKOLLI-NY                        
066200     ELSE                                                                 
066300       IF RESP-KDKOLLI-UPD = ALL '+'                                      
066400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKOLLI-NY                        
066500       ELSE                                                               
066600         MOVE RESP-KDKOLLI-UPD                                            
066700                                 TO MOD-KDKOLLI-NY                        
066800       END-IF                                                             
066900     END-IF                                                               
067000                                                                          
067100     MOVE RESP-VKORDBTO-KOLLI-UPD-ATTR                                    
067200                                 TO MOD-VKORDBTO-KOLLI-NY-ATTR            
067300     IF RESP-VKORDBTO-KOLLI-UPD = SPACE                                   
067400       MOVE MFS-RENSA-FAELT      TO MOD-VKORDBTO-KOLLI-NY                 
067500     ELSE                                                                 
067600       IF RESP-VKORDBTO-KOLLI-UPD = ALL '+'                               
067700         MOVE MFS-ROER-EJ-FAELT  TO MOD-VKORDBTO-KOLLI-NY                 
067800       ELSE                                                               
067900         MOVE RESP-VKORDBTO-KOLLI-UPD                                     
068000                                 TO MOD-VKORDBTO-KOLLI-NY                 
068100       END-IF                                                             
068200     END-IF                                                               
068300                                                                          
068400     PERFORM                                                              
068500     VARYING INDX FROM +1 BY +1                                           
068600       UNTIL INDX > RESP-KVRADER                                          
068700                                                                          
068800       MOVE RESP-CMD-LINE-ATTR (INDX)                                     
068900                                 TO MOD-CMD-ATTR (INDX)                   
069000       IF RESP-CMD-LINE (INDX) = SPACE                                    
069100         MOVE MFS-RENSA-FAELT    TO MOD-CMD (INDX)                        
069200       ELSE                                                               
069300         IF RESP-CMD-LINE (INDX) = ALL '+'                                
069400           MOVE MFS-ROER-EJ-FAELT                                         
069500                                 TO MOD-CMD (INDX)                        
069600         ELSE                                                             
069700           MOVE RESP-CMD-LINE (INDX)                                      
069800                                 TO MOD-CMD (INDX)                        
069900         END-IF                                                           
070000       END-IF                                                             
070100                                                                          
070200       IF RESP-IDORDNR7-LINE (INDX) = SPACE                               
070300         MOVE MFS-RENSA-FAELT    TO MOD-IDORDNR7 (INDX)                   
070400       ELSE                                                               
070500         IF RESP-IDORDNR7-LINE (INDX) = ALL '+'                           
070600           MOVE MFS-ROER-EJ-FAELT                                         
070700                                 TO MOD-IDORDNR7 (INDX)                   
070800         ELSE                                                             
070900           MOVE RESP-IDORDNR7-LINE (INDX)                                 
071000                                 TO MOD-IDORDNR7 (INDX)                   
071100         END-IF                                                           
071200       END-IF                                                             
071300                                                                          
071400       IF RESP-IDPRODNR-LINE (INDX) = SPACE                               
071500         MOVE MFS-RENSA-FAELT    TO MOD-IDPRODNR (INDX)                   
071600       ELSE                                                               
071700         IF RESP-IDPRODNR-LINE (INDX) = ALL '+'                           
071800           MOVE MFS-ROER-EJ-FAELT                                         
071900                                 TO MOD-IDPRODNR (INDX)                   
072000         ELSE                                                             
072100           MOVE RESP-IDPRODNR-LINE (INDX)                                 
072200                                 TO MOD-IDPRODNR (INDX)                   
072300         END-IF                                                           
072400       END-IF                                                             
072500                                                                          
072600       IF RESP-IDKOLLI-LINE (INDX) = SPACE                                
072700         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI (INDX)                    
072800       ELSE                                                               
072900         IF RESP-IDKOLLI-LINE (INDX) = ALL '+'                            
073000           MOVE MFS-ROER-EJ-FAELT                                         
073100                                 TO MOD-IDKOLLI (INDX)                    
073200         ELSE                                                             
073300           MOVE RESP-IDKOLLI-LINE (INDX)                                  
073400                                 TO MOD-IDKOLLI (INDX)                    
073500         END-IF                                                           
073600       END-IF                                                             
073700                                                                          
073800       IF RESP-IDPSN-LINE (INDX) = SPACE                                  
073900         MOVE MFS-RENSA-FAELT    TO MOD-IDPSN (INDX)                      
074000       ELSE                                                               
074100         IF RESP-IDPSN-LINE (INDX) = ALL '+'                              
074200           MOVE MFS-ROER-EJ-FAELT                                         
074300                                 TO MOD-IDPSN (INDX)                      
074400         ELSE                                                             
074500           MOVE RESP-IDPSN-LINE (INDX)                                    
074600                                 TO MOD-IDPSN (INDX)                      
074700         END-IF                                                           
074800       END-IF                                                             
074900                                                                          
075000       IF RESP-KDORDKL-LINE (INDX) = SPACE                                
075100         MOVE MFS-RENSA-FAELT    TO MOD-KDORDKL (INDX)                    
075200       ELSE                                                               
075300         IF RESP-KDORDKL-LINE (INDX) = ALL '+'                            
075400           MOVE MFS-ROER-EJ-FAELT                                         
075500                                 TO MOD-KDORDKL (INDX)                    
075600         ELSE                                                             
075700           MOVE RESP-KDORDKL-LINE (INDX)                                  
075800                                 TO MOD-KDORDKL (INDX)                    
075900         END-IF                                                           
076000       END-IF                                                             
076100                                                                          
076200       IF RESP-KDKOLLI-LINE (INDX) = SPACE                                
076300         MOVE MFS-RENSA-FAELT    TO MOD-KDKOLLI (INDX)                    
076400       ELSE                                                               
076500         IF RESP-KDKOLLI-LINE (INDX) = ALL '+'                            
076600           MOVE MFS-ROER-EJ-FAELT                                         
076700                                 TO MOD-KDKOLLI (INDX)                    
076800         ELSE                                                             
076900           MOVE RESP-KDKOLLI-LINE (INDX)                                  
077000                                 TO MOD-KDKOLLI (INDX)                    
077100         END-IF                                                           
077200       END-IF                                                             
077300                                                                          
077400       MOVE RESP-VKORDBTO-KOLLI-LINE-ATTR (INDX)                          
077500                                 TO MOD-VKORDBTO-KOLLI-ATTR (INDX)        
077600       IF RESP-VKORDBTO-KOLLI-LINE (INDX) = SPACE                         
077700         MOVE MFS-RENSA-FAELT    TO MOD-VKORDBTO-KOLLI (INDX)             
077800       ELSE                                                               
077900         IF RESP-VKORDBTO-KOLLI-LINE (INDX) = ALL '+'                     
078000           MOVE MFS-ROER-EJ-FAELT                                         
078100                                 TO MOD-VKORDBTO-KOLLI (INDX)             
078200         ELSE                                                             
078300           MOVE RESP-VKORDBTO-KOLLI-LINE (INDX)                           
078400                                 TO MOD-VKORDBTO-KOLLI (INDX)             
078500         END-IF                                                           
078600       END-IF                                                             
078700                                                                          
078800     END-PERFORM                                                          
078900                                                                          
079000     PERFORM                                                              
079100     VARYING INDX FROM INDX BY +1                                         
079200       UNTIL INDX > MAX-INDX                                              
079300       MOVE MFS-RENSA-FAELT      TO MOD-CMD (INDX)                        
079400                                    MOD-IDORDNR7 (INDX)                   
079500                                    MOD-IDPRODNR (INDX)                   
079600                                    MOD-IDKOLLI (INDX)                    
079700                                    MOD-IDPSN (INDX)                      
079800                                    MOD-KDORDKL (INDX)                    
079900                                    MOD-KDKOLLI (INDX)                    
080000                                    MOD-VKORDBTO-KOLLI (INDX)             
080100     END-PERFORM                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 MFS-RENSA-FAELT-UT SECTION.                                              
080500                                                                          
080600*    --- ALLA UTDATA-FÄLT                                                 
080700*    --- INKL. BLÄDDRINGSNYCKLAR                                          
080800     MOVE +1 TO INDX                                                      
080900     PERFORM UNTIL INDX > MAX-INDX                                        
081000       MOVE MFS-RENSA-FAELT      TO MOD-CMD (INDX)                        
081100                                    MOD-IDORDNR7 (INDX)                   
081200                                    MOD-IDPRODNR (INDX)                   
081300                                    MOD-IDKOLLI (INDX)                    
081400                                    MOD-IDPSN (INDX)                      
081500                                    MOD-KDORDKL (INDX)                    
081600                                    MOD-KDKOLLI (INDX)                    
081700                                    MOD-VKORDBTO-KOLLI (INDX)             
081800     ADD +1 TO INDX                                                       
081900     END-PERFORM                                                          
082000     MOVE MFS-RENSA-FAELT        TO MOD-BEGMT-RAD1-UT                     
082100                                    MOD-BEGMT-RAD2-UT                     
082200                                    MOD-ADGMT-GATA-UT                     
082300                                    MOD-ADGMT-PADR-UT                     
082400                                    MOD-KDCMD-NY                          
082500                                    MOD-IDORDNR7-NY                       
082600                                    MOD-IDPRODNR-NY                       
082700                                    MOD-IDKOLLI-NY                        
082800                                    MOD-KDORDKL-NY                        
082900                                    MOD-KDKOLLI-NY                        
083000                                    MOD-VKORDBTO-KOLLI-NY                 
083100     .                                                                    
083200     SKIP3                                                                
083300 MFS-RENSA-FAELT-IN SECTION.                                              
083400                                                                          
083500*    --- ALLA INDATA-FÄLT                                                 
083600     MOVE MFS-RENSA-FAELT        TO MOD-TISKEPPN-IN                       
083700                                    MOD-IDTRPTNR-IN                       
083800                                    MOD-IDLBBET-IN                        
083900                                    MOD-IDDISTR-IN                        
084000                                    MOD-IDKUNDNR-IN                       
084100                                    MOD-BEGMT-RAD1-IN                     
084200                                    MOD-BEGMT-RAD2-IN                     
084300                                    MOD-ADGMT-GATA-IN                     
084400                                    MOD-ADGMT-PADR-IN                     
084500                                    MOD-KDCMD-NY                          
084600                                    MOD-IDORDNR7-NY                       
084700                                    MOD-IDPRODNR-NY                       
084800                                    MOD-IDKOLLI-NY                        
084900                                    MOD-IDPSN-NY                          
085000                                    MOD-KDKOLLI-NY                        
085100                                    MOD-KDORDKL-NY                        
085200                                    MOD-VKORDBTO-KOLLI-NY                 
085300     .                                                                    
085400     EJECT                                                                
085500 MFS-FORM-ATTR SECTION.                                                   
085600                                                                          
085700*    --- ALLA INDATA-FÄLT                                                 
085800     MOVE MFS-FORMATETS-ATTR     TO MOD-KDCMD-NY-ATTR                     
085900                                    MOD-IDORDNR7-NY-ATTR                  
086000                                    MOD-IDPRODNR-NY-ATTR                  
086100                                    MOD-IDKOLLI-NY-ATTR                   
086200                                    MOD-KDKOLLI-NY-ATTR                   
086300                                    MOD-KDORDKL-NY-ATTR                   
086400                                    MOD-VKORDBTO-KOLLI-NY-ATTR            
086500     MOVE +1 TO INDX                                                      
086600     PERFORM UNTIL INDX > MAX-INDX                                        
086700      MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (INDX)                      
086800                                 MOD-VKORDBTO-KOLLI-ATTR(INDX)            
086900     ADD +1 TO INDX                                                       
087000     END-PERFORM                                                          
087100     .                                                                    
087200     SKIP2                                                                
087300                                                                          
087400* --- IMS SEKTIONER ---                                                   
087500     SKIP3                                                                
087600 IMS-GET-MSG SECTION.                                                     
087700                                                                          
087800     MOVE '  QC' TO GODK-STATUSKODER                                      
087900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088100     PERFORM IMS-STATUSKONTROLL                                           
088200     .                                                                    
088300     SKIP3                                                                
088400 IMS-INSERT-MSG SECTION.                                                  
088500                                                                          
088600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088700     MOVE SPACE TO GODK-STATUSKODER                                       
088800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     .                                                                    
089200     EJECT                                                                
089300 IMS-STATUSKONTROLL SECTION.                                              
089400                                                                          
089500     SET STATUS-IX TO 1                                                   
089600     SEARCH GODK-STATUS                                                   
089700       AT END                                                             
089800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
089900         DELIMITED BY SIZE INTO FELTEXT                                   
090000         CALL FELLOG                                                      
090100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
090200         CONTINUE                                                         
090300     END-SEARCH                                                           
090400     .                                                                    
