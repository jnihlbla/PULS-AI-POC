000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2722000.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   94/12/19.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SKAPAR EN FIL FÖR ATT I EFTERFÖLJANDE PGM                        
001100*        UPPDATERA  FÖRÄNDRAD PROGNOS, BESTÄLLNINGSPUNKT,                 
001200*        BESTÄLLNINGSKVANTITET OCH ÖVERLAGERPUNKT                         
001300*                                                                         
001400*        SKAPAR EN FIL FÖR UPPDATERING (W27220)                           
001500*                                                                         
001600*        THE PROGRAM UPDATES   WDK7                                       
001700*                                                                         
001800*    ABENDCODES:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003100     SELECT W27220                     ASSIGN TO W27220D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700     SKIP3                                                                
003800 FD  W27220                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27220 -PRE UT-     -L.                                   
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600*    -COPY WY2000W1                                                       
004700     SKIP3                                                                
004800 77  IDPGM                       PIC X(8)    VALUE 'W2722000'.            
004900 77  YES                         PIC X       VALUE 'Y'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100 77  DEFINITIV                   PIC S9      VALUE +1  COMP-3.            
005200                                                                          
005300 01  ARBETSFALT.                                                          
005400     03 IX                       PIC 9(2)    VALUE ZERO.                  
005500     03 IX-2                     PIC 9(2)    VALUE ZERO.                  
005600     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005700     03  WS-PRIS-K6              PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005800                                                                          
005900 01  ERRTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES TODAYS-DATE.                                        
006500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006700     03  TODAYS-DATE-DAY         PIC 9(2).                                
006800*                                                                         
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400*                                                                         
007500 01  DAGENS-DATUM2               PIC 9(8)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM2.                                      
007700     03  DAGENS-DATUM2-SEKEL      PIC 9(2).                               
007800     03  DAGENS-DATUM2-AAR        PIC 9(2).                               
007900     03  DAGENS-DATUM2-MAANAD     PIC 9(2).                               
008000     03  DAGENS-DATUM2-DAG        PIC 9(2).                               
008100*                                                                         
008200     EJECT                                                                
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDC99                                                       
008600       EJECT                                                              
008700*01    -COPY WWDCKONS                                                     
008800       EJECT                                                              
008900 01  GENERAL-SUBPROGRAM.                                                  
009000*                                                                         
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009500     03  W272REFL                PIC X(8)    VALUE 'W272REFL'.            
009600     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
009700     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL DATKORT                                          
010400*                                                                         
010500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27220'.              
010600     SKIP2                                                                
010700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010800     SKIP2                                                                
010900*01  -COPY WDATKORT                                                       
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL W272REFL                                         
011200*                                                                         
011300*01  -COPY W272REFL                                                       
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL W271UTIL                                         
011600*                                                                         
011700*01  -COPY W271UTIL                                                       
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL W272UTUP                                         
012000*                                                                         
012100*01  -COPY W272UTUP                                                       
012200     EJECT                                                                
012300 01  UT-AREA-START               PIC X(24)   VALUE                        
012400                                             'UT-AREA-START'.             
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W27220     -PRE UT-                                       
012800*                                                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  KEYS-TILL-DLI.                                                       
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013500     03  W-IDDC-X.                                                        
013600         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
013700     03  W-IDLAND-X.                                                      
013800         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
013900     03  W-IDLEVNR-X.                                                     
014000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
014100     03  W-KDSEGKEY-X.                                                    
014200         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
014300     SKIP2                                                                
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FOUND                       VALUE '  '.                  
014700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
015000     88  IMS-NOT-OK                          VALUE 'XD'.                  
015100     SKIP2                                                                
015200 01  GOOD-STATUSCODES.                                                    
015300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500 01  SSA1                        PIC X(128).                              
015600 01  SSA2                        PIC X(128).                              
015700     EJECT                                                                
015800*    --- IMS FUNCTION CODES                                               
015900*01  -COPY W0003                                                          
016000     EJECT                                                                
016100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
016200 01  DLI-IO-WDK601.                                                       
016300*    03  -COPY WDK601                                                     
016400     EJECT                                                                
016500                                                                          
016600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
016700 01  DLI-IO-WDK611.                                                       
016800*    03  -COPY WDK611                                                     
016900     EJECT                                                                
017000                                                                          
017100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
017200 01  DLI-IO-WDK629.                                                       
017300*    03  -COPY WDK629                                                     
017400     EJECT                                                                
017500                                                                          
017600                                                                          
017700 LINKAGE SECTION.                                                         
017800                                                                          
017900*01  -COPY W0009   -PRE MSG-                                              
018000     EJECT                                                                
018100*01  -COPY W0008  -PRE WDK6-                                              
018200     05  FILLER                          PIC X.                           
018300     EJECT                                                                
018400*01  -COPY W0008  -PRE REFL-2501-                                         
018500     05  FILLER                          PIC X.                           
018600     EJECT                                                                
018700*01  -COPY W0008  -PRE REFL-WDB6-                                         
018800     05  FILLER                          PIC X.                           
018900     EJECT                                                                
019000 01  UTIL-WDK6-PCB                       PIC X.                           
019100 01  UTIL-WDK7-PCB                       PIC X.                           
019200 01  UTIL-WDB6-PCB                       PIC X.                           
019300     EJECT                                                                
019400*****W272UTUP**********                                                   
019500 01  U2-WDK6-PCB                         PIC X.                           
019600 01  U2-WDB6-PCB                         PIC X.                           
019700 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
019800 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
019900 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
020000 01  U2-PBTO-W222-R1-2501-PCB            PIC X.                           
020100 01  U2-PBTO-W222-R1-WDB6R-PCB           PIC X.                           
020200 01  U2-PBTO-W222-R1-WDK7R-PCB           PIC X.                           
020300 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
020400 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
020500 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
020600 01  U2-PBTO-W222-R1-UTIL-WDK6-PCB       PIC X.                           
020700 01  U2-PBTO-W222-R1-UTIL-WDK7-PCB       PIC X.                           
020800 01  U2-PBTO-W222-R1-UTIL-WDB6-PCB       PIC X.                           
020900 01  U2-PBTO-W222-U1-WDK7-PCB            PIC X.                           
021000 01  U2-PBTO-W222-U1-WDB6-PCB            PIC X.                           
021100 01  U2-PBTO-W222-U1-UTIL-WDK6-PCB       PIC X.                           
021200 01  U2-PBTO-W222-U1-UTIL-WDK7-PCB       PIC X.                           
021300 01  U2-PBTO-W222-U1-UTIL-WDB6-PCB       PIC X.                           
021400 01  U2-REFL2-2501-PCB                   PIC X.                           
021500 01  U2-REFL2-WDB6-PCB                   PIC X.                           
021600 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
021700 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
021800 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
021900 01  U2-W222-WDK6-PCB                    PIC X.                           
022000 01  U2-W222-WDK7-PCB                    PIC X.                           
022100 01  U2-W222-ARTM-PCB                    PIC X.                           
022200 01  U2-W222-2501-PCB                    PIC X.                           
022300 01  U2-W222-WDB6R-PCB                   PIC X.                           
022400 01  U2-W222-WDK7R-PCB                   PIC X.                           
022500 01  U2-W222-WDB6-PCB                    PIC X.                           
022600 01  U2-W222-WDD7-PCB                    PIC X.                           
022700 01  U2-W222-WDK7E-PCB                   PIC X.                           
022800 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
022900 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
023000 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
023100 01  U2-W222-U1-WDK7-PCB                 PIC X.                           
023200 01  U2-W222-U1-WDB6-PCB                 PIC X.                           
023300 01  U2-W222-U1-UTIL-WDK6-PCB            PIC X.                           
023400 01  U2-W222-U1-UTIL-WDK7-PCB            PIC X.                           
023500 01  U2-W222-U1-UTIL-WDB6-PCB            PIC X.                           
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING                                                
023800                           MSG-PCB  WDK6-PCB                              
023900                           REFL-2501-PCB                                  
024000                           REFL-WDB6-PCB                                  
024100                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
024200                           UTIL-WDB6-PCB                                  
024300                           U2-WDK6-PCB                                    
024400                           U2-WDB6-PCB                                    
024500                           U2-PBTO-W222-WDK6-PCB                          
024600                           U2-PBTO-W222-WDK7-PCB                          
024700                           U2-PBTO-W222-ARTM-PCB                          
024800                           U2-PBTO-W222-R1-2501-PCB                       
024900                           U2-PBTO-W222-R1-WDB6R-PCB                      
025000                           U2-PBTO-W222-R1-WDK7R-PCB                      
025100                           U2-PBTO-W222-WDB6-PCB                          
025200                           U2-PBTO-W222-WDD7-PCB                          
025300                           U2-PBTO-W222-WDK7E-PCB                         
025400                           U2-PBTO-W222-R1-UTIL-WDK6-PCB                  
025500                           U2-PBTO-W222-R1-UTIL-WDK7-PCB                  
025600                           U2-PBTO-W222-R1-UTIL-WDB6-PCB                  
025700                           U2-PBTO-W222-U1-WDK7-PCB                       
025800                           U2-PBTO-W222-U1-WDB6-PCB                       
025900                           U2-PBTO-W222-U1-UTIL-WDK6-PCB                  
026000                           U2-PBTO-W222-U1-UTIL-WDK7-PCB                  
026100                           U2-PBTO-W222-U1-UTIL-WDB6-PCB                  
026200                           U2-REFL2-2501-PCB                              
026300                           U2-REFL2-WDB6-PCB                              
026400                           U2-REFL2-UTIL-WDK6-PCB                         
026500                           U2-REFL2-UTIL-WDK7-PCB                         
026600                           U2-REFL2-UTIL-WDB6-PCB                         
026700                           U2-W222-WDK6-PCB                               
026800                           U2-W222-WDK7-PCB                               
026900                           U2-W222-ARTM-PCB                               
027000                           U2-W222-2501-PCB                               
027100                           U2-W222-WDB6R-PCB                              
027200                           U2-W222-WDK7R-PCB                              
027300                           U2-W222-WDB6-PCB                               
027400                           U2-W222-WDD7-PCB                               
027500                           U2-W222-WDK7E-PCB                              
027600                           U2-W222-UTIL-WDK6-PCB                          
027700                           U2-W222-UTIL-WDK7-PCB                          
027800                           U2-W222-UTIL-WDB6-PCB                          
027900                           U2-W222-U1-WDK7-PCB                            
028000                           U2-W222-U1-WDB6-PCB                            
028100                           U2-W222-U1-UTIL-WDK6-PCB                       
028200                           U2-W222-U1-UTIL-WDK7-PCB                       
028300                           U2-W222-U1-UTIL-WDB6-PCB.                      
028400     ENTRY 'DLITCBL' USING                                                
028500                           MSG-PCB  WDK6-PCB                              
028600                           REFL-2501-PCB                                  
028700                           REFL-WDB6-PCB                                  
028800                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
028900                           UTIL-WDB6-PCB                                  
029000                           U2-WDK6-PCB                                    
029100                           U2-WDB6-PCB                                    
029200                           U2-PBTO-W222-WDK6-PCB                          
029300                           U2-PBTO-W222-WDK7-PCB                          
029400                           U2-PBTO-W222-ARTM-PCB                          
029500                           U2-PBTO-W222-R1-2501-PCB                       
029600                           U2-PBTO-W222-R1-WDB6R-PCB                      
029700                           U2-PBTO-W222-R1-WDK7R-PCB                      
029800                           U2-PBTO-W222-WDB6-PCB                          
029900                           U2-PBTO-W222-WDD7-PCB                          
030000                           U2-PBTO-W222-WDK7E-PCB                         
030100                           U2-PBTO-W222-R1-UTIL-WDK6-PCB                  
030200                           U2-PBTO-W222-R1-UTIL-WDK7-PCB                  
030300                           U2-PBTO-W222-R1-UTIL-WDB6-PCB                  
030400                           U2-PBTO-W222-U1-WDK7-PCB                       
030500                           U2-PBTO-W222-U1-WDB6-PCB                       
030600                           U2-PBTO-W222-U1-UTIL-WDK6-PCB                  
030700                           U2-PBTO-W222-U1-UTIL-WDK7-PCB                  
030800                           U2-PBTO-W222-U1-UTIL-WDB6-PCB                  
030900                           U2-REFL2-2501-PCB                              
031000                           U2-REFL2-WDB6-PCB                              
031100                           U2-REFL2-UTIL-WDK6-PCB                         
031200                           U2-REFL2-UTIL-WDK7-PCB                         
031300                           U2-REFL2-UTIL-WDB6-PCB                         
031400                           U2-W222-WDK6-PCB                               
031500                           U2-W222-WDK7-PCB                               
031600                           U2-W222-ARTM-PCB                               
031700                           U2-W222-2501-PCB                               
031800                           U2-W222-WDB6R-PCB                              
031900                           U2-W222-WDK7R-PCB                              
032000                           U2-W222-WDB6-PCB                               
032100                           U2-W222-WDD7-PCB                               
032200                           U2-W222-WDK7E-PCB                              
032300                           U2-W222-UTIL-WDK6-PCB                          
032400                           U2-W222-UTIL-WDK7-PCB                          
032500                           U2-W222-UTIL-WDB6-PCB                          
032600                           U2-W222-U1-WDK7-PCB                            
032700                           U2-W222-U1-WDB6-PCB                            
032800                           U2-W222-U1-UTIL-WDK6-PCB                       
032900                           U2-W222-U1-UTIL-WDK7-PCB                       
033000                           U2-W222-U1-UTIL-WDB6-PCB.                      
033100                                                                          
033200     PERFORM A-INIT                                                       
033300     PERFORM IMS-GN-WDK629                                                
033400     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
033500       IF CREF-KDREFSTA = 'A'                                             
033600         PERFORM IMS-GNP-WDK611                                           
033700         PERFORM IMS-GNP-WDK601                                           
033800         INITIALIZE REFL-W272REFL                                         
033900                                                                          
034000         MOVE '11'         TO WS-IDDC                                     
034100                                REFL-IDDC                                 
034200                                W-IDDC                                    
034300         MOVE ART-IDARTNR    TO REFL-IDARTNR                              
034400                                W-IDARTNR                                 
034500         MOVE CREF-IDDC-REF  TO REFL-IDDC-REF                             
034600         MOVE CREF-IDREFTAB  TO REFL-IDREFTAB                             
034700         MOVE CREF-FLREFBEO  TO REFL-FLREFBEO                             
034800         MOVE CREF-FLWILSON  TO REFL-FLWILSON                             
034900         MOVE CLAG-PRARTSTD  TO REFL-PRARTBES                             
035000         MOVE CREF-FLFLYG    TO REFL-FLFLYG                               
035100                                                                          
035200         MOVE CREF-TIREFPAF  TO TMP1-YYMMDD                               
035300         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
035400         PERFORM WY2000P1                                                 
035500         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
035600*                                                                         
035700*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
035800*-----     FÖR CDC ANVÄNDS CLAG-KVQ SOM KVREFBER                          
035900           MOVE CLAG-KVQ      TO REFL-IN-KVREFBER                         
036000         ELSE                                                             
036100           MOVE +0          TO REFL-IN-KVREFBER                           
036200         END-IF                                                           
036300                                                                          
036400         MOVE CREF-TIREFPKT  TO TMP1-YYMMDD                               
036500         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
036600         PERFORM WY2000P1                                                 
036700         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
036800*                                                                         
036900*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
037000*                                                                         
037100           MOVE CREF-KVREFPKT TO REFL-IN-KVREFPKT                         
037200         ELSE                                                             
037300           MOVE +0        TO REFL-IN-KVREFPKT                             
037400         END-IF                                                           
037500                                                                          
037600         MOVE 2                 TO UTUP-KDCALL                            
037700         MOVE W-IDARTNR         TO UTUP-IDARTNR                           
037800         MOVE W-IDDC            TO UTUP-IDDC                              
037900         IF CREF-IDDC-REF = SPACE                                         
038000           CALL FELLOG                                                    
038100         ELSE                                                             
038200           MOVE CREF-IDDC-REF TO UTUP-IDDC-REF                            
038300         END-IF                                                           
038400                                                                          
038500         CALL W272UTUP USING UTUP-W272UTUP                                
038600                           U2-WDK6-PCB                                    
038700                           U2-WDB6-PCB                                    
038800                           U2-PBTO-W222-WDK6-PCB                          
038900                           U2-PBTO-W222-WDK7-PCB                          
039000                           U2-PBTO-W222-ARTM-PCB                          
039100                           U2-PBTO-W222-R1-2501-PCB                       
039200                           U2-PBTO-W222-R1-WDB6R-PCB                      
039300                           U2-PBTO-W222-R1-WDK7R-PCB                      
039400                           U2-PBTO-W222-WDB6-PCB                          
039500                           U2-PBTO-W222-WDD7-PCB                          
039600                           U2-PBTO-W222-WDK7E-PCB                         
039700                           U2-PBTO-W222-R1-UTIL-WDK6-PCB                  
039800                           U2-PBTO-W222-R1-UTIL-WDK7-PCB                  
039900                           U2-PBTO-W222-R1-UTIL-WDB6-PCB                  
040000                           U2-PBTO-W222-U1-WDK7-PCB                       
040100                           U2-PBTO-W222-U1-WDB6-PCB                       
040200                           U2-PBTO-W222-U1-UTIL-WDK6-PCB                  
040300                           U2-PBTO-W222-U1-UTIL-WDK7-PCB                  
040400                           U2-PBTO-W222-U1-UTIL-WDB6-PCB                  
040500                           U2-REFL2-2501-PCB                              
040600                           U2-REFL2-WDB6-PCB                              
040700                           U2-REFL2-UTIL-WDK6-PCB                         
040800                           U2-REFL2-UTIL-WDK7-PCB                         
040900                           U2-REFL2-UTIL-WDB6-PCB                         
041000                           U2-W222-WDK6-PCB                               
041100                           U2-W222-WDK7-PCB                               
041200                           U2-W222-ARTM-PCB                               
041300                           U2-W222-2501-PCB                               
041400                           U2-W222-WDB6R-PCB                              
041500                           U2-W222-WDK7R-PCB                              
041600                           U2-W222-WDB6-PCB                               
041700                           U2-W222-WDD7-PCB                               
041800                           U2-W222-WDK7E-PCB                              
041900                           U2-W222-UTIL-WDK6-PCB                          
042000                           U2-W222-UTIL-WDK7-PCB                          
042100                           U2-W222-UTIL-WDB6-PCB                          
042200                           U2-W222-U1-WDK7-PCB                            
042300                           U2-W222-U1-WDB6-PCB                            
042400                           U2-W222-U1-UTIL-WDK6-PCB                       
042500                           U2-W222-U1-UTIL-WDK7-PCB                       
042600                           U2-W222-U1-UTIL-WDB6-PCB                       
042700         IF UTUP-KDSVAR-OK                                                
042800            MOVE UTUP-LEADTID-BEHOV  TO REFL-IN-LEADTID-BEHOV             
042900         ELSE                                                             
043000            DISPLAY 'W272UTUP-ERROR :' UTUP-TEXT                          
043100            CALL FELLOG                                                   
043200         END-IF                                                           
043300                                                                          
043400         CALL W272REFL USING REFL-W272REFL REFL-2501-PCB                  
043500                             REFL-WDB6-PCB                                
043600                             UTIL-WDK6-PCB                                
043700                             UTIL-WDK7-PCB                                
043800                             UTIL-WDB6-PCB                                
043900*********                                                                 
044000         IF CREF-KVREFOVL = REFL-KVREFOVL AND                             
044100            CREF-KVREFPKT = REFL-KVREFPKT AND                             
044200            CLAG-KVQ      = REFL-KVREFBER AND                             
044300            CLAG-KVSLAGER = REFL-KVSLAGER                                 
044400           CONTINUE                                                       
044500         ELSE                                                             
044600           PERFORM B-SKRIV-UPD-WDK6                                       
044700         END-IF                                                           
044800       END-IF                                                             
044900       PERFORM IMS-GN-WDK629                                              
045000     END-PERFORM                                                          
045100                                                                          
045200     PERFORM Z-FINIT                                                      
045300                                                                          
045400     MOVE ZERO TO RETURN-CODE                                             
045500     GOBACK                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 A-INIT SECTION.                                                          
045900                                                                          
046000     OPEN OUTPUT W27220                                                   
046100                                                                          
046200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
046300     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
046400                         DAGENS-DATUM2-AAR                                
046500     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
046600                         DAGENS-DATUM2-MAANAD                             
046700     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
046800                         DAGENS-DATUM2-DAG                                
046900     MOVE 20          TO DAGENS-DATUM2-SEKEL                              
047000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
047100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
047200                                                                          
047300     ACCEPT TODAYS-DATE FROM DATE                                         
047400     .                                                                    
047500     EJECT                                                                
047600 B-SKRIV-UPD-WDK6 SECTION.                                                
047700                                                                          
047800     MOVE ART-IDARTNR     TO UT-IDARTNR                                   
047900     MOVE REFL-KVREFOVL   TO UT-KVREFOVL                                  
048000     MOVE REFL-KVREFPKT   TO UT-KVREFPKT                                  
048100     MOVE REFL-KVREFBER   TO UT-KVREFBER                                  
048200     MOVE REFL-KVSLAGER   TO UT-KVSLAGER                                  
048300                                                                          
048400     PERFORM S11-SKRIV-W27220                                             
048500     .                                                                    
048600     EJECT                                                                
048700                                                                          
048800 Z-FINIT SECTION.                                                         
048900                                                                          
049000     CLOSE W27220                                                         
049100     SKIP2                                                                
049200     MOVE 'S' TO POSTSUM-OPKOD                                            
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     .                                                                    
049500     EJECT                                                                
049600 S11-SKRIV-W27220 SECTION.                                                
049700     SKIP2                                                                
049800     WRITE UT-POST FROM UT-AREA                                           
049900                                                                          
050000     MOVE UT-IDARTNR TO POSTSUM-TRANSTYP                                  
050100     MOVE 'W27220 '  TO POSTSUM-FDNAMN                                    
050200     MOVE 'W27212D1' TO POSTSUM-DDNAMN2                                   
050300     CALL POSTSUM    USING POSTSUM-PARM                                   
050400     .                                                                    
050500     EJECT                                                                
050600* --- IMS SECTIONS  ---                                                   
050700     SKIP3                                                                
050800     EJECT                                                                
050900 IMS-GN-WDK629 SECTION.                                                   
051000     MOVE 'WDK629 '        TO SSA1                                        
051100     MOVE '  GEGB'           TO GOOD-STATUSCODES                          
051200     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
051300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUSCHECK                                              
051500     SKIP3                                                                
051600     .                                                                    
051700     EJECT                                                                
051800 IMS-GNP-WDK611 SECTION.                                                  
051900     MOVE 'WDK611 '        TO SSA1                                        
052000     MOVE '  '             TO GOOD-STATUSCODES                            
052100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
052200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSCHECK                                              
052400     SKIP3                                                                
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GNP-WDK601 SECTION.                                                  
052800     MOVE 'WDK601 '        TO SSA1                                        
052900     MOVE '  '             TO GOOD-STATUSCODES                            
053000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
053100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053200     PERFORM IMS-STATUSCHECK                                              
053300     SKIP3                                                                
053400     .                                                                    
053500     EJECT                                                                
053600 IMS-STATUSCHECK SECTION.                                                 
053700     SKIP2                                                                
053800     SET STATUS-IX TO 1                                                   
053900     SEARCH GOOD-STATUS                                                   
054000       AT END                                                             
054100         MOVE 'WRONG CODE' TO ERRTEXT-STR                                 
054200         DISPLAY ERRTEXT                                                  
054300         CALL FELLOG                                                      
054400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
054500         CONTINUE                                                         
054600     END-SEARCH                                                           
054700     .                                                                    
054800     EJECT                                                                
054900*    -COPY WY2000P1                                                       
