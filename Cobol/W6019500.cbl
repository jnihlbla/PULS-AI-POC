000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019500.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/05/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM                               
000900*        HANTERAR UTSKRIFT AV ETIKETTER.                                  
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W6T195X                                             
001300*        MID:         W6I19501                                            
001400*                                                                         
001500*    UTDATA:                                                              
001600*        UTSKRIVNA ETIKETTER (VIA W006PRC1)                               
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6019500'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003400     88 EGEN-MID                             VALUE '6195'.                
003500 01      FILLER                  PIC X(8)    VALUE 'WS******'.            
003600 01      WS.                                                              
003700*     -- PRINTER-ID                                                       
003800  02     WS-IDPRTLST             PIC X(8)    VALUE SPACE.                 
003900     EJECT                                                                
004000 01      FILLER                  PIC X(8)    VALUE 'IX-*****'.            
004100 01      IX-INDEXVARIABLER.                                               
004200*     -- POST INOM MID                                                    
004300  02     IX-POST                 PIC S9(9)   VALUE ZERO COMP SYNC.        
004400  02     ETIKETT-IX              PIC S9(9)   VALUE ZERO COMP SYNC.        
004500     SKIP3                                                                
004600 01      FILLER                  PIC X(8)    VALUE 'K-******'.            
004700 01      K-KONSTANTER.                                                    
004800*     -- MAX ANTAL POSTER I MID                                           
004900  02     K-MAX-KVPOST            PIC S9(3)   VALUE +15  COMP-3.           
005000*     -- MAX ANTAL RADER PER ETIKETT                                      
005100  02     MAX-IX                  PIC S9(3)   VALUE +15  COMP-3.           
005200  02     MAX-IX-VCOM             PIC S9(3)   VALUE +9   COMP-3.           
005300  02     MAX-IX-ZEBRA            PIC S9(3)   VALUE +17  COMP-3.           
005400     EJECT                                                                
005500 01      LIST1-ARTIKEL-RAD       PIC X(132).                              
005600         EJECT                                                            
005700*    --- ETIKETT-LISTA ARBETSFÄLT                                         
005800 01      FILLER                  PIC X(16)                                
005900                                 VALUE 'ETIKETT-TAB-VCOM'.                
006000 01      ETIKETT-TAB-VCOM.                                                
006100   03    FILLER                  PIC X(80)   VALUE                        
006200         '!M "ETIKETT"                                         '.         
006300   03    FILLER.                                                          
006400      05 ETI-IDLOPNRM-VCOM       PIC Z(8).                                
006500      05 FILLER                  PIC X(1)   VALUE SPACE.                  
006600      05 ETI-IDRADNR-VCOM        PIC Z(3).                                
006700      05 FILLER                  PIC X(68)  VALUE SPACE.                  
006800   03    FILLER.                                                          
006900      05 ETI-IDARTNR-VCOM        PIC Z(8).                                
007000      05 FILLER                  PIC X(72)  VALUE SPACE.                  
007100   03    FILLER.                                                          
007200      05 ETI-KVINLART-VCOM       PIC Z(6).                                
007300      05 FILLER                  PIC X(74)  VALUE SPACE.                  
007400   03    FILLER.                                                          
007500      05 ETI-BEART-VCOM          PIC X(20).                               
007600      05 FILLER                  PIC X(60)  VALUE SPACE.                  
007700   03    FILLER.                                                          
007800      05 ETI-ADLAGOMR-VCOM       PIC Z(2).                                
007900      05 FILLER                  PIC X(1)   VALUE SPACE.                  
008000      05 ETI-ADGANG-VCOM         PIC Z(2).                                
008100      05 FILLER                  PIC X(1)   VALUE SPACE.                  
008200      05 ETI-ADPLATS-VCOM        PIC Z(5).                                
008300      05 FILLER                  PIC X(69)  VALUE SPACE.                  
008400   03    FILLER.                                                          
008500      05 ETI-IDLOPNRM-STRK-VCOM  PIC 9(8).                                
008600      05 ETI-IDRADNR-STRK-VCOM   PIC 9(3).                                
008700      05 FILLER                  PIC X(69)  VALUE SPACE.                  
008800   03    FILLER                  PIC X(80)   VALUE                        
008900         '!P 2                                                 '.         
009000   03    FILLER                  PIC X(80)   VALUE                        
009100         '!R                                                   '.         
009200 01  FILLER REDEFINES ETIKETT-TAB-VCOM.                                   
009300   03  FILLER OCCURS 09.                                                  
009400     05  ETIKETT-RAD-VCOM        PIC X(80).                               
009500     EJECT                                                                
009600 01      FILLER                  PIC X(16)                                
009700                                 VALUE 'ETIKETT-TAB'.                     
009800 01      ETIKETT-TAB.                                                     
009900   03    FILLER                  PIC X(80)   VALUE                        
010000         '!C                                                   '.         
010100   03    FILLER                  PIC X(80)   VALUE                        
010200         '!C                                                   '.         
010300   03    FILLER                  PIC X(80)   VALUE                        
010400         '!F T S 400 1000 L 1 1 3 "Etikettnummer"              '.         
010500   03    FILLER                  PIC X(80)   VALUE                        
010600         '!F T S 150 1000 L 1 1 3 "Artikelnummer"              '.         
010700   03    FILLER                  PIC X(80)   VALUE                        
010800         '!F T S  50 1000 L 1 1 3 "Antal"                      '.         
010900   03    FILLER                  PIC X(80)   VALUE                        
011000         '!F T S 160 550  L 1 1 3 "Benämning"                  '.         
011100   03    FILLER                  PIC X(80)   VALUE                        
011200         '!F T S  60 1000 L 1 1 3 "Lagerplats"                 '.         
011300   03    FILLER.                                                          
011400      05 FILLER                  PIC X(25)  VALUE                         
011500         '!F T S 350 1000 L 2 2 3 "'.                                     
011600      05 ETI-IDLOPNRM            PIC Z(8).                                
011700      05 FILLER                  PIC X(1)   VALUE SPACE.                  
011800      05 ETI-IDRADNR             PIC Z(3).                                
011900      05 FILLER                  PIC X(43)  VALUE                         
012000         '"                        '.                                     
012100   03    FILLER.                                                          
012200      05 FILLER                  PIC X(25)  VALUE                         
012300         '!F T S 100 1000 L 2 2 3 "'.                                     
012400      05 ETI-IDARTNR             PIC Z(8).                                
012500      05 FILLER                  PIC X(47)  VALUE                         
012600         '"                        '.                                     
012700   03    FILLER.                                                          
012800      05 FILLER                  PIC X(25)  VALUE                         
012900         '!F T S  10 1000 L 2 2 3 "'.                                     
013000      05 ETI-KVINLART            PIC Z(6).                                
013100      05 FILLER                  PIC X(49)  VALUE                         
013200         '"                        '.                                     
013300   03    FILLER.                                                          
013400      05 FILLER                  PIC X(25)  VALUE                         
013500         '!F T S 100 550  L 2 2 3 "'.                                     
013600      05 ETI-BEART               PIC X(20).                               
013700      05 FILLER                  PIC X(35)  VALUE                         
013800         '"                        '.                                     
013900   03    FILLER.                                                          
014000      05 FILLER                  PIC X(25)  VALUE                         
014100         '!F T S  10 550  L 2 2 3 "'.                                     
014200      05 ETI-ADLAGOMR            PIC Z(2).                                
014300      05 FILLER                  PIC X(1)   VALUE SPACE.                  
014400      05 ETI-ADGANG              PIC Z(2).                                
014500      05 FILLER                  PIC X(1)   VALUE SPACE.                  
014600      05 ETI-ADPLATS             PIC Z(5).                                
014700      05 FILLER                  PIC X(44)  VALUE                         
014800         '"                        '.                                     
014900   03    FILLER.                                                          
015000      05 FILLER                  PIC X(29)  VALUE                         
015100         '!F C S 200 1000 L 130 3 12 "P'.                                 
015200      05 ETI-IDLOPNRM-STRK       PIC 9(8).                                
015300      05 ETI-IDRADNR-STRK        PIC 9(3).                                
015400      05 FILLER                  PIC X(40)  VALUE                         
015500         '"                        '.                                     
015600   03    FILLER                  PIC X(80)   VALUE                        
015700         '!P 2                                                 '.         
015800   03    FILLER                  PIC X(80)   VALUE                        
015900         '!R                                                   '.         
016000 01  FILLER REDEFINES ETIKETT-TAB.                                        
016100   03  FILLER OCCURS 15.                                                  
016200     05  ETIKETT-RAD             PIC X(80).                               
016300     EJECT                                                                
016400 01      FILLER                  PIC X(16)   VALUE 'ZEBRA-TAB'.           
016500 01      ZEBRA-TAB.                                                       
016600   03    FILLER                  PIC X(80)   VALUE                        
016700         '^XA^CF0^FWN^FS                                       '.         
016800   03    FILLER.                                                          
016900       05  FILLER                PIC X(80)   VALUE                        
017000         '^FO129,64^A0N,30,30^FD Sequence number.^FS           '.         
017100   03    FILLER.                                                          
017200       05  FILLER                PIC X(80)   VALUE                        
017300         '^FO141,365^A0N,30,30^FDPartnumber^FS                 '.         
017400   03    FILLER.                                                          
017500       05  FILLER                PIC X(80)   VALUE                        
017600         '^FO667,365^A0N,30,30^FDDescription^FS                '.         
017700   03    FILLER.                                                          
017800       05  FILLER                PIC X(80)   VALUE                        
017900         '^FO141,482^A0N,30,30^FDQuantity^FS                   '.         
018000   03    FILLER.                                                          
018100       05  FILLER                PIC X(80)   VALUE                        
018200         '^FO667,482^A0N,30,30^FDStorage Area^FS               '.         
018300   03    FILLER.                                                          
018400       05  FILLER                PIC X(23)   VALUE                        
018500         '^FO141,105^A0N,70,70^FD'.                                       
018600       05  ZEBRA-IDLOPNRM        PIC 9(08).                               
018700       05  FILLER                PIC X(49)   VALUE                        
018800         '^FS                    '.                                       
018900   03    FILLER.                                                          
019000       05  FILLER                PIC X(23)   VALUE                        
019100         '^FO527,105^A0N,70,70^FD'.                                       
019200       05  ZEBRA-IDRADNR         PIC 9(03).                               
019300       05  FILLER                PIC X(54)   VALUE                        
019400         '^FS                    '.                                       
019500   03    FILLER.                                                          
019600       05  FILLER                PIC X(23)   VALUE                        
019700         '^FO141,415^A0N,70,70^FD'.                                       
019800       05  ZEBRA-IDARTNR         PIC z(08).                               
019900       05  FILLER                PIC X(49)   VALUE                        
020000         '^FS                    '.                                       
020100   03    FILLER.                                                          
020200       05  FILLER                PIC X(23)   VALUE                        
020300         '^FO667,415^A0N,70,70^FD'.                                       
020400       05  ZEBRA-BEART           PIC x(25).                               
020500       05  FILLER                PIC X(32)   VALUE                        
020600         '^FS                    '.                                       
020700   03    FILLER.                                                          
020800       05  FILLER                PIC X(23)   VALUE                        
020900         '^FO141,525^A0N,70,70^FD'.                                       
021000       05  ZEBRA-KVINLART        PIC z(06).                               
021100       05  FILLER                PIC X(51)   VALUE                        
021200         '^FS                    '.                                       
021300   03    FILLER.                                                          
021400       05  FILLER                PIC X(23)   VALUE                        
021500         '^FO667,525^A0N,70,70^FD'.                                       
021600       05  ZEBRA-ADLAGOMR        PIC 9(02).                               
021700       05  FILLER                PIC X(55)   VALUE                        
021800         '^FS                    '.                                       
021900   03    FILLER.                                                          
022000       05  FILLER                PIC X(23)   VALUE                        
022100         '^FO767,525^A0N,70,70^FD'.                                       
022200       05  ZEBRA-ADGANG          PIC Z(02).                               
022300       05  FILLER                PIC X(55)   VALUE                        
022400         '^FS                    '.                                       
022500   03    FILLER.                                                          
022600       05  FILLER                PIC X(23)   VALUE                        
022700         '^FO887,525^A0N,70,70^FD'.                                       
022800       05  ZEBRA-ADPLATS         PIC Z(05).                               
022900       05  FILLER                PIC X(52)   VALUE                        
023000         '^FS                    '.                                       
023100   03    FILLER.                                                          
023200       05  FILLER                PIC X(80)   VALUE                        
023300         '^BY4,,                 '.                                       
023400   03    FILLER.                                                          
023500       05  FILLER                PIC X(32)   VALUE                        
023600         '^FO129,181^B3N,N,152,N,N^FWN^FDP'.                              
023700       05  ZEBRA-IDLOPNRM-STRK   PIC 9(08).                               
023800       05  ZEBRA-IDRADNR-STRK    PIC 9(03).                               
023900       05  FILLER                PIC X(37)   value                        
024000         '^FS                    '.                                       
024100   03    FILLER.                                                          
024200       05  FILLER                PIC X(80)   VALUE                        
024300         '^XZ                             '.                              
024400 01  FILLER REDEFINES ZEBRA-TAB.                                          
024500   03  FILLER OCCURS 17.                                                  
024600     05  ZEBRA-RAD             PIC X(80).                                 
024700     EJECT                                                                
024800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
024900 01  GENERELLA-SUBPROGRAM.                                                
025000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025200     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
025300     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
025400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
025500     EJECT                                                                
025600*    --- W006PRAR, AREA FÖR W006PRC1 OCH W006PRS1                         
025700 01  FILLER                    PIC X(8)  VALUE 'W006PRAR'.                
025800                                                                          
025900*01  -COPY W006PRAR                                                       
026000     EJECT                                                                
026100 01  FILLER                    PIC X(8)  VALUE 'W006PRVC'.                
026200                                                                          
026300*01  -COPY W006PRVC                                                       
026400     EJECT                                                                
026500*01  -COPY W006PRT                                                        
026600     EJECT                                                                
026700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
026800*                                                                         
026900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
027000     SKIP3                                                                
027100*01  MID -COPY W6I19501                                                   
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
027400     SKIP3                                                                
027500*01  -COPY WMSGAREA                                                       
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027800     SKIP3                                                                
027900*01  -COPY WMSGSPAR                                                       
028000     EJECT                                                                
028100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028200*                                                                         
028300     SKIP2                                                                
028400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028500     SKIP2                                                                
028600*    --- STATUS-KOD FRÅN IMS                                              
028700 01  STATUS-WS                   PIC XX.                                  
028800     88  SEGMENT-FINNS                       VALUE '  '.                  
028900     SKIP2                                                                
029000 01  GODK-STATUSKODER.                                                    
029100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029200     EJECT                                                                
029300*    --- IMS FUNKTIONSKODER                                               
029400*01  -COPY W0003                                                          
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700*01  -COPY W0009   -PRE MSG-                                              
029800     EJECT                                                                
029900*01  -COPY W0009   -PRE ALT-                                              
030000     EJECT                                                                
030100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB.                               
030200 MAIN SECTION.                                                            
030300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
030400                                                                          
030500     PERFORM IMS-GET-MSG                                                  
030600     IF SEGMENT-FINNS                                                     
030700       PERFORM A-INIT                                                     
030800                                                                          
030900       MOVE MID-IDPRTLST         TO WS-IDPRTLST                           
031000                                    PRT-IDPRTLST                          
031100                                                                          
031200       PERFORM S01-PRT-OPEN                                               
031300                                                                          
031400       MOVE +1                   TO IX-POST                               
031500                                                                          
031600       PERFORM UNTIL (IX-POST > K-MAX-KVPOST                              
031700                  OR  IX-POST > MID-KVPOST)                               
031800                                                                          
031900         PERFORM B-RED-SKR-SIDA                                           
032000         ADD +1                  TO IX-POST                               
032100       END-PERFORM                                                        
032200                                                                          
032300       PERFORM S03-PRT-CLOSE                                              
032400     END-IF                                                               
032500                                                                          
032600     MOVE ZERO TO RETURN-CODE                                             
032700     GOBACK                                                               
032800     .                                                                    
032900                                                                          
033000     EJECT                                                                
033100 A-INIT SECTION.                                                          
033200                                                                          
033300     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I19501                    
033400     MOVE MSG-IDTRANS-1      TO MSG-SPAR-IDTRANS                          
033500     MOVE MSG-KDMFSFOR-1     TO MSG-SPAR-KDMFSFOR                         
033600                                                                          
033700     MOVE MSG-KDTRTYP TO MSG-SPAR-KDTRTYP                                 
033800     MOVE MSG-IDPFK TO MSG-SPAR-IDPFK                                     
033900     MOVE MSG-SPAR-IDTRANS TO W-IDTRANS                                   
034000                                                                          
034100     MOVE LOW-VALUE TO MSG-AREA                                           
034200     .                                                                    
034300                                                                          
034400     EJECT                                                                
034500 B-RED-SKR-SIDA SECTION.                                                  
034600                                                                          
034700     MOVE SPACE TO LIST1-ARTIKEL-RAD                                      
034800     MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                             
034900                                                                          
035000     MOVE MID-IDLOPNRM (IX-POST) TO ETI-IDLOPNRM                          
035100                                    ETI-IDLOPNRM-VCOM                     
035200                                    ETI-IDLOPNRM-STRK                     
035300                                    ETI-IDLOPNRM-STRK-VCOM                
035400                                    ZEBRA-IDLOPNRM                        
035500                                    ZEBRA-IDLOPNRM-STRK                   
035600     MOVE MID-IDRADNR  (IX-POST) TO ETI-IDRADNR                           
035700                                    ETI-IDRADNR-VCOM                      
035800                                    ETI-IDRADNR-STRK                      
035900                                    ETI-IDRADNR-STRK-VCOM                 
036000                                    ZEBRA-IDRADNR                         
036100                                    ZEBRA-IDRADNR-STRK                    
036200                                                                          
036300     MOVE MID-IDARTNR  (IX-POST) TO ETI-IDARTNR                           
036400                                    ETI-IDARTNR-VCOM                      
036500                                    ZEBRA-IDARTNR                         
036600     MOVE MID-BEART    (IX-POST) TO ETI-BEART                             
036700                                    ETI-BEART-VCOM                        
036800                                    ZEBRA-BEART                           
036900                                                                          
037000     MOVE MID-KVINLART (IX-POST) TO ETI-KVINLART                          
037100                                    ETI-KVINLART-VCOM                     
037200                                    ZEBRA-KVINLART                        
037300     MOVE MID-ADLAGOMR (IX-POST) TO ETI-ADLAGOMR                          
037400                                    ETI-ADLAGOMR-VCOM                     
037500                                    ZEBRA-ADLAGOMR                        
037600     MOVE MID-ADGANG   (IX-POST) TO ETI-ADGANG                            
037700                                    ETI-ADGANG-VCOM                       
037800                                    ZEBRA-ADGANG                          
037900     MOVE MID-ADPLATS  (IX-POST) TO ETI-ADPLATS                           
038000                                    ETI-ADPLATS-VCOM                      
038100                                    ZEBRA-ADPLATS                         
038200                                                                          
038300     MOVE +1 TO ETIKETT-IX                                                
038400     IF WS-IDPRTLST = '6EPRE1  '                                          
038500       PERFORM UNTIL ETIKETT-IX > MAX-IX-ZEBRA                            
038600         MOVE ZEBRA-RAD (ETIKETT-IX) TO LIST1-ARTIKEL-RAD                 
038700         CALL W006PRS1 USING         PRT-SPOOL-OVR                        
038800                                     PRT-WRITE                            
038900                                     WS-IDPRTLST                          
039000                                     ALT-PCB                              
039100                                     PRT-RADSKIP                          
039200                                     LIST1-ARTIKEL-RAD                    
039300         MOVE PRT-AFTER-1          TO PRT-RADSKIP                         
039400         ADD +1 TO ETIKETT-IX                                             
039500       END-PERFORM                                                        
039600       MOVE +1 TO ETIKETT-IX                                              
039700       PERFORM UNTIL ETIKETT-IX > MAX-IX-ZEBRA                            
039800         MOVE ZEBRA-RAD (ETIKETT-IX) TO LIST1-ARTIKEL-RAD                 
039900         CALL W006PRS1 USING         PRT-SPOOL-OVR                        
040000                                     PRT-WRITE                            
040100                                     WS-IDPRTLST                          
040200                                     ALT-PCB                              
040300                                     PRT-RADSKIP                          
040400                                     LIST1-ARTIKEL-RAD                    
040500         MOVE PRT-AFTER-1          TO PRT-RADSKIP                         
040600         ADD +1 TO ETIKETT-IX                                             
040700       END-PERFORM                                                        
040800     ELSE                                                                 
040900       IF PRT-BEPRTLST(1:4) = 'VCOM'                                      
041000         CONTINUE                                                         
041100*        PERFORM UNTIL ETIKETT-IX > MAX-IX-VCOM                           
041200*          MOVE ETIKETT-RAD-VCOM (ETIKETT-IX) TO PRC1-DATA                
041300*          CALL W006PRC1 USING         PRT-VCOM                           
041400*                                      PRT-WRITE                          
041500*                                      WS-IDPRTLST                        
041600*                                      ALT-PCB                            
041700*                                      PRC1-W006PRVC                      
041800*          ADD +1 TO ETIKETT-IX                                           
041900*        END-PERFORM                                                      
042000       ELSE                                                               
042100         PERFORM UNTIL ETIKETT-IX > MAX-IX                                
042200           MOVE ETIKETT-RAD (ETIKETT-IX) TO LIST1-ARTIKEL-RAD             
042300           CALL W006PRS1 USING         PRT-SPOOL-OVR                      
042400                                       PRT-WRITE                          
042500                                       WS-IDPRTLST                        
042600                                       ALT-PCB                            
042700                                       PRT-RADSKIP                        
042800                                       LIST1-ARTIKEL-RAD                  
042900           ADD +1 TO ETIKETT-IX                                           
043000         END-PERFORM                                                      
043100       END-IF                                                             
043200     END-IF                                                               
043300     .                                                                    
043400                                                                          
043500     EJECT                                                                
043600 S01-PRT-OPEN  SECTION.                                                   
043700                                                                          
043800     MOVE '001'            TO    PRT-KDCALL                               
043900     CALL W006PRT USING PRT-W006PRT                                       
044000                                                                          
044100*    MOVE 'W601Z1SE'       TO    PRT-IDVCOM                               
044200     MOVE 'W601FLAA'       TO    PRT-IDCPYTXT                             
044300     MOVE +120             TO    PRC1-KVLRECL                             
044400     MOVE 'W006ASCI'       TO    PRC1-IDVCINIT                            
044500     MOVE 'W006PRT '       TO    PRC1-TEVCOMST                            
044600     MOVE SPACE            TO    PRC1-DATA                                
044700                                                                          
044800                                                                          
044900     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
044910       CONTINUE                                                           
045000*      CALL W006PRC1 USING         PRT-VCOM                               
045100*                                  PRT-OPEN                               
045200*                                  WS-IDPRTLST                            
045300*                                  ALT-PCB                                
045400*                                  PRC1-W006PRVC                          
045500     ELSE                                                                 
045600       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
045700                                   PRT-OPEN                               
045800                                   WS-IDPRTLST                            
045900                                   ALT-PCB                                
046000                                   PRT-FILLER                             
046100                                   PRT-FILLER                             
046200     END-IF                                                               
046300     .                                                                    
046400                                                                          
046500     EJECT                                                                
046600 S03-PRT-CLOSE SECTION.                                                   
046700                                                                          
046800     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
046810       CONTINUE                                                           
046900*      CALL W006PRC1 USING         PRT-VCOM                               
047000*                                  PRT-CLOSE                              
047100*                                  WS-IDPRTLST                            
047200*                                  ALT-PCB                                
047300*                                  PRC1-W006PRVC                          
047400     ELSE                                                                 
047500       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
047600                                   PRT-CLOSE                              
047700                                   WS-IDPRTLST                            
047800                                   ALT-PCB                                
047900                                   PRT-FILLER                             
048000                                   PRT-FILLER                             
048100     END-IF                                                               
048200     .                                                                    
048300                                                                          
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600                                                                          
048700                                                                          
048800 IMS-GET-MSG SECTION.                                                     
048900                                                                          
049000     MOVE '  QC' TO GODK-STATUSKODER                                      
049100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500                                                                          
049600                                                                          
049700 IMS-STATUSKONTROLL SECTION.                                              
049800                                                                          
049900     SET STATUS-IX TO 1                                                   
050000     SEARCH GODK-STATUS                                                   
050100       AT END                                                             
050200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
050300         DELIMITED BY SIZE INTO FELTEXT                                   
050400         CALL FELLOG                                                      
050500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050600         CONTINUE                                                         
050700     END-SEARCH                                                           
050800     .                                                                    
