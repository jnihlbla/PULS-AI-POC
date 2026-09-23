000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036600.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   FEBR. 90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        MPP-PROGRAM SOM INGÅR I PD90-ORDERSYSTEMET                       
001100*                                  (WOPS-SYSTEMET).                       
001200*                                                                         
001300*        MÖJLIGHETER: UNDERHÅLL AV PRC-STYR-TABELL.                       
001400*                                                                         
001500*                     PROGRAMMET VISAR OCH UPPDATERAR DEN                 
001600*                     TABELL SOM VISAR VILKEN PRC SOM                     
001700*                     ORDERDELEN SKALL HAMNA I.                           
001800*                                                                         
001900*                     PROGRAMMET UPPDATERAR WLXXKG (WDR1).                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T366  W4T366U                                     
002300*        MID:         W4I36601                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W4O36601                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                   PIC X(8)    VALUE 'W4036600'.                
003600                                                                          
003700 77  JA                      PIC X       VALUE 'J'.                       
003800 77  NEJ                     PIC X       VALUE 'N'.                       
003900 77  STRECK                  PIC X       VALUE '-'.                       
004000                                                                          
004100 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
004200 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004300 77  MAX-TABRADER            PIC S9(9)   VALUE +13  COMP SYNC.            
004400 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +960 COMP SYNC.            
004500                                                                          
004600 77  IDPRCTAB-WS             PIC X(2)    VALUE SPACE.                     
004700 77  WS-IO-AREA              PIC X(150).                                  
004820 77  LAES-SW                 PIC X       VALUE 'J'.                       
004830 77  INDATA-SW               PIC X       VALUE 'J'.                       
004900   88  INDATA-OK                         VALUE 'J'.                       
005000   88  INDATA-FEL                        VALUE 'N'.                       
005100                                                                          
005200 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
005300   88  NYCKLAR-OK                        VALUE 'J'.                       
005400   88  NYCKLAR-FEL                       VALUE 'N'.                       
005500                                                                          
005600 77  UPPDAT-SW               PIC X       VALUE 'J'.                       
005700   88  UPPDATERING-OK                    VALUE 'J'.                       
005800   88  UPPDATERING-FEL                   VALUE 'N'.                       
005900                                                                          
006000 77  NYTABELL-SW             PIC X       VALUE 'N'.                       
006100   88  NYTABELL-OK                       VALUE 'J'.                       
006200   88  EJ-NYTABELL                       VALUE 'N'.                       
006300                                                                          
006400 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006500   88  EGEN-TRANS                        VALUE '4366'.                    
006600   88  GODK-TRANS                        VALUE '4361' '4362'              
006700                                               '4363' '4366'.             
006800                                                                          
006900 01  WS-IDPRC.                                                            
007000   03  WS-IDPRCBAS           PIC X(3)    VALUE SPACE.                     
007100   03  WS-IDPRCVAR           PIC X       VALUE SPACE.                     
007200                                                                          
007300     EJECT                                                                
007400 01  GENERELLA-SUBPROGRAM.                                                
007500   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
007600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007800   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
007900     EJECT                                                                
008000*   -COPY WMEDAREA                                                        
008100     EJECT                                                                
008200*                   ****    PARAMETRAR TILL W005INIT                      
008300*01  -COPY WMSGINIT                                                       
008400     EJECT                                                                
008500 01  FELM-CODES.                                                          
008600     03  FILLER                     PIC X(16)  VALUE 'FELM AREA'.         
008700     03  FELM-KOR-UPPLYSTA-FAELT    PIC X(3)   VALUE '001'.               
008800     03  FELM-OTILL-UPPDATERING     PIC X(3)   VALUE '007'.               
008900     03  FELM-PF11-O-TOM-INDATARAD  PIC X(3)   VALUE '011'.               
009000     03  FELM-TABELL-SAKNAS         PIC X(3)   VALUE '023'.               
009100     03  FELM-FEL-NYCKEL            PIC X(3)   VALUE '401'.               
009200     SKIP3                                                                
009300 01  MESSAGE-CODES.                                                       
009400     03  FILLER                     PIC X(16)   VALUE 'INFO AREA'.        
009500     03  INFO-TRYCK-PF11            PIC X(3)    VALUE '003'.              
009600     03  INFO-FOERSTA-SIDAN         PIC X(3)    VALUE '006'.              
009700     03  INFO-GEN-TAB-UPPLAGD       PIC X(3)    VALUE '024'.              
009800     03  INFO-UPPDAT-GJORD          PIC X(3)    VALUE '101'.              
009900     03  INFO-MER-INFO-FINNS-PF8    PIC X(3)    VALUE '105'.              
010000     03  INFO-LAST-PAGE             PIC X(3)    VALUE '106'.              
010100     EJECT                                                                
010200 01  SAVE-AREA.                                                           
010300   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
010400   03  PGNO                      PIC 9(2)    VALUE 01.                    
010410   03  FIRST-SW                  PIC X       VALUE 'J'.                   
010500   03  SAVE-IDRADNR-ENTER OCCURS 20 PIC 9(4) VALUE ZERO.                  
010600   03  SAVE-IDRADNR-NEXT            PIC 9(4) VALUE ZERO.                  
011300******************************************************************        
011400*                                                                         
011500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011600*                                                                         
011700 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
011800     SKIP3                                                                
011900*01  MID -COPY W4I36601                                                   
012000     EJECT                                                                
012100*01  -COPY WMSGAREA                                                       
012200     EJECT                                                                
012300*  03  MOD -COPY W4O36601 -RED MSG-AREA.                                  
012400     EJECT                                                                
012500*01  -COPY WMFSAREA                                                       
012600     EJECT                                                                
012700******************************************************************        
012800*                                                                         
012900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013000*                                                                         
013100 01  IMS-WS.                                                              
013200   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
013300     SKIP3                                                                
013400*                        **** STATUS-KOD FRÅN IMS                         
013500   03  STATUS-WS             PIC XX.                                      
013600     88  SEGMENT-FINNS                   VALUE '  '.                      
013700     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
013800     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
013900     SKIP3                                                                
014000   03  GODK-STATUSKODER.                                                  
014100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  NYCKLAR-TILL-DLI.                                                    
014400   03  W-WDGXKEY-4445-X.                                                  
014500     05  W-IDHTYP-4445       PIC X(4)    VALUE '4445'.                    
014600     05  W-IDDC-4445         PIC X(2)    VALUE '00'.                      
014700     05  W-IDPRCTAB          PIC 9(2)    VALUE ZERO.                      
014800     05  FILLER              PIC X(22)   VALUE LOW-VALUE.                 
014900                                                                          
015000   03  W-WDGXKEY-4446-X.                                                  
015100     05  W-IDRADNR           PIC S9(5)   VALUE ZERO  COMP-3.              
015200     05  FILLER              PIC X(2)    VALUE LOW-VALUE.                 
015300                                                                          
015400   03  W-WDGXKEY-4447-X.                                                  
015500     05  W-IDHTYP-4447       PIC X(4)    VALUE '4447'.                    
015600     05  W-IDDC-4447         PIC X(2)    VALUE '00'.                      
015700     05  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
015800                                                                          
015900   03  W-WDGXKEY-4448-X.                                                  
016000     05  W-IDPRC             PIC X(4)    VALUE SPACE.                     
016100     05  FILLER              PIC X       VALUE LOW-VALUE.                 
016200                                                                          
016300   03  W-WDGXKEY-4451-X.                                                  
016400     05  W-IDHTYP-4451       PIC X(4)    VALUE '4451'.                    
016500     05  W-IDDC-4451         PIC X(2)    VALUE '00'.                      
016600     05  W-IDPTIDTAB         PIC 9(2)    VALUE ZERO.                      
016700     05  FILLER              PIC X(22)   VALUE LOW-VALUE.                 
016800                                                                          
016900   03  W-IDDC-B6-X.                                                       
017000       05 W-IDDC-B6                  PIC X(2).                            
017100                                                                          
017200 01    SSA1                  PIC X(64).                                   
017300 01    SSA2                  PIC X(64).                                   
017400     EJECT                                                                
017500*                            IMS FUNKTIONSKODER                           
017600*01    -COPY W0003                                                        
017700     EJECT                                                                
017800*           DLI INPUT-OUTPUT AREA                                         
017900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
018000                                                                          
018100 01  DLI-IO-AREA.                                                         
018200   03  IO-AREA               PIC X(150)  VALUE SPACE.                     
018300                                                                          
018400                                                                          
018500*  03  WLXXKG01  -COPY WDGX4445  -PRE XXKG-  -RED IO-AREA.                
018600     EJECT                                                                
018700*  03  WLXXKG11  -COPY WDGX4446  -PRE XXKG-  -RED IO-AREA.                
018800                                                                          
018900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019000 01   DLI-IO-AREA-B601.                                                   
019100*     03  -COPY WDB601                                                    
019200                                                                          
019300     EJECT                                                                
019400                                                                          
019500 LINKAGE SECTION.                                                         
019600*01  -COPY W0009     -PRE MSG-                                            
019700     EJECT                                                                
019800*01  -COPY W0008     -PRE USEA-                                           
019900     05  FILLER              PIC X.                                       
020000     EJECT                                                                
020100*01  -COPY W0008     -PRE XXKG-                                           
020200     05  FILLER              PIC X.                                       
020300     EJECT                                                                
020400*01  -COPY W0008     -PRE XXKH-                                           
020500     05  FILLER              PIC X.                                       
020600*01  -COPY W0008     -PRE XXKI-                                           
020700     05  FILLER              PIC X.                                       
020800*01  -COPY W0008     -PRE WDB6-                                           
020900     05  FILLER              PIC X.                                       
021000                                                                          
021100 EJECT                                                                    
021200 PROCEDURE DIVISION USING  MSG-PCB  USEA-PCB                              
021300                                    XXKG-PCB  XXKH-PCB  XXKI-PCB          
021400                                    WDB6-PCB.                             
021500     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
021600                                    XXKG-PCB  XXKH-PCB  XXKI-PCB          
021700                                    WDB6-PCB.                             
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FINNS                                                     
022100       PERFORM A-INIT                                                     
022200       PERFORM B-KOLLA-NYCKLAR                                            
022300       IF NYCKLAR-OK                                                      
022500                                                                          
022600         IF MFS-UPDATE                                                    
022700           PERFORM G-UPPDATERA                                            
023200         ELSE                                                             
023202             IF MFS-FIRST                                                 
023203               PERFORM C-FOERSTA-SIDAN                                    
023500             ELSE                                                         
023501               IF MFS-NEXT                                                
023502                 PERFORM D-NAESTA-SIDAN                                   
023530               ELSE                                                       
023531                 IF MFS-PREVIOUS                                          
023532                   PERFORM I-PREV-SIDE                                    
023533                 ELSE                                                     
023600                   PERFORM E-SAMMA-SIDA                                   
023700                 END-IF                                                   
023701               END-IF                                                     
023710             END-IF                                                       
023900             PERFORM S01-LAES-VISA-TABELL                                 
024800         END-IF                                                           
024900       END-IF                                                             
025000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025100       PERFORM IMS-INSERT-MSG                                             
025200     END-IF                                                               
025400     MOVE ZERO TO RETURN-CODE                                             
025500     GOBACK                                                               
025600     .                                                                    
025700     EJECT                                                                
025800 A-INIT SECTION.                                                          
026000     IF MSG-DUBBLA-TRANSKODER                                             
026100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36601                 
026200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026400     ELSE                                                                 
026500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36601                  
026600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026800     END-IF                                                               
026900                                                                          
027000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
027100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
027200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027300                                                                          
027400     MOVE LOW-VALUE TO MSG-AREA                                           
027500     MOVE 'W4O366N1' TO MFS-IDMOD                                         
027600     MOVE '4366' TO MOD-IDTRANS                                           
027700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027900     IF NOT EGEN-TRANS                                                    
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100       MOVE '7' TO MFS-IDPFK                                              
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 B-KOLLA-NYCKLAR SECTION.                                                 
028700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028800     MOVE '001'             TO MSGI-KDCALL                                
028900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029000     MOVE '4366'            TO MSGI-IDTRANS                               
029100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029110     IF EGEN-TRANS                                                        
029120        MOVE MID-IDPRCTAB-IN TO MSGI-IDPRCTAB                             
029130     END-IF                                                               
029200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029210     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
029300                                                                          
029400     IF MSGI-IDLAND-SPR = 'GB'                                            
029500       MOVE +2    TO SPRAK-IX                                             
029600       MOVE 'GB ' TO MED-IDSKYLT                                          
029700     ELSE                                                                 
029800       MOVE 'S  ' TO MED-IDSKYLT                                          
029900       MOVE +1    TO SPRAK-IX                                             
030000     END-IF                                                               
030100                                                                          
030200     IF GODK-TRANS                                                        
030300       MOVE JA TO NYCKLAR-SW                                              
030400       MOVE MFS-RENSA-FAELT TO MOD-IDPRCTAB-IN                            
030500                               MOD-IDDC-IN                                
030600                                                                          
030700       IF MID-IDPRCTAB-IN = ALL '+'                                       
030800         MOVE MID-IDPRCTAB-UT TO IDPRCTAB-WS                              
030900         INSPECT IDPRCTAB-WS REPLACING LEADING SPACE BY ZERO              
031000       ELSE                                                               
031100         MOVE MID-IDPRCTAB-IN TO IDPRCTAB-WS                              
031200         MOVE '7'           TO MFS-IDPFK                                  
031300         MOVE SPACE         TO MFS-KDTRTYP                                
031400       END-IF                                                             
031500                                                                          
031600       IF IDPRCTAB-WS NUMERIC AND IDPRCTAB-WS > ZERO                      
031700         MOVE IDPRCTAB-WS      TO W-IDPRCTAB                              
031800       ELSE                                                               
031900         MOVE NEJ TO NYCKLAR-SW                                           
032000       END-IF                                                             
032100                                                                          
032200       MOVE MSGI-IDDC               TO W-IDDC-B6                          
032300       PERFORM IMS-GU-WDB601                                              
032400                                                                          
032500       IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                       
032600         MOVE NEJ                     TO NYCKLAR-SW                       
032700       END-IF                                                             
032800                                                                          
032900       IF NYCKLAR-OK                                                      
033000         MOVE DCS-IDDC         TO W-IDDC-4445                             
033100                                  W-IDDC-4447                             
033200                                  W-IDDC-4451                             
033300       END-IF                                                             
033400                                                                          
033500       IF GODK-TRANS OR NYCKLAR-OK                                        
033600         MOVE IDPRCTAB-WS      TO MOD-IDPRCTAB-UT                         
033700         MOVE DCS-IDDC         TO MOD-IDDC-UT                             
033800         INSPECT MOD-IDPRCTAB-UT REPLACING LEADING ZERO BY SPACE          
033900       ELSE                                                               
034000         MOVE MFS-RENSA-FAELT  TO MOD-IDPRCTAB-UT                         
034100                                  MOD-IDDC-UT                             
034200       END-IF                                                             
034300                                                                          
034400       IF NYCKLAR-FEL                                                     
034500         MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                             
034600         CALL WMEDKONV USING MED-WMEDAREA                                 
034700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
034800         PERFORM MFS-RENSA-FAELT-IN                                       
034900         PERFORM MFS-RENSA-FAELT-UT                                       
035000       END-IF                                                             
035100     ELSE                                                                 
035200       MOVE NEJ TO NYCKLAR-SW                                             
035300       MOVE MFS-RENSA-FAELT TO MOD-IDPRCTAB-IN                            
035400       PERFORM MFS-RENSA-FAELT-IN                                         
035500       PERFORM MFS-RENSA-FAELT-UT                                         
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 C-FOERSTA-SIDAN SECTION.                                                 
035910                                                                          
036000     MOVE ZERO TO W-IDRADNR                                               
036100     MOVE 1    TO PGNO                                                    
036110     MOVE JA   TO FIRST-SW                                                
036200     MOVE INFO-FOERSTA-SIDAN TO MED-IDMFSFEL                              
036300     CALL WMEDKONV USING MED-WMEDAREA                                     
036400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
036500     PERFORM MFS-RENSA-FAELT-IN                                           
036600     .                                                                    
036700     EJECT                                                                
036800 D-NAESTA-SIDAN SECTION.                                                  
037110     IF SAVE-IDTRANS = '4366'                                             
037122       IF  SAVE-IDRADNR-NEXT = SAVE-IDRADNR-ENTER(PGNO)                   
037126         CONTINUE                                                         
037127       ELSE                                                               
037128         IF PGNO = 20                                                     
037130           PERFORM VARYING PGNO FROM 1 BY 1                               
037131           UNTIL PGNO = 20                                                
037132           MOVE  SAVE-IDRADNR-ENTER(PGNO + 1) TO                          
037133                                        SAVE-IDRADNR-ENTER(PGNO)          
037134           END-PERFORM                                                    
037135           MOVE NEJ TO FIRST-SW                                           
037136         ELSE                                                             
037190           COMPUTE PGNO = PGNO + 1                                        
037191         END-IF                                                           
037192       END-IF                                                             
037193       MOVE SAVE-IDRADNR-NEXT             TO W-IDRADNR                    
037200       PERFORM MFS-RENSA-FAELT-IN                                         
037202     END-IF                                                               
037203     .                                                                    
037204     EJECT                                                                
037400 E-SAMMA-SIDA SECTION.                                                    
037600     MOVE SAVE-IDRADNR-ENTER(PGNO) TO W-IDRADNR                           
037700     IF MID-IDRADNR-IN NOT = ALL '+' OR MID-INPUT NOT = ALL '+'           
037800       MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                               
037900       CALL WMEDKONV USING MED-WMEDAREA                                   
038000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
038200       PERFORM MFS-LAES-IN-IGEN                                           
038300     ELSE                                                                 
038400       PERFORM MFS-RENSA-FAELT-IN                                         
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 G-UPPDATERA SECTION.                                                     
038900                                                                          
038910     MOVE IDPRCTAB-WS      TO W-IDPRCTAB                                  
038920     PERFORM IMS-GET-XXKG-TABELL                                          
039000     IF SEGMENT-SAKNAS                                                    
039100       PERFORM GA-SKAPA-NY-TABELL                                         
039200       PERFORM IMS-GET-XXKG-TABELL                                        
039300     END-IF                                                               
039400                                                                          
039500     PERFORM GB-KOLLA-INPUT                                               
039600                                                                          
039700     IF INDATA-OK AND UPPDATERING-OK                                      
039800       PERFORM IMS-GET-XXKG-RAD-UNIK                                      
039900                                                                          
040000       IF SEGMENT-FINNS                                                   
040100                                                                          
040200         IF MID-INPUT = ALL '+'                                           
040300           PERFORM IMS-DLET-XXKG-RAD                                      
040400           MOVE JA TO UPPDAT-SW                                           
040500                                                                          
040600         ELSE                                                             
040700           PERFORM GC-AENDRA-RAD                                          
040800         END-IF                                                           
040900                                                                          
041000       ELSE                                                               
041100         IF MID-INPUT = ALL '+'                                           
041200           MOVE SAVE-IDRADNR-ENTER(PGNO)  TO W-IDRADNR                    
041300           MOVE FELM-PF11-O-TOM-INDATARAD TO MED-IDMFSFEL                 
041400           CALL WMEDKONV USING MED-WMEDAREA                               
041500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
041600           MOVE NEJ        TO UPPDAT-SW                                   
041700         ELSE                                                             
041800           PERFORM GD-SKAPA-NY-RAD                                        
041900         END-IF                                                           
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     PERFORM S01-LAES-VISA-TABELL                                         
042500                                                                          
042600     IF UPPDATERING-OK                                                    
042700       MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                             
042800       CALL WMEDKONV USING MED-WMEDAREA                                   
042900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
043000       PERFORM MFS-FORM-ATTR                                              
043100       PERFORM MFS-RENSA-FAELT-IN                                         
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 GA-SKAPA-NY-TABELL SECTION.                                              
043600                                                                          
043700     MOVE '4445'          TO XXKG-4445-IDHTYP                             
043800     MOVE W-IDDC-4445     TO XXKG-4445-IDDC                               
043900     MOVE W-IDPRCTAB      TO XXKG-4445-IDPRCTAB                           
044000     MOVE LOW-VALUE       TO XXKG-4445-LOW-VALUE                          
044100     PERFORM IMS-ISRT-XXKG-TABELL                                         
044200                                                                          
044300     MOVE NEJ TO NYTABELL-SW                                              
044400     MOVE +1 TO W-IDPRCTAB                                                
044500     MOVE +99999 TO W-IDRADNR                                             
044600     PERFORM IMS-GET-XXKG-DEFAULT-RAD                                     
044700     MOVE DLI-IO-AREA TO WS-IO-AREA                                       
044800                                                                          
044900     IF SEGMENT-FINNS                                                     
045000       MOVE IDPRCTAB-WS TO W-IDPRCTAB                                     
045100       PERFORM IMS-GET-XXKG-TABELL                                        
045200       IF SEGMENT-FINNS                                                   
045300          MOVE WS-IO-AREA TO DLI-IO-AREA                                  
045400          PERFORM IMS-ISRT-XXKG-RAD                                       
045500          MOVE JA TO NYTABELL-SW                                          
045600       END-IF                                                             
045700       IF MID-IDRADNR-IN = ALL '+' AND MID-INPUT = ALL '+'                
045800         PERFORM MFS-RENSA-FAELT-IN                                       
045900       END-IF                                                             
046000                                                                          
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400 GB-KOLLA-INPUT SECTION.                                                  
046500                                                                          
046600     MOVE JA                       TO INDATA-SW                           
046710     IF SAVE-IDRADNR-ENTER(PGNO) NUMERIC                                  
046711       MOVE SAVE-IDRADNR-ENTER(PGNO) TO W-IDRADNR                         
046720     ELSE                                                                 
046721       MOVE ZERO                   TO W-IDRADNR                           
046722       MOVE ZERO                   TO SAVE-IDRADNR-ENTER(PGNO)            
046730     END-IF                                                               
046800                                                                          
046900     IF IDPRCTAB-WS = '01'                                                
047000       MOVE NEJ               TO UPPDAT-SW                                
047100       PERFORM MFS-RENSA-FAELT-IN                                         
047200       MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                        
047300       CALL WMEDKONV USING MED-WMEDAREA                                   
047400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
047500                                                                          
047600     ELSE                                                                 
047700                                                                          
047800       IF MID-IDRADNR-IN = ALL '+' AND MID-INPUT = ALL '+'                
047900         MOVE FELM-PF11-O-TOM-INDATARAD TO MED-IDMFSFEL                   
048000         CALL WMEDKONV USING MED-WMEDAREA                                 
048100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
048200         PERFORM MFS-RENSA-FAELT-IN                                       
048300         MOVE NEJ        TO UPPDAT-SW                                     
048400                                                                          
048500       ELSE                                                               
048600         IF MID-IDRADNR-IN NOT = ALL '+'                                  
048700           IF MID-IDRADNR-IN NOT NUMERIC                                  
048800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                
048900             MOVE NEJ               TO INDATA-SW                          
049000           ELSE                                                           
049100             IF MID-IDRADNR-IN = +9999                                    
049200               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR              
049300               MOVE NEJ               TO INDATA-SW                        
049400             ELSE                                                         
049500               IF MID-IDRADNR-IN < +1                                     
049600                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR            
049700                 MOVE NEJ         TO INDATA-SW                            
049800               ELSE                                                       
049900                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR          
050000                 MOVE MID-IDRADNR-IN      TO W-IDRADNR                    
050100               END-IF                                                     
050200             END-IF                                                       
050300           END-IF                                                         
050400                                                                          
050500         ELSE                                                             
050600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                  
050700           MOVE NEJ               TO INDATA-SW                            
050800         END-IF                                                           
050900                                                                          
051000         IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+'                            
051100           IF MID-IDGMTOMR-FOM-UPP NOT NUMERIC                            
051200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-F-UPP-ATTR            
051300             MOVE NEJ               TO INDATA-SW                          
051400           ELSE                                                           
051500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-F-UPP-ATTR          
051600           END-IF                                                         
051700         END-IF                                                           
051800                                                                          
051900         IF MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                            
052000           IF MID-IDGMTOMR-TOM-UPP NOT NUMERIC                            
052100             MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-T-UPP-ATTR            
052200             MOVE NEJ               TO INDATA-SW                          
052300           ELSE                                                           
052400             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-T-UPP-ATTR          
052500           END-IF                                                         
052600         END-IF                                                           
052700                                                                          
052800         IF MID-KDPRODKL-FOM-UPP NOT = ALL '+'                            
052900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-F-UPP-ATTR           
053000         END-IF                                                           
053100                                                                          
053200         IF MID-KDPRODKL-TOM-UPP NOT = ALL '+'                            
053300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-T-UPP-ATTR           
053400         END-IF                                                           
053500                                                                          
053600         IF MID-KDFRAKT-FOM-UPP NOT = ALL '+'                             
053700           IF MID-KDFRAKT-FOM-UPP NOT NUMERIC                             
053800             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-F-UPP-ATTR             
053900             MOVE NEJ               TO INDATA-SW                          
054000           ELSE                                                           
054100             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-F-UPP-ATTR           
054200           END-IF                                                         
054300         END-IF                                                           
054400                                                                          
054500         IF MID-KDFRAKT-TOM-UPP NOT = ALL '+'                             
054600           IF MID-KDFRAKT-TOM-UPP NOT NUMERIC                             
054700             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-T-UPP-ATTR             
054800             MOVE NEJ               TO INDATA-SW                          
054900           ELSE                                                           
055000             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-T-UPP-ATTR           
055100           END-IF                                                         
055200         END-IF                                                           
055300                                                                          
055400         IF MID-IDTRP-UPP NOT = ALL '+'                                   
055500           IF MID-IDTRP-UPP NOT NUMERIC                                   
055600             MOVE MFS-NUM-FAELT-FEL TO MOD-IDTRP-UPP-ATTR                 
055700             MOVE NEJ               TO INDATA-SW                          
055800           ELSE                                                           
055900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTRP-UPP-ATTR               
056000           END-IF                                                         
056100         END-IF                                                           
056200                                                                          
056300         IF MID-IDHLO-FOM-UPP NOT = ALL '+'                               
056400           IF MID-IDHLO-FOM-UPP NOT NUMERIC                               
056500             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-F-UPP-ATTR               
056600             MOVE NEJ               TO INDATA-SW                          
056700           ELSE                                                           
056800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-F-UPP-ATTR             
056900           END-IF                                                         
057000         END-IF                                                           
057100                                                                          
057200         IF MID-IDHLO-TOM-UPP NOT = ALL '+'                               
057300           IF MID-IDHLO-TOM-UPP NOT NUMERIC                               
057400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-T-UPP-ATTR               
057500             MOVE NEJ               TO INDATA-SW                          
057600           ELSE                                                           
057700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-T-UPP-ATTR             
057800           END-IF                                                         
057900         END-IF                                                           
058000                                                                          
058100         IF MID-IDPTIDTAB-UPP NOT = ALL '+'                               
058200           IF MID-IDPTIDTAB-UPP NOT NUMERIC                               
058300             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPTIDTAB-UPP-ATTR          
058400             MOVE NEJ                  TO INDATA-SW                       
058500           ELSE                                                           
058600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPTIDTAB-UPP-ATTR           
058700           END-IF                                                         
058800         END-IF                                                           
058900                                                                          
059000         IF MID-IDPRC-UPP NOT = ALL '+'                                   
059100           MOVE MID-IDPRC-UPP TO WS-IDPRC                                 
059200           IF WS-IDPRCBAS NOT NUMERIC                                     
059300             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRC-UPP-ATTR              
059400             MOVE NEJ                  TO INDATA-SW                       
059500           ELSE                                                           
059600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-UPP-ATTR              
059700           END-IF                                                         
059800         END-IF                                                           
059900                                                                          
060000         IF INDATA-FEL                                                    
060100           PERFORM GBA-VISA-INPUTFEL                                      
060200           MOVE NEJ TO UPPDAT-SW                                          
060300         ELSE                                                             
060400           MOVE JA TO UPPDAT-SW                                           
060500         END-IF                                                           
060600       END-IF                                                             
060700     END-IF                                                               
060800     .                                                                    
060900     EJECT                                                                
061000 GBA-VISA-INPUTFEL SECTION.                                               
061100                                                                          
061200     IF MID-IDRADNR-IN = +9999                                            
061300       MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                        
061400     ELSE                                                                 
061500       IF INDATA-FEL                                                      
061600         MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                     
061700       END-IF                                                             
061800     END-IF                                                               
061900                                                                          
062000     CALL WMEDKONV USING MED-WMEDAREA                                     
062100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
062200     MOVE SAVE-IDRADNR-ENTER(PGNO) TO W-IDRADNR                           
062300     PERFORM MFS-ROER-EJ-FAELT-IN                                         
062400     .                                                                    
062500     EJECT                                                                
062600 GC-AENDRA-RAD SECTION.                                                   
062700                                                                          
062800     PERFORM GCA-KOLLA-ANDRAD-RAD                                         
062900                                                                          
063000     IF INDATA-FEL                                                        
063100       MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                       
063200       CALL WMEDKONV USING MED-WMEDAREA                                   
063300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
063400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
063500       MOVE SAVE-IDRADNR-ENTER(PGNO) TO W-IDRADNR                         
063600       MOVE NEJ TO UPPDAT-SW                                              
063700     ELSE                                                                 
063800       PERFORM GCB-ANDRING-I-DB                                           
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 GCA-KOLLA-ANDRAD-RAD SECTION.                                            
064300                                                                          
064400     MOVE JA TO INDATA-SW                                                 
064500     MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR                      
064600                                                                          
064700     IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+' AND                            
064800        MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                                
064900                                                                          
065000       IF MID-IDGMTOMR-FOM-UPP > MID-IDGMTOMR-TOM-UPP                     
065100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-F-UPP-ATTR                
065200         MOVE NEJ                TO INDATA-SW                             
065300       ELSE                                                               
065400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-F-UPP-ATTR              
065500       END-IF                                                             
065600                                                                          
065700     ELSE                                                                 
065800       IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+' AND                          
065900          MID-IDGMTOMR-TOM-UPP = ALL '+'                                  
066000                                                                          
066100         IF MID-IDGMTOMR-FOM-UPP > XXKG-4446-IDGMTOMR-TOM                 
066200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-F-UPP-ATTR              
066300           MOVE NEJ              TO INDATA-SW                             
066400         ELSE                                                             
066500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-F-UPP-ATTR            
066600         END-IF                                                           
066700       ELSE                                                               
066800         IF MID-IDGMTOMR-FOM-UPP = ALL '+' AND                            
066900            MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                            
067000                                                                          
067100           IF MID-IDGMTOMR-TOM-UPP < XXKG-4446-IDGMTOMR-FOM               
067200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-T-UPP-ATTR            
067300             MOVE NEJ            TO INDATA-SW                             
067400           ELSE                                                           
067500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-T-UPP-ATTR          
067600           END-IF                                                         
067700         END-IF                                                           
067800       END-IF                                                             
067900     END-IF                                                               
068000                                                                          
068100     IF MID-KDPRODKL-FOM-UPP NOT = ALL '+' AND                            
068200        MID-KDPRODKL-TOM-UPP NOT = ALL '+'                                
068300                                                                          
068400       IF MID-KDPRODKL-FOM-UPP > MID-KDPRODKL-TOM-UPP                     
068500         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-F-UPP-ATTR               
068600         MOVE NEJ                TO INDATA-SW                             
068700       ELSE                                                               
068800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-F-UPP-ATTR             
068900       END-IF                                                             
069000                                                                          
069100     ELSE                                                                 
069200       IF MID-KDPRODKL-FOM-UPP NOT = ALL '+' AND                          
069300          MID-KDPRODKL-TOM-UPP = ALL '+'                                  
069400                                                                          
069500         IF MID-KDPRODKL-FOM-UPP > XXKG-4446-KDPRODKL-TOM                 
069600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-F-UPP-ATTR             
069700           MOVE NEJ              TO INDATA-SW                             
069800         ELSE                                                             
069900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-F-UPP-ATTR           
070000         END-IF                                                           
070100       ELSE                                                               
070200         IF MID-KDPRODKL-FOM-UPP = ALL '+' AND                            
070300            MID-KDPRODKL-TOM-UPP NOT = ALL '+'                            
070400                                                                          
070500           IF MID-KDPRODKL-TOM-UPP < XXKG-4446-KDPRODKL-FOM               
070600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-T-UPP-ATTR           
070700             MOVE NEJ            TO INDATA-SW                             
070800           ELSE                                                           
070900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-T-UPP-ATTR         
071000           END-IF                                                         
071100         END-IF                                                           
071200       END-IF                                                             
071300     END-IF                                                               
071400                                                                          
071500     IF MID-KDFRAKT-FOM-UPP NOT = ALL '+' AND                             
071600        MID-KDFRAKT-TOM-UPP NOT = ALL '+'                                 
071700                                                                          
071800       IF MID-KDFRAKT-FOM-UPP > MID-KDFRAKT-TOM-UPP                       
071900         MOVE MFS-NUM-FAELT-FEL  TO MOD-KDFRAKT-F-UPP-ATTR                
072000         MOVE NEJ                TO INDATA-SW                             
072100       ELSE                                                               
072200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-F-UPP-ATTR               
072300       END-IF                                                             
072400                                                                          
072500     ELSE                                                                 
072600       IF MID-KDFRAKT-FOM-UPP NOT = ALL '+' AND                           
072700          MID-KDFRAKT-TOM-UPP = ALL '+'                                   
072800                                                                          
072900         IF MID-KDFRAKT-FOM-UPP > XXKG-4446-KDFRAKT-TOM                   
073000           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-F-UPP-ATTR               
073100           MOVE NEJ               TO INDATA-SW                            
073200         ELSE                                                             
073300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-F-UPP-ATTR             
073400         END-IF                                                           
073500       ELSE                                                               
073600         IF MID-KDFRAKT-FOM-UPP = ALL '+' AND                             
073700            MID-KDFRAKT-TOM-UPP NOT = ALL '+'                             
073800                                                                          
073900           IF MID-KDFRAKT-TOM-UPP < XXKG-4446-KDFRAKT-FOM                 
074000             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-T-UPP-ATTR             
074100             MOVE NEJ               TO INDATA-SW                          
074200           ELSE                                                           
074300             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-T-UPP-ATTR           
074400           END-IF                                                         
074500         END-IF                                                           
074600       END-IF                                                             
074700     END-IF                                                               
074800                                                                          
074900     IF MID-IDTRP-UPP NOT = ALL '+'                                       
075000       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTRP-UPP-ATTR                     
075100     END-IF                                                               
075200                                                                          
075300     IF MID-IDHLO-FOM-UPP NOT = ALL '+' AND                               
075400        MID-IDHLO-TOM-UPP NOT = ALL '+'                                   
075500                                                                          
075600       IF MID-IDHLO-FOM-UPP > MID-IDHLO-TOM-UPP                           
075700         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDHLO-F-UPP-ATTR                  
075800         MOVE NEJ                TO INDATA-SW                             
075900       ELSE                                                               
076000         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-F-UPP-ATTR                 
076100       END-IF                                                             
076200                                                                          
076300     ELSE                                                                 
076400       IF MID-IDHLO-FOM-UPP NOT = ALL '+' AND                             
076500          MID-IDHLO-TOM-UPP = ALL '+'                                     
076600                                                                          
076700         IF MID-IDHLO-FOM-UPP > XXKG-4446-IDHLO-TOM                       
076800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-F-UPP-ATTR                 
076900           MOVE NEJ               TO INDATA-SW                            
077000         ELSE                                                             
077100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-F-UPP-ATTR               
077200         END-IF                                                           
077300       ELSE                                                               
077400         IF MID-IDHLO-FOM-UPP = ALL '+' AND                               
077500            MID-IDHLO-TOM-UPP NOT = ALL '+'                               
077600                                                                          
077700           IF MID-IDHLO-TOM-UPP < XXKG-4446-IDHLO-FOM                     
077800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-T-UPP-ATTR               
077900             MOVE NEJ               TO INDATA-SW                          
078000           ELSE                                                           
078100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-T-UPP-ATTR             
078200           END-IF                                                         
078300         END-IF                                                           
078400       END-IF                                                             
078500     END-IF                                                               
078600                                                                          
078700     IF MID-IDPTIDTAB-UPP NOT = ALL '+'                                   
078800       MOVE MID-IDPTIDTAB-UPP TO W-IDPTIDTAB                              
078900       PERFORM IMS-GET-XXKI-PTIDTAB-UNIK                                  
079000       IF SEGMENT-SAKNAS                                                  
079100         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPTIDTAB-UPP-ATTR              
079200         MOVE NEJ                  TO INDATA-SW                           
079300       ELSE                                                               
079400         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPTIDTAB-UPP-ATTR              
079500       END-IF                                                             
079600     END-IF                                                               
079700                                                                          
079800     IF MID-IDPRC-UPP NOT = ALL '+'                                       
079900       MOVE MID-IDPRC-UPP TO W-IDPRC                                      
080000       PERFORM IMS-GET-XXKH-PRC-UNIK                                      
080100       IF SEGMENT-SAKNAS                                                  
080200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRC-UPP-ATTR                  
080300         MOVE NEJ                  TO INDATA-SW                           
080400       ELSE                                                               
080500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-UPP-ATTR                  
080600       END-IF                                                             
080700     END-IF                                                               
080800     .                                                                    
080900     EJECT                                                                
081000 GCB-ANDRING-I-DB SECTION.                                                
081100                                                                          
081200     MOVE IDPRCTAB-WS TO W-IDPRCTAB                                       
081300     MOVE MID-IDRADNR-IN TO W-IDRADNR                                     
081400     PERFORM IMS-GET-XXKG-RAD-GHU                                         
081500                                                                          
081600     MOVE MID-IDRADNR-IN TO XXKG-4446-IDRADNR                             
081700                                                                          
081800     IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+'                                
081900       MOVE MID-IDGMTOMR-FOM-UPP  TO XXKG-4446-IDGMTOMR-FOM               
082000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDGMTOMR-FOM-ATTR(1)             
082100     END-IF                                                               
082200                                                                          
082300     IF MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                                
082400       MOVE MID-IDGMTOMR-TOM-UPP  TO XXKG-4446-IDGMTOMR-TOM               
082500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDGMTOMR-TOM-ATTR(1)             
082600     END-IF                                                               
082700                                                                          
082800     IF MID-KDPRODKL-FOM-UPP NOT = ALL '+'                                
082900       MOVE MID-KDPRODKL-FOM-UPP  TO XXKG-4446-KDPRODKL-FOM               
083000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODKL-FOM-ATTR(1)             
083100     END-IF                                                               
083200                                                                          
083300     IF MID-KDPRODKL-TOM-UPP NOT = ALL '+'                                
083400       MOVE MID-KDPRODKL-TOM-UPP  TO XXKG-4446-KDPRODKL-TOM               
083500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODKL-TOM-ATTR(1)             
083600     END-IF                                                               
083700                                                                          
083800     IF MID-KDFRAKT-FOM-UPP NOT = ALL '+'                                 
083900       MOVE MID-KDFRAKT-FOM-UPP   TO XXKG-4446-KDFRAKT-FOM                
084000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-FOM-ATTR(1)              
084100     END-IF                                                               
084200                                                                          
084300     IF MID-KDFRAKT-TOM-UPP NOT = ALL '+'                                 
084400       MOVE MID-KDFRAKT-TOM-UPP   TO XXKG-4446-KDFRAKT-TOM                
084500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-TOM-ATTR(1)              
084600     END-IF                                                               
084700                                                                          
084800     IF MID-IDTRP-UPP NOT = ALL '+'                                       
084900       IF MID-IDTRP-UPP = ZERO                                            
085000         MOVE SPACE               TO XXKG-4446-IDTRP                      
085100       ELSE                                                               
085200         MOVE MID-IDTRP-UPP       TO XXKG-4446-IDTRP                      
085300       END-IF                                                             
085400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTRP-ATTR(1)                    
085500     END-IF                                                               
085600                                                                          
085700     IF MID-IDHLO-FOM-UPP NOT = ALL '+'                                   
085800       MOVE MID-IDHLO-FOM-UPP     TO XXKG-4446-IDHLO-FOM                  
085900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDHLO-FOM-ATTR(1)                
086000     END-IF                                                               
086100                                                                          
086200     IF MID-IDHLO-TOM-UPP NOT = ALL '+'                                   
086300       MOVE MID-IDHLO-TOM-UPP     TO XXKG-4446-IDHLO-TOM                  
086400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDHLO-TOM-ATTR(1)                
086500     END-IF                                                               
086600                                                                          
086700     IF MID-IDPTIDTAB-UPP NOT = ALL '+'                                   
086800       MOVE MID-IDPTIDTAB-UPP     TO XXKG-4446-IDPTIDTAB                  
086900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPTIDTAB-ATTR(1)                
087000     END-IF                                                               
087100                                                                          
087200     IF MID-IDPRC-UPP NOT = ALL '+'                                       
087300       MOVE MID-IDPRC-UPP         TO XXKG-4446-IDPRC                      
087400       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-ATTR(1)                    
087500     END-IF                                                               
087600                                                                          
087700     MOVE SPACE                   TO XXKG-4446-FILLER                     
087800                                                                          
087900     PERFORM IMS-REPL-XXKG-RAD                                            
088000     MOVE JA TO UPPDAT-SW                                                 
088100     PERFORM IMS-GET-XXKG-TABELL                                          
088200     .                                                                    
088300     EJECT                                                                
088400 GD-SKAPA-NY-RAD SECTION.                                                 
088500                                                                          
088600     PERFORM GDA-KOLLA-INMATAD-NY-RAD                                     
088700                                                                          
088800     IF INDATA-FEL                                                        
088900       MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                       
089000       CALL WMEDKONV USING MED-WMEDAREA                                   
089100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
089200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
089300       MOVE SAVE-IDRADNR-ENTER(PGNO) TO W-IDRADNR                         
089400       MOVE NEJ TO UPPDAT-SW                                              
089500     ELSE                                                                 
089600       PERFORM GDB-RADTILLAGG-I-DB                                        
089700     END-IF                                                               
089800     .                                                                    
089900     EJECT                                                                
090000 GDA-KOLLA-INMATAD-NY-RAD SECTION.                                        
090100                                                                          
090200     MOVE JA TO INDATA-SW                                                 
090300     MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR                      
090400                                                                          
090500     IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+' AND                            
090600        MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                                
090700                                                                          
090800       IF MID-IDGMTOMR-FOM-UPP > MID-IDGMTOMR-TOM-UPP                     
090900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-F-UPP-ATTR                
091000         MOVE NEJ                TO INDATA-SW                             
091100       ELSE                                                               
091200         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-F-UPP-ATTR              
091300                                      MOD-IDGMTOMR-T-UPP-ATTR             
091400       END-IF                                                             
091500                                                                          
091600     ELSE                                                                 
091700       IF MID-IDGMTOMR-FOM-UPP = ALL '+' AND                              
091800          MID-IDGMTOMR-TOM-UPP = ALL '+'                                  
091900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDGMTOMR-F-UPP-ATTR                
092000         MOVE NEJ                  TO INDATA-SW                           
092100                                                                          
092200       ELSE                                                               
092300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDGMTOMR-F-UPP-ATTR              
092400                                      MOD-IDGMTOMR-T-UPP-ATTR             
092500       END-IF                                                             
092600     END-IF                                                               
092700                                                                          
092800     IF MID-KDPRODKL-FOM-UPP NOT = ALL '+' AND                            
092900        MID-KDPRODKL-TOM-UPP NOT = ALL '+'                                
093000                                                                          
093100       IF MID-KDPRODKL-FOM-UPP > MID-KDPRODKL-TOM-UPP                     
093200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-F-UPP-ATTR               
093300         MOVE NEJ                TO INDATA-SW                             
093400       ELSE                                                               
093500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-F-UPP-ATTR             
093600                                      MOD-KDPRODKL-T-UPP-ATTR             
093700       END-IF                                                             
093800                                                                          
093900     ELSE                                                                 
094000       IF MID-KDPRODKL-FOM-UPP = ALL '+' AND                              
094100          MID-KDPRODKL-TOM-UPP = ALL '+'                                  
094200         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-F-UPP-ATTR               
094300         MOVE NEJ                TO INDATA-SW                             
094400                                                                          
094500       ELSE                                                               
094600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-F-UPP-ATTR             
094700                                      MOD-KDPRODKL-T-UPP-ATTR             
094800       END-IF                                                             
094900     END-IF                                                               
095000                                                                          
095100     IF MID-KDFRAKT-FOM-UPP NOT = ALL '+' AND                             
095200        MID-KDFRAKT-TOM-UPP NOT = ALL '+'                                 
095300                                                                          
095400       IF MID-KDFRAKT-FOM-UPP > MID-KDFRAKT-TOM-UPP                       
095500         MOVE MFS-NUM-FAELT-FEL  TO MOD-KDFRAKT-F-UPP-ATTR                
095600         MOVE NEJ                TO INDATA-SW                             
095700       ELSE                                                               
095800         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-F-UPP-ATTR               
095900                                     MOD-KDFRAKT-T-UPP-ATTR               
096000       END-IF                                                             
096100                                                                          
096200     ELSE                                                                 
096300       IF MID-KDFRAKT-FOM-UPP = ALL '+' AND                               
096400          MID-KDFRAKT-TOM-UPP = ALL '+'                                   
096500         MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-F-UPP-ATTR                 
096600         MOVE NEJ                TO INDATA-SW                             
096700                                                                          
096800       ELSE                                                               
096900         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-F-UPP-ATTR               
097000                                     MOD-KDFRAKT-T-UPP-ATTR               
097100       END-IF                                                             
097200     END-IF                                                               
097300                                                                          
097400     MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTRP-UPP-ATTR                       
097500                                                                          
097600     IF MID-IDHLO-FOM-UPP NOT = ALL '+' AND                               
097700        MID-IDHLO-TOM-UPP NOT = ALL '+'                                   
097800                                                                          
097900       IF MID-IDHLO-FOM-UPP > MID-IDHLO-TOM-UPP                           
098000         MOVE MFS-NUM-FAELT-FEL  TO MOD-IDHLO-F-UPP-ATTR                  
098100         MOVE NEJ                TO INDATA-SW                             
098200       ELSE                                                               
098300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-F-UPP-ATTR                 
098400                                     MOD-IDHLO-T-UPP-ATTR                 
098500       END-IF                                                             
098600                                                                          
098700     ELSE                                                                 
098800       IF MID-IDHLO-FOM-UPP = ALL '+' AND                                 
098900          MID-IDHLO-TOM-UPP = ALL '+'                                     
099000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-F-UPP-ATTR                   
099100         MOVE NEJ                TO INDATA-SW                             
099200                                                                          
099300       ELSE                                                               
099400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-F-UPP-ATTR                 
099500                                     MOD-IDHLO-T-UPP-ATTR                 
099600       END-IF                                                             
099700     END-IF                                                               
099800                                                                          
099900     IF MID-IDPTIDTAB-UPP NOT = ALL '+'                                   
100000       MOVE MID-IDPTIDTAB-UPP TO W-IDPTIDTAB                              
100100       PERFORM IMS-GET-XXKI-PTIDTAB-UNIK                                  
100200       IF SEGMENT-SAKNAS                                                  
100300         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPTIDTAB-UPP-ATTR              
100400         MOVE NEJ                  TO INDATA-SW                           
100500       ELSE                                                               
100600         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPTIDTAB-UPP-ATTR              
100700       END-IF                                                             
100800     ELSE                                                                 
100900       MOVE MFS-NUM-FAELT-FEL      TO MOD-IDPTIDTAB-UPP-ATTR              
101000       MOVE NEJ                    TO INDATA-SW                           
101100     END-IF                                                               
101200                                                                          
101300     IF MID-IDPRC-UPP NOT = ALL '+'                                       
101400       MOVE MID-IDPRC-UPP TO W-IDPRC                                      
101500       PERFORM IMS-GET-XXKH-PRC-UNIK                                      
101600       IF SEGMENT-SAKNAS                                                  
101700         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRC-UPP-ATTR                    
101800         MOVE NEJ                    TO INDATA-SW                         
101900       ELSE                                                               
102000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRC-UPP-ATTR                  
102100       END-IF                                                             
102200     ELSE                                                                 
102300       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRC-UPP-ATTR                    
102400       MOVE NEJ                  TO INDATA-SW                             
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800 GDB-RADTILLAGG-I-DB SECTION.                                             
102900                                                                          
103000     MOVE LOW-VALUE             TO XXKG-4446-LOW-VALUE                    
103100     MOVE MID-IDRADNR-IN        TO XXKG-4446-IDRADNR                      
103200     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDRADNR-ATTR(1)                    
103300                                                                          
103400     IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+' AND                            
103500        MID-IDGMTOMR-TOM-UPP NOT = ALL '+'                                
103600       MOVE MID-IDGMTOMR-FOM-UPP  TO XXKG-4446-IDGMTOMR-FOM               
103700       MOVE MID-IDGMTOMR-TOM-UPP  TO XXKG-4446-IDGMTOMR-TOM               
103800                                                                          
103900     ELSE                                                                 
104000       IF MID-IDGMTOMR-FOM-UPP NOT = ALL '+' AND                          
104100          MID-IDGMTOMR-TOM-UPP = ALL '+'                                  
104200         MOVE MID-IDGMTOMR-FOM-UPP TO XXKG-4446-IDGMTOMR-FOM              
104300                                      XXKG-4446-IDGMTOMR-TOM              
104400                                                                          
104500       ELSE                                                               
104600         MOVE MID-IDGMTOMR-TOM-UPP TO XXKG-4446-IDGMTOMR-FOM              
104700                                      XXKG-4446-IDGMTOMR-TOM              
104800       END-IF                                                             
104900     END-IF                                                               
105000     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDGMTOMR-FOM-ATTR(1)               
105100                                   MOD-IDGMTOMR-TOM-ATTR(1)               
105200                                                                          
105300     IF MID-KDPRODKL-FOM-UPP NOT = ALL '+' AND                            
105400        MID-KDPRODKL-TOM-UPP NOT = ALL '+'                                
105500       MOVE MID-KDPRODKL-FOM-UPP  TO XXKG-4446-KDPRODKL-FOM               
105600       MOVE MID-KDPRODKL-TOM-UPP  TO XXKG-4446-KDPRODKL-TOM               
105700                                                                          
105800     ELSE                                                                 
105900       IF MID-KDPRODKL-FOM-UPP NOT = ALL '+' AND                          
106000          MID-KDPRODKL-TOM-UPP = ALL '+'                                  
106100         MOVE MID-KDPRODKL-FOM-UPP TO XXKG-4446-KDPRODKL-FOM              
106200                                      XXKG-4446-KDPRODKL-TOM              
106300                                                                          
106400       ELSE                                                               
106500         MOVE MID-KDPRODKL-TOM-UPP TO XXKG-4446-KDPRODKL-FOM              
106600                                      XXKG-4446-KDPRODKL-TOM              
106700                                                                          
106800       END-IF                                                             
106900     END-IF                                                               
107000     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODKL-FOM-ATTR(1)               
107100                                   MOD-KDPRODKL-TOM-ATTR(1)               
107200                                                                          
107300     IF MID-KDFRAKT-FOM-UPP NOT = ALL '+' AND                             
107400        MID-KDFRAKT-TOM-UPP NOT = ALL '+'                                 
107500       MOVE MID-KDFRAKT-FOM-UPP   TO XXKG-4446-KDFRAKT-FOM                
107600       MOVE MID-KDFRAKT-TOM-UPP   TO XXKG-4446-KDFRAKT-TOM                
107700                                                                          
107800     ELSE                                                                 
107900       IF MID-KDFRAKT-FOM-UPP NOT = ALL '+' AND                           
108000          MID-KDFRAKT-TOM-UPP = ALL '+'                                   
108100         MOVE MID-KDFRAKT-FOM-UPP TO XXKG-4446-KDFRAKT-FOM                
108200                                     XXKG-4446-KDFRAKT-TOM                
108300                                                                          
108400       ELSE                                                               
108500         MOVE MID-KDFRAKT-TOM-UPP TO XXKG-4446-KDFRAKT-FOM                
108600                                     XXKG-4446-KDFRAKT-TOM                
108700       END-IF                                                             
108800     END-IF                                                               
108900     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-FOM-ATTR(1)                
109000     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFRAKT-TOM-ATTR(1)                
109100                                                                          
109200     IF MID-IDTRP-UPP NOT = ALL '+'                                       
109300       MOVE MID-IDTRP-UPP         TO XXKG-4446-IDTRP                      
109400     ELSE                                                                 
109500       MOVE SPACE                 TO XXKG-4446-IDTRP                      
109600     END-IF                                                               
109700     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDTRP-ATTR(1)                      
109800                                                                          
109900     IF MID-IDHLO-FOM-UPP NOT = ALL '+' AND                               
110000        MID-IDHLO-TOM-UPP NOT = ALL '+'                                   
110100       MOVE MID-IDHLO-FOM-UPP     TO XXKG-4446-IDHLO-FOM                  
110200       MOVE MID-IDHLO-TOM-UPP     TO XXKG-4446-IDHLO-TOM                  
110300                                                                          
110400     ELSE                                                                 
110500       IF MID-IDHLO-FOM-UPP NOT = ALL '+' AND                             
110600          MID-IDHLO-TOM-UPP = ALL '+'                                     
110700         MOVE MID-IDHLO-FOM-UPP   TO XXKG-4446-IDHLO-FOM                  
110800                                     XXKG-4446-IDHLO-TOM                  
110900                                                                          
111000       ELSE                                                               
111100        MOVE MID-IDHLO-TOM-UPP    TO XXKG-4446-IDHLO-FOM                  
111200                                     XXKG-4446-IDHLO-TOM                  
111300       END-IF                                                             
111400     END-IF                                                               
111500     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDHLO-FOM-ATTR(1)                  
111600     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDHLO-TOM-ATTR(1)                  
111700                                                                          
111800     IF MID-IDPTIDTAB-UPP NOT = ALL '+'                                   
111900       MOVE MID-IDPTIDTAB-UPP     TO XXKG-4446-IDPTIDTAB                  
112000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPTIDTAB-ATTR(1)                
112100     END-IF                                                               
112200                                                                          
112300     IF MID-IDPRC-UPP NOT = ALL '+'                                       
112400       MOVE MID-IDPRC-UPP         TO XXKG-4446-IDPRC                      
112500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPRC-ATTR(1)                    
112600     END-IF                                                               
112700                                                                          
112800     PERFORM IMS-ISRT-XXKG-RAD                                            
112900     MOVE JA TO UPPDAT-SW                                                 
113000     .                                                                    
113100     EJECT                                                                
113110 I-PREV-SIDE SECTION.                                                     
113120     IF SAVE-IDTRANS = '4366'                                             
113130       IF PGNO > 1                                                        
113131         COMPUTE PGNO = PGNO - 1                                          
113132       END-IF                                                             
113200       IF PGNO = 1                                                        
113201         IF FIRST-SW = JA                                                 
113202           MOVE INFO-FOERSTA-SIDAN       TO MED-IDMFSFEL                  
113203           CALL WMEDKONV              USING MED-WMEDAREA                  
113204           MOVE MED-MFSFEL               TO MOD-TEMFSFEL                  
113205         END-IF                                                           
113206       END-IF                                                             
113207       MOVE SAVE-IDRADNR-ENTER(PGNO)  TO W-IDRADNR                        
113208     END-IF                                                               
113212     .                                                                    
113213     EJECT                                                                
113220 S01-LAES-VISA-TABELL SECTION.                                            
113224     MOVE IDPRCTAB-WS      TO W-IDPRCTAB                                  
113225     PERFORM IMS-GET-XXKG-TABELL                                          
114300       IF SEGMENT-FINNS                                                   
114302         CONTINUE                                                         
116100       ELSE                                                               
116102         MOVE NEJ TO LAES-SW                                              
116103         MOVE FELM-TABELL-SAKNAS TO MED-IDMFSFEL                          
116104         CALL WMEDKONV USING MED-WMEDAREA                                 
116105         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
116106         PERFORM MFS-RENSA-FAELT-IN                                       
116107         PERFORM MFS-RENSA-FAELT-UT                                       
116300       END-IF                                                             
116302                                                                          
116303     IF LAES-SW = JA                                                      
116305         MOVE +1 TO INDX                                                  
116306         PERFORM IMS-GET-XXKG-RAD-FIRST                                   
116307                                                                          
116308         IF SEGMENT-FINNS                                                 
116309            MOVE XXKG-4446-IDRADNR   TO SAVE-IDRADNR-ENTER(PGNO)          
116310         ELSE                                                             
116311            MOVE ZERO                TO SAVE-IDRADNR-ENTER(PGNO)          
116312         END-IF                                                           
116313                                                                          
116314         PERFORM UNTIL INDX > MAX-TABRADER                                
116315          IF SEGMENT-FINNS                                                
116316            MOVE XXKG-4446-IDRADNR       TO MOD-IDRADNR(INDX)             
116317            MOVE XXKG-4446-IDGMTOMR-FOM  TO MOD-IDGMTOMR-FOM(INDX)        
116318            MOVE STRECK               TO MOD-IDGMTOMR-STRECK(INDX)        
116319            MOVE XXKG-4446-IDGMTOMR-TOM  TO MOD-IDGMTOMR-TOM(INDX)        
116320            MOVE XXKG-4446-KDPRODKL-FOM  TO MOD-KDPRODKL-FOM(INDX)        
116321            MOVE STRECK               TO MOD-KDPRODKL-STRECK(INDX)        
116322            MOVE XXKG-4446-KDPRODKL-TOM  TO MOD-KDPRODKL-TOM(INDX)        
116323            MOVE XXKG-4446-KDFRAKT-FOM   TO MOD-KDFRAKT-FOM(INDX)         
116324            MOVE STRECK                TO MOD-KDFRAKT-STRECK(INDX)        
116325            MOVE XXKG-4446-KDFRAKT-TOM   TO MOD-KDFRAKT-TOM(INDX)         
116326            MOVE XXKG-4446-IDTRP         TO MOD-IDTRP(INDX)               
116327            MOVE XXKG-4446-IDHLO-FOM     TO MOD-IDHLO-FOM(INDX)           
116328            MOVE STRECK               TO MOD-IDHLO-STRECK(INDX)           
116329            MOVE XXKG-4446-IDHLO-TOM     TO MOD-IDHLO-TOM(INDX)           
116330            MOVE XXKG-4446-IDPTIDTAB     TO MOD-IDPTIDTAB(INDX)           
116331            MOVE XXKG-4446-IDPRC         TO MOD-IDPRC(INDX)               
116332            PERFORM IMS-GET-XXKG-RAD                                      
116333          ELSE                                                            
116334            PERFORM MFS-RENSA-RADFAELT                                    
116335          END-IF                                                          
116400          ADD +1 TO INDX                                                  
116500         END-PERFORM                                                      
116510         PERFORM  S02-KOLLA-OM-FLER-SIDOR                                 
116590     END-IF                                                               
116591                                                                          
116592     MOVE '002'     TO MSGI-KDCALL                                        
116593     MOVE '4366'    TO MSGI-IDTRANS                                       
116594     MOVE '4366'    TO SAVE-IDTRANS                                       
116595     MOVE SAVE-AREA TO MSGI-SPAR-AREA                                     
116596     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
116600     .                                                                    
116700     EJECT                                                                
116800 S02-KOLLA-OM-FLER-SIDOR SECTION.                                         
116900                                                                          
117000     IF SEGMENT-FINNS                                                     
117100       MOVE XXKG-4446-IDRADNR TO SAVE-IDRADNR-NEXT                        
117200       MOVE INFO-MER-INFO-FINNS-PF8 TO MED-IDMFSINF                       
117300       CALL WMEDKONV USING MED-WMEDAREA                                   
117400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
117500     ELSE                                                                 
117510         MOVE INFO-LAST-PAGE           TO MED-IDMFSFEL                    
117520         CALL WMEDKONV              USING MED-WMEDAREA                    
117530         MOVE MED-MFSFEL               TO MOD-TEMFSFEL                    
117600         MOVE SAVE-IDRADNR-ENTER(PGNO) TO SAVE-IDRADNR-NEXT               
117700       IF NOT MFS-FIRST                                                   
117800         IF NYTABELL-OK                                                   
117900           MOVE INFO-GEN-TAB-UPPLAGD TO MED-IDMFSINF                      
118000           CALL WMEDKONV USING MED-WMEDAREA                               
118100           MOVE MED-MFSINF TO MOD-TEMFSINF                                
118200         END-IF                                                           
118300       END-IF                                                             
118400     END-IF                                                               
118500     .                                                                    
118600     EJECT                                                                
118700 MFS-RENSA-FAELT-UT SECTION.                                              
118800                                                                          
119100     MOVE +1                TO INDX                                       
119200     PERFORM UNTIL INDX > MAX-TABRADER                                    
119300       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                          
119400                               MOD-IDGMTOMR-FOM(INDX)                     
119500                               MOD-IDGMTOMR-STRECK(INDX)                  
119600                               MOD-IDGMTOMR-TOM(INDX)                     
119700                               MOD-KDPRODKL-FOM(INDX)                     
119800                               MOD-KDPRODKL-STRECK(INDX)                  
119900                               MOD-KDPRODKL-TOM(INDX)                     
120000                               MOD-KDFRAKT-FOM(INDX)                      
120100                               MOD-KDFRAKT-STRECK(INDX)                   
120200                               MOD-KDFRAKT-TOM(INDX)                      
120300                               MOD-IDTRP(INDX)                            
120400                               MOD-IDHLO-FOM(INDX)                        
120500                               MOD-IDHLO-STRECK(INDX)                     
120600                               MOD-IDHLO-TOM(INDX)                        
120700                               MOD-IDPTIDTAB(INDX)                        
120800                               MOD-IDPRC(INDX)                            
120900                                                                          
121000       ADD +1 TO INDX                                                     
121100     END-PERFORM                                                          
121200     .                                                                    
121300     SKIP2                                                                
121400 MFS-RENSA-FAELT-IN SECTION.                                              
121500                                                                          
121600     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-IN                               
121700                             MOD-IDGMTOMR-F-UPP                           
121800                             MOD-IDGMTOMR-T-UPP                           
121900                             MOD-KDPRODKL-F-UPP                           
122000                             MOD-KDPRODKL-T-UPP                           
122100                             MOD-KDFRAKT-F-UPP                            
122200                             MOD-KDFRAKT-T-UPP                            
122300                             MOD-IDTRP-UPP                                
122400                             MOD-IDHLO-F-UPP                              
122500                             MOD-IDHLO-T-UPP                              
122600                             MOD-IDPTIDTAB-UPP                            
122700                             MOD-IDPRC-UPP                                
122800     .                                                                    
122900     EJECT                                                                
123000 MFS-RENSA-RADFAELT SECTION.                                              
123100                                                                          
123200     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                            
123300                             MOD-IDGMTOMR-FOM(INDX)                       
123400                             MOD-IDGMTOMR-STRECK(INDX)                    
123500                             MOD-IDGMTOMR-TOM(INDX)                       
123600                             MOD-KDPRODKL-FOM(INDX)                       
123700                             MOD-KDPRODKL-STRECK(INDX)                    
123800                             MOD-KDPRODKL-TOM(INDX)                       
123900                             MOD-KDFRAKT-FOM(INDX)                        
124000                             MOD-KDFRAKT-STRECK(INDX)                     
124100                             MOD-KDFRAKT-TOM(INDX)                        
124200                             MOD-IDTRP(INDX)                              
124300                             MOD-IDHLO-FOM(INDX)                          
124400                             MOD-IDHLO-STRECK(INDX)                       
124500                             MOD-IDHLO-TOM(INDX)                          
124600                             MOD-IDPTIDTAB(INDX)                          
124700                             MOD-IDPRC(INDX)                              
124800     .                                                                    
124900     EJECT                                                                
125000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
125100                                                                          
125200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-IN                             
125300                               MOD-IDGMTOMR-F-UPP                         
125400                               MOD-IDGMTOMR-T-UPP                         
125500                               MOD-KDPRODKL-F-UPP                         
125600                               MOD-KDPRODKL-T-UPP                         
125700                               MOD-KDFRAKT-F-UPP                          
125800                               MOD-KDFRAKT-T-UPP                          
125900                               MOD-IDTRP-UPP                              
126000                               MOD-IDHLO-F-UPP                            
126100                               MOD-IDHLO-T-UPP                            
126200                               MOD-IDPTIDTAB-UPP                          
126300                               MOD-IDPRC-UPP                              
126400     .                                                                    
126500 MFS-LAES-IN-IGEN SECTION.                                                
126600                                                                          
126700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-IN-ATTR                    
126800                                   MOD-IDGMTOMR-F-UPP-ATTR                
126900                                   MOD-IDGMTOMR-T-UPP-ATTR                
127000                                   MOD-KDPRODKL-F-UPP-ATTR                
127100                                   MOD-KDPRODKL-T-UPP-ATTR                
127200                                   MOD-KDFRAKT-F-UPP-ATTR                 
127300                                   MOD-KDFRAKT-T-UPP-ATTR                 
127400                                   MOD-IDTRP-UPP-ATTR                     
127500                                   MOD-IDHLO-F-UPP-ATTR                   
127600                                   MOD-IDHLO-T-UPP-ATTR                   
127700                                   MOD-IDPTIDTAB-UPP-ATTR                 
127800                                   MOD-IDPRC-UPP-ATTR                     
127900     .                                                                    
128000     EJECT                                                                
128100 MFS-FORM-ATTR SECTION.                                                   
128200                                                                          
128300     MOVE MFS-FORMATETS-ATTR TO MOD-IDRADNR-IN-ATTR                       
128400                                MOD-IDGMTOMR-F-UPP-ATTR                   
128500                                MOD-IDGMTOMR-T-UPP-ATTR                   
128600                                MOD-KDPRODKL-F-UPP-ATTR                   
128700                                MOD-KDPRODKL-T-UPP-ATTR                   
128800                                MOD-KDFRAKT-F-UPP-ATTR                    
128900                                MOD-KDFRAKT-T-UPP-ATTR                    
129000                                MOD-IDTRP-UPP-ATTR                        
129100                                MOD-IDHLO-F-UPP-ATTR                      
129200                                MOD-IDHLO-T-UPP-ATTR                      
129300                                MOD-IDPTIDTAB-UPP-ATTR                    
129400                                MOD-IDPRC-UPP-ATTR                        
129500     .                                                                    
129600     EJECT                                                                
129700* IMS SEKTIONER                                                           
129800     SKIP3                                                                
129900 IMS-GET-MSG SECTION.                                                     
130000                                                                          
130100     MOVE '  QC' TO GODK-STATUSKODER                                      
130200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
130300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130400     PERFORM IMS-STATUSKONTROLL                                           
130500     .                                                                    
130600     SKIP3                                                                
130700 IMS-INSERT-MSG SECTION.                                                  
130800                                                                          
130900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
131000       MOVE '0' TO MFS-KDHUVOMR                                           
131100     END-IF                                                               
131200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
131300     MOVE SPACE TO GODK-STATUSKODER                                       
131400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
131500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131600     PERFORM IMS-STATUSKONTROLL                                           
131700     .                                                                    
131800     EJECT                                                                
131900 IMS-GET-XXKG-TABELL SECTION.                                             
132000                                                                          
132100     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     MOVE '  GE' TO GODK-STATUSKODER                                      
132400     CALL CBLTDLI USING GHU XXKG-PCB DLI-IO-AREA SSA1                     
132500     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800     SKIP3                                                                
132900 IMS-GET-XXKH-PRC-UNIK SECTION.                                           
133000                                                                          
133100     STRING 'WLXXKH01(WDGXKEY  =' W-WDGXKEY-4447-X ')'                    
133200          DELIMITED BY SIZE INTO SSA1                                     
133300     STRING 'WLXXKH11(WDGXKEY  =' W-WDGXKEY-4448-X ')'                    
133400          DELIMITED BY SIZE INTO SSA2                                     
133500     MOVE '  GE' TO GODK-STATUSKODER                                      
133600     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
133700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000     EJECT                                                                
134100 IMS-GET-XXKI-PTIDTAB-UNIK SECTION.                                       
134200                                                                          
134300     STRING 'WLXXKI01(WDGXKEY  =' W-WDGXKEY-4451-X ')'                    
134400          DELIMITED BY SIZE INTO SSA1                                     
134500     MOVE '  GE' TO GODK-STATUSKODER                                      
134600     CALL CBLTDLI USING GU XXKI-PCB DLI-IO-AREA SSA1                      
134700     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000     SKIP3                                                                
135100 IMS-GET-XXKG-RAD-GHU  SECTION.                                           
135200                                                                          
135300     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
135400          DELIMITED BY SIZE INTO SSA1                                     
135500     STRING 'WLXXKG11(WDGXKEY  =' W-WDGXKEY-4446-X ')'                    
135600          DELIMITED BY SIZE INTO SSA2                                     
135700     MOVE '    ' TO GODK-STATUSKODER                                      
135800     CALL CBLTDLI USING GHU XXKG-PCB DLI-IO-AREA SSA1 SSA2                
135900     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
136000     PERFORM IMS-STATUSKONTROLL                                           
136100     .                                                                    
136200     EJECT                                                                
136300 IMS-GET-XXKG-DEFAULT-RAD SECTION.                                        
136400                                                                          
136500     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
136600          DELIMITED BY SIZE INTO SSA1                                     
136700     STRING 'WLXXKG11(WDGXKEY  =' W-WDGXKEY-4446-X ')'                    
136800          DELIMITED BY SIZE INTO SSA2                                     
136900     MOVE '    ' TO GODK-STATUSKODER                                      
137000     CALL CBLTDLI USING GU XXKG-PCB DLI-IO-AREA SSA1 SSA2                 
137100     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     SKIP3                                                                
137500 IMS-GET-XXKG-RAD SECTION.                                                
137600                                                                          
137700     STRING 'WLXXKG11(WDGXKEY =>' W-WDGXKEY-4446-X ')'                    
137800          DELIMITED BY SIZE INTO SSA1                                     
137900     MOVE '  GE' TO GODK-STATUSKODER                                      
138000     CALL CBLTDLI USING GNP XXKG-PCB DLI-IO-AREA SSA1                     
138100     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400     EJECT                                                                
138500 IMS-GET-XXKG-RAD-FIRST SECTION.                                          
138600                                                                          
138700     STRING 'WLXXKG11*F(WDGXKEY =>' W-WDGXKEY-4446-X ')'                  
138800          DELIMITED BY SIZE INTO SSA1                                     
138900     MOVE '  GE' TO GODK-STATUSKODER                                      
139000     CALL CBLTDLI USING GNP XXKG-PCB DLI-IO-AREA SSA1                     
139100     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     SKIP3                                                                
139500 IMS-GET-XXKG-RAD-UNIK SECTION.                                           
139600                                                                          
139700     STRING 'WLXXKG11*F(WDGXKEY  =' W-WDGXKEY-4446-X ')'                  
139800          DELIMITED BY SIZE INTO SSA1                                     
139900     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GHNP XXKG-PCB DLI-IO-AREA SSA1                    
140100     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     EJECT                                                                
140500 IMS-ISRT-XXKG-TABELL SECTION.                                            
140600                                                                          
140700     MOVE 'WLXXKG01 ' TO SSA1                                             
140800     MOVE '  ' TO GODK-STATUSKODER                                        
140900     CALL CBLTDLI USING ISRT XXKG-PCB DLI-IO-AREA SSA1                    
141000     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     SKIP3                                                                
141400 IMS-ISRT-XXKG-RAD SECTION.                                               
141500                                                                          
141600     STRING 'WLXXKG01(WDGXKEY  =' W-WDGXKEY-4445-X ')'                    
141700          DELIMITED BY SIZE INTO SSA1                                     
141800     MOVE 'WLXXKG11 ' TO SSA2                                             
141900     MOVE '  ' TO GODK-STATUSKODER                                        
142000     CALL CBLTDLI USING ISRT XXKG-PCB DLI-IO-AREA SSA1 SSA2               
142100     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     EJECT                                                                
142500 IMS-REPL-XXKG-RAD SECTION.                                               
142600                                                                          
142700     MOVE '  ' TO GODK-STATUSKODER                                        
142800     CALL CBLTDLI USING REPL XXKG-PCB DLI-IO-AREA                         
142900     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
143000     PERFORM IMS-STATUSKONTROLL                                           
143100     .                                                                    
143200     SKIP3                                                                
143300 IMS-DLET-XXKG-RAD SECTION.                                               
143400                                                                          
143500     MOVE '  ' TO GODK-STATUSKODER                                        
143600     CALL CBLTDLI USING DLET XXKG-PCB DLI-IO-AREA                         
143700     MOVE XXKG-STATUS-CODE TO STATUS-WS                                   
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     EJECT                                                                
144100 IMS-GU-WDB601    SECTION.                                                
144200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
144300          DELIMITED BY SIZE INTO SSA1                                     
144400     MOVE '  GE' TO GODK-STATUSKODER                                      
144500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
144600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
144700     PERFORM IMS-STATUSKONTROLL                                           
144800     IF SEGMENT-SAKNAS                                                    
144900         MOVE SPACE TO DCS-KDDC                                           
145000     END-IF                                                               
145100     .                                                                    
145200 IMS-STATUSKONTROLL SECTION.                                              
145300                                                                          
145400     SET STATUS-IX TO 1                                                   
145500     SEARCH GODK-STATUS                                                   
145600       AT END CALL FELLOG                                                 
145700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
145800     END-SEARCH                                                           
145900     .                                                                    
