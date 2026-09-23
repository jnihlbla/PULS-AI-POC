000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4033800.                                                
000400 AUTHOR.         CAP GEMINI / BOH                                         
000500     DATE-WRITTEN.   MARS  86.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA SKALL INTE GE                    
000900*    NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.                              
001000*                                                                         
001100*    FUNKTION.                                                            
001200*    PROGRAMMET ÄR ETT RENT UPPDATERINGSPROGRAM.                          
001300*    DET UPPDATERAR KOLLIUPPGIFTER FÖR SVERIGE PÅ OLIKA REGISTER          
001400*    EFTER INMATNING VID BANDSTATION.                                     
001500*    W4033800 SKICKAR ÄVEN IVÄG EN TRANS TILL W4034100                    
001600*    VILKET SKRIVER UT EN 'FÖLJESEDEL'  .                                 
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T338                                              
002100*        MID:         W4I33801                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        TRANSAKTION: W4T341                                              
002500*        TRANSAKTION: W4T333                                              
002600*        MOD:         W4O33801                                            
002700*                                                                         
002800*    CHANGE LOG                                                           
002900*                                                                         
003000*    DIGAMBAR/021011                                                      
003100*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
003200*    THE RESPONSE TIME OF THE SCREEN 4312.                                
003300*                                                                         
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP3                                                                
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77   PROGRAM-NAMN           VALUE 'W4033800'                             
004300                                 PIC X(8).                                
004400 77  JA                          PIC X(1)    VALUE 'J'.                   
004500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
004700 77  SAKNAS                      PIC X       VALUE 'S'.                   
004800 77  RAETT                       PIC X       VALUE 'R'.                   
004900 77  INDX                        PIC S9(4)   COMP SYNC.                   
005000 77  WS-MFS-KDMFSFOR             PIC 9.                                   
005100 77  WS-IDTRPTNR                 PIC S9(3)   COMP-3 VALUE +998.           
005200 77  WS-IDKUNDNR                 PIC X(6).                                
005300 77  WS-IDKUNDNR-NUM             PIC 9(6).                                
005400 77  WS-IDKOLLI                  PIC X(5).                                
005500 77  WS-KDKOLLI                  PIC X(8).                                
005600 77  WS-KDKOLLID                 PIC X.                                   
005700 77  WS-IDKOLLI-NUM5             PIC 9(5).                                
005800 77  WS-KDEMBTYP                 PIC S9      COMP-3.                      
005900 77  WS-DIKOLLIL                 PIC S9(5)   COMP-3.                      
006000 77  WS-DIKOLLIB                 PIC S9(3)   COMP-3.                      
006100 77  WS-DIKOLLIH                 PIC S9(3)   COMP-3.                      
006200 77  WS-EMB-VKTARA-ONE-CASE      PIC S9(6)V9 VALUE ZERO  COMP-3.          
006300 77  WS-VLORDBTO                 PIC S9(4)V9(3) COMP-3.                   
006400 77  WS-SUORDV-KOLLI             PIC S9(9)V9(2) COMP-3.                   
006500 77  WS-SUORDV-KOLLI-LOC         PIC S9(9)V9(2) COMP-3.                   
006600 77  WS-SUORDV-KOLLI-LOCPREL     PIC S9(9)V9(2) COMP-3.                   
006700 77  WS-IDPRODNR                 PIC  9(7).                               
006800 77  WS-IDKUNDNR-7               PIC  9(7).                               
006900 77  FILLER                      PIC  X(8) VALUE 'AAAAAAAA'.              
007000 77  WS-KDFRAKT                  PIC  9(2) VALUE ZERO.                    
007100 77  WS-KDORDKL                  PIC S9      COMP-3.                      
007200 77  WS-IDPLKLST                 PIC S9(3)   COMP-3.                      
007300 77  WS-FLAUTFAK                 PIC X.                                   
007400 77  WS-KDFAKTYP                 PIC X.                                   
007500 77  WS-BEGMT                    PIC X(70).                               
007600 77  WS-ADGMT                    PIC X(105).                              
007700 77  WS-KVLOCK                   PIC S9(3)   COMP-3.                      
007800 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   COMP-3 VALUE ZERO.           
007900 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
008000 77  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
008100 77  W-IDSHIPM                   PIC 9(7)    VALUE ZERO.                  
008200 77  WS-DAGENS-DATUM             PIC 9(6)   VALUE ZERO.                   
008300 77  WS-TIDPUNKT                 PIC 9(8)   VALUE ZERO.                   
008400 77  WS-TISKPTID                 PIC 9(6)   VALUE ZERO.                   
008500                                                                          
008600 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
008700 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
008800 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
008900 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
009000                                                                          
009100 77  4338-MOD-LAENGD             PIC S9(4)   VALUE +175 COMP SYNC.        
009200 77  4341-MID-TRANS-LAENGD       PIC S9(4)   VALUE +80  COMP SYNC.        
009300                                                                          
009400 01  WS-IDPRTLST.                                                         
009500     03 WS-SYSTDEL               PIC X(1).                                
009600     03 WS-LISTTYP               PIC X(2).                                
009700     03 WS-DC                    PIC X(2).                                
009800     03 WS-KDPRT                 PIC X(3).                                
009900                                                                          
010000 77  WS-SLINGA-KLAR              PIC X(1).                                
010100     88  SLINGA-KLAR                         VALUE 'J'.                   
010200                                                                          
010300 77  FOERSTA-SW                  PIC X(1).                                
010400     88  FOERSTA-PERFORM-UNTIL               VALUE 'J'.                   
010500                                                                          
010600 77  PLOCKLISTA-HITTAD-SW        PIC X(1).                                
010700     88  PLOCKLISTA-HITTAD                   VALUE 'J'.                   
010800                                                                          
010900 77  4490-BORTTAG-SW             PIC X(1).                                
011000     88  SEG-4490-BORTTAG                    VALUE 'J'.                   
011100                                                                          
011200 77    IDPRODNR-IFYLLT-SW        PIC X(01).                               
011300   88  IDPRODNR-IFYLLT                       VALUE 'J'.                   
011400*                                                                         
011500 77  WS-KDPRTVAL-FS              PIC XX.                                  
011600 77  WS-KDPRTVAL-AF              PIC XX.                                  
011700 77  WS-PRT-KDSVAR-ADRESSFL      PIC X.                                   
011800 77  WS-PRT-KDSVAR-FOLJEFL       PIC X.                                   
011900     SKIP2                                                                
012000 77  WS-IDTRANS                  PIC X(4).                                
012100     88  WS-GODKAEND-BILD                    VALUE '4331' '4332'          
012200                              '4333' '4334' '4335' '4336' '4338'.         
012300     SKIP2                                                                
012400 77  WS-VORD-FARDIGPACKAD        PIC X(1).                                
012500     SKIP2                                                                
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     SKIP2                                                                
013000*                                                                         
013100 01  GEMENSAMMA-SUBPROGRAM.                                               
013200     03  W006PRT                PIC X(8)    VALUE 'W006PRT '.             
013300*        PRINTERKONTROLL                                                  
013400*                                                                         
013500*                                                                         
013600                                                                          
013700*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
013800*                                                                         
013900 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
014000*                                                                         
014100 01  FILLER                     PIC X(16)   VALUE 'W006PRT  '.            
014200*   -COPY W006PRT                                                         
014300     EJECT                                                                
014400     SKIP3                                                                
014500                                                                          
014600                                                                          
014700 01  WS-IDDISTR                  PIC X(4).                                
014800 01  WS-IDDISTR-NUM REDEFINES WS-IDDISTR PIC 9(4).                        
014900                                                                          
015000                                                                          
015100                                                                          
015200 01  WS-IDKUNDRF.                                                         
015300                                                                          
015400     03  WS-IDORDNR              PIC X(5).                                
015500     03  FILLER                  PIC X(5)  VALUE SPACE.                   
015600                                                                          
015700 01  WS-TIPACTID-8.                                                       
015800     03  FILLER                  PIC 9(8).                                
015900 01  WS-TIPACTID-8-6.                                                     
016000     03  WS-TIPACTID-6           PIC 9(6).                                
016100     03  FILLER                  PIC 9(2).                                
016200                                                                          
016300                                                                          
016400                                                                          
016500 01  WS-FEL-FUNNET               PIC X     VALUE 'N'.                     
016600                                                                          
016700     88  FEL-FUNNET                        VALUE 'J'.                     
016800     88  FEL-EJ-FUNNET                     VALUE 'N'.                     
016900                                                                          
017000                                                                          
017100                                                                          
017200 01  WS-DISTRIKT-TYP             PIC X(3)  VALUE SPACE.                   
017300                                                                          
017400     88  EXPORT-DISTRIKT                   VALUE 'EXP'.                   
017500     88  SVERIGE-DISTRIKT                  VALUE 'SVE'.                   
017600     EJECT                                                                
017700 01  NYCKLAR-TILL-DLI.                                                    
017800                                                                          
017900     03  W-K501-KDKOLLI-X.                                                
018000         05  W-K501-KDKOLLI      PIC X(8).                                
018100                                                                          
018200     03  W-E4A1-WDE4KEY-X.                                                
018300         05  W-E4A1-IDDISTR      PIC S9(5)   COMP-3.                      
018400         05  W-E4A1-IDKUNDNR     PIC S9(7)   COMP-3.                      
018500         05  W-E4A1-IDKUNDRF.                                             
018600             07  W-E4A1-IDORDNR  PIC X(5).                                
018700             07  FILLER          PIC X(5)    VALUE SPACE.                 
018800                                                                          
018900     03  W-E401-WDE4KEY-X.                                                
019000         05  W-E401-IDDISTR      PIC S9(5)   COMP-3.                      
019100         05  W-E401-IDKUNDNR     PIC S9(7)   COMP-3.                      
019200         05  W-E401-IDKUNDRF.                                             
019300             07  W-E401-IDORDNR  PIC X(5).                                
019400             07  FILLER          PIC X(5)    VALUE SPACE.                 
019500         05  W-E401-IDPRODNR     PIC S9(7)   COMP-3.                      
019600         05  W-E401-IDPLKLST     PIC S9(3)   COMP-3.                      
019700                                                                          
019800                                                                          
019900     03    W-WDE4E1KY-MAX-X.                                              
020000       05    W-WDE4E1KY-MAX          PIC S9(7) COMP-3.                    
020100     03    W-WDE4SEQE.                                                    
020200       05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.        
020300       05    W-WDE4E1-MAX2.                                               
020400           07 W-WDE4E1-IDDISTR     PIC S9(5) COMP-3 VALUE 99999.          
020500           07 W-WDE4E1-IDKUNDNR    PIC S9(7) COMP-3 VALUE 9999999.        
020600           07 W-WDE4E1-IDKUNDRF    PIC X(10) VALUE HIGH-VALUE.            
020700           07 W-WDE4E1-IDPLSLST    PIC S9(3) COMP-3 VALUE 999.            
020800                                                                          
020900     03    W-WDE4E1KY-MIN-X.                                              
021000       05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.        
021100       05    W-WDE4E1-MIN.                                                
021200           07 W-WDE4E1-IDDISTR-M   PIC S9(5) COMP-3 VALUE ZERO.           
021300           07 W-WDE4E1-IDKUNDNR-M  PIC S9(7) COMP-3 VALUE ZERO.           
021400           07 W-WDE4E1-IDKUNDRF-M  PIC X(10) VALUE LOW-VALUE.             
021500           07 W-WDE4E1-IDPLSLST-M  PIC S9(3) COMP-3 VALUE ZERO.           
021600                                                                          
021700     03  W-E601-IDPRODNR-X.                                               
021800         05  W-E601-IDPRODNR     PIC S9(7)   COMP-3.                      
021900                                                                          
022000     03  W-E611-IDKOLLI-X.                                                
022100         05  W-E611-IDKOLLI      PIC S9(5)   COMP-3.                      
022200                                                                          
022300     03  W-E612-IDPURAD-MIN-X.                                            
022400         05  W-E612-IDPURAD-MIN  PIC  X(3)   VALUE LOW-VALUE.             
022500                                                                          
022600     03  W-E612-IDPURAD-MAX-X.                                            
022700         05  W-E612-IDPURAD-MAX  PIC  X(3)   VALUE HIGH-VALUE.            
022800*                                                                         
022900     03    W-WDQ201-X.                                                    
023000         05    W-201-IDORDER     PIC S9(7) COMP-3.                        
023100                                                                          
023200     03    W-WDQ212-X.                                                    
023300         05    W-212-IDDC        PIC X(2).                                
023400                                                                          
023500     03  W-Q301-KEY-X.                                                    
023600         05  W-Q301-IDORDER      PIC S9(7)   COMP-3.                      
023700         05  W-Q301-IDDC         PIC X(2).                                
023800         05  W-Q301-IDPRODNR     PIC S9(7)   COMP-3.                      
023900         05  W-Q301-IDPLKLST     PIC S9(3)   COMP-3.                      
024000                                                                          
024100                                                                          
024200     03  W-Q301-KEY-MIN-X.                                                
024300         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
024400         05  W-Q301-MIN-IDDC     PIC X(2).                                
024500         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
024600         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
024700                                                                          
024800     03  W-Q301-KEY-MAX-X.                                                
024900         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
025000         05  W-Q301-MAX-IDDC     PIC X(2).                                
025100         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
025200         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
025300*                                                                         
025400     03  W-4301-WDGXKEY-X.                                                
025500         05 W-4301-IDHTYP        PIC X(4)  VALUE '4301'.                  
025600         05 W-4301-IDPRODNR      PIC S9(7) VALUE ZERO COMP-3.             
025700         05 W-4301-NYCKEL-VALFRI PIC X(22) VALUE LOW-VALUE.               
025800*                                                                         
025900     03  W-4302-WDGXKEY-X.                                                
026000         05 W-4302-IDKOLLI-X.                                             
026100            07 W-4302-IDKOLLI    PIC S9(5) VALUE ZERO COMP-3.             
026200         05 W-4302-IDPLKLST-X.                                            
026300            07 W-4302-IDPLKLST   PIC S9(3) VALUE ZERO COMP-3.             
026400                                                                          
026500*                                                                         
026600     03  W-4447-WDGXKEY-X.                                                
026700         05  FILLER              PIC X(4)  VALUE '4447'.                  
026800         05  W-4447-IDDC         PIC X(2).                                
026900         05  FILLER              PIC X(24) VALUE LOW-VALUE.               
027000*                                                                         
027100     03  W-4448-WDGXKEY-X.                                                
027200         05  W-4448-IDPRC        PIC X(4).                                
027300         05  FILLER              PIC X(1)  VALUE LOW-VALUE.               
027400*                                                                         
027500     03  W-4487-WDGXKEY-X.                                                
027600         05  FILLER              PIC X(4)  VALUE '4487'.                  
027700         05  W-4487-IDDC         PIC X(2).                                
027800         05  FILLER              PIC X(24) VALUE LOW-VALUE.               
027900*                                                                         
028000     03  W-4488-WDGXKEY-X.                                                
028100         05  W-4488-KDPRCGRP     PIC X(5).                                
028200*                                                                         
028300     03  W-4490-WDGXKEY-X.                                                
028400         05  W-4490-DARFS        PIC 9(12).                               
028500         05  W-4490-IDPRODNR     PIC S9(7)  COMP-3.                       
028600         05  W-4490-IDPLKLST     PIC S9(3)  COMP-3.                       
028700                                                                          
028800     03  W-4726-WDGXKEY-ROT-X.                                            
028900         05  W-4726-IDHTYP       PIC X(4)    VALUE '4726'.                
029000         05  W-4726-FLBATCH      PIC X(1)    VALUE SPACE.                 
029100         05  W-4726-LOWVALUE     PIC X(25)   VALUE LOW-VALUE.             
029200                                                                          
029300     03  W-4726-WDGXKEY-UNDSEG-X.                                         
029400         05  W-4726-IDDISTR      PIC S9(5)   COMP-3.                      
029500         05  W-4726-IDKUNDNR     PIC S9(7)   COMP-3.                      
029600         05  W-4726-IDDC         PIC X(2).                                
029700         05  W-4726-KDFAKTYP     PIC X.                                   
029800                                                                          
029900     03  W-4321-IDHTYP-X.                                                 
030000         05  W-4321-IDHTYP       PIC X(4)    VALUE '4321'.                
030100         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
030200                                                                          
030300     03  W-IDDC-B6-X.                                                     
030400         05 W-IDDC-B6                  PIC X(2).                          
030500     EJECT                                                                
030600 01  MEDDELANDE.                                                          
030700                                                                          
030800     03  FEL-1.                                                           
030900         05  FILLER              PIC X(40)   VALUE                        
031000             '726 KOLLIKOD SAKNAS                     '.                  
031100         05  FILLER              PIC X(40)   VALUE                        
031200             '726 KOLLIKODE ONTBREEKT                 '.                  
031300     03  FEL-726 REDEFINES FEL-1 OCCURS 2 PIC X(40).                      
031400                                                                          
031500     03  FEL-2.                                                           
031600         05  FILLER              PIC X(40)   VALUE                        
031700             '761 ANGE MÅTT                           '.                  
031800         05  FILLER              PIC X(40)   VALUE                        
031900             '761 GEEF AFMETINGEN                     '.                  
032000     03  FEL-761 REDEFINES FEL-2 OCCURS 2 PIC X(40).                      
032100                                                                          
032200     03  FEL-3.                                                           
032300         05  FILLER              PIC X(40)   VALUE                        
032400             '762 ANGIVET DISTR, KUND, ORDER FINNS EJ '.                  
032500         05  FILLER              PIC X(40)   VALUE                        
032600             '762 DISTR-KLANT-ORDER BESTAAT NIET      '.                  
032700     03  FEL-762 REDEFINES FEL-3 OCCURS 2 PIC X(40).                      
032800                                                                          
032900     03  FEL-4.                                                           
033000         05  FILLER              PIC X(40)   VALUE                        
033100             '758 KOLLI SAKNAS                        '.                  
033200         05  FILLER              PIC X(40)   VALUE                        
033300             '758 KIST ONTBREEKT                      '.                  
033400     03  FEL-758 REDEFINES FEL-4 OCCURS 2 PIC X(40).                      
033500                                                                          
033600     03  FEL-5.                                                           
033700         05  FILLER              PIC X(40)   VALUE                        
033800             '748 UPPLYSTA FÄLT FEL                   '.                  
033900         05  FILLER              PIC X(40)   VALUE                        
034000             '748 VERLICHTE ZONE FOUTIEF              '.                  
034100     03  FEL-748 REDEFINES FEL-5 OCCURS 2 PIC X(40).                      
034200                                                                          
034300     03  FEL-6.                                                           
034400         05  FILLER              PIC X(40)   VALUE                        
034500             '763 MÅTTUPPGIFTER MÅSTE VARA NUMERISKA  '.                  
034600         05  FILLER              PIC X(40)   VALUE                        
034700             '763 AFMETINGEN MOETEN NUMERISCH ZIJN    '.                  
034800     03  FEL-763 REDEFINES FEL-6 OCCURS 2 PIC X(40).                      
034900                                                                          
035000     03  FEL-7.                                                           
035100         05  FILLER              PIC X(40)   VALUE                        
035200             '749 FEL NYCKEL                          '.                  
035300         05  FILLER              PIC X(40)   VALUE                        
035400             '749 VERKEERDE SLEUTEL - HERBEGIN        '.                  
035500     03  FEL-749 REDEFINES FEL-7 OCCURS 2 PIC X(40).                      
035600                                                                          
035700     03  FEL-8.                                                           
035800         05  FILLER              PIC X(40)   VALUE                        
035900             '797 EJ SVERIGE                          '.                  
036000         05  FILLER              PIC X(40)   VALUE                        
036100             '797 NIET - ZWEDEN                       '.                  
036200     03  FEL-797 REDEFINES FEL-8 OCCURS 2 PIC X(40).                      
036300                                                                          
036400     03  FEL-9.                                                           
036500         05  FILLER              PIC X(40)   VALUE                        
036600             '798 KOLLIT REDAN BANDSTATIONSRAPPORTERAT'.                  
036700         05  FILLER              PIC X(40)   VALUE                        
036800             '798 KIST REEDS GERAPP AAN STRAPSTATION  '.                  
036900     03  FEL-798 REDEFINES FEL-9 OCCURS 2 PIC X(40).                      
037000                                                                          
037100     03  FEL-10.                                                          
037200         05  FILLER              PIC X(40)   VALUE                        
037300             '764 ANGIVET PRODNR FINNS EJ.            '.                  
037400         05  FILLER              PIC X(40)   VALUE                        
037500             '764 PRODNR DOES NOT EXIST.              '.                  
037600     03  FEL-764 REDEFINES FEL-10 OCCURS 2 PIC X(40).                     
037700                                                                          
037800     03  MED-1.                                                           
037900         05  FILLER              PIC X(40)   VALUE                        
038000             'UPPDATERING UTFÖRD                      '.                  
038100         05  FILLER              PIC X(40)   VALUE                        
038200             'UPDATING UITGEVOERD                     '.                  
038300     03  MED-001 REDEFINES MED-1 OCCURS 2 PIC X(40).                      
038400     EJECT                                                                
038500 01    FILLER              PIC X(16)   VALUE 'TEST-IDDISTR'.              
038600 01  TEST-IDDISTR          PIC S9(5)   COMP-3.                            
038700                                                                          
038800                                                                          
038900                                                                          
039000*01  FILLER      -COPY WWDIST03    -RED TEST-IDDISTR.                     
039100     EJECT                                                                
039200*01  FILLER      -COPY WWDIST18    -RED TEST-IDDISTR.                     
039300     EJECT                                                                
039400*01  FILLER      -COPY WWDIST19    -RED TEST-IDDISTR.                     
039500     EJECT                                                                
039600*01  FILLER      -COPY WWDIST21    -RED TEST-IDDISTR.                     
039700     EJECT                                                                
039800*01  FILLER      -COPY WWDIST47    -RED TEST-IDDISTR.                     
039900     EJECT                                                                
040000*01  FILLER      -COPY WWDIS128    -RED TEST-IDDISTR.                     
040100     EJECT                                                                
040200 01  FILLER                      PIC X(08)  VALUE 'FRAKT1  '.             
040300*   -COPY WWFRAKT1                                                        
040400     EJECT                                                                
040500                                                                          
040600 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
040700                                                                          
040800 01    FILLER              PIC X(16)   VALUE 'MID W4I338 MID'.            
040900*01  MID -COPY W4I33801.                                                  
041000     EJECT                                                                
041100*01  -COPY WMSGAREA                                                       
041200     EJECT                                                                
041300*    03  MOD -COPY W4O33801  -RED MSG-AREA.                               
041400     EJECT                                                                
041500 01    FILLER              PIC X(16)   VALUE 'M0D W40636I1 M0D'.          
041600*01  -COPY W40636I1   -PRE MOD4636-                                       
041700     EJECT                                                                
041800 01  4333-MID-IO-AREA.                                                    
041900                                                                          
042000     03  4333-MID-KVLL           PIC S9(4)   COMP SYNC.                   
042100     03  4333-MID-Z1             PIC X.                                   
042200     03  4333-MID-Z2             PIC X.                                   
042300     03  4333-MID-TRANSKOD       PIC X(8)    VALUE 'W4T333  '.            
042400     03  4333-MID-IDTRANS        PIC X(4)    VALUE '433B'.                
042500     03  4333-MID-KDMFSFOR       PIC X.                                   
042600*    03  MID -COPY W4I33301   -PRE 4333-.                                 
042700     EJECT                                                                
042800 01  4341-MID-IO-AREA.                                                    
042900                                                                          
043000     03  4341-MID-KVLL          PIC S9(4)   COMP SYNC.                    
043100     03  4341-MID-Z1             PIC X.                                   
043200     03  4341-MID-Z2             PIC X.                                   
043300     03  4341-MID-TRANSKOD       PIC X(8)    VALUE 'W4T341  '.            
043400     03  4341-MID-IDTRANS        PIC X(4)    VALUE '433H'.                
043500     03  4341-MID-KDMFSFOR       PIC X.                                   
043600*    03  MID -COPY W4I34101   -PRE 4341-.                                 
043700     EJECT                                                                
043800*01  -COPY WMFSAREA                                                       
043900     EJECT                                                                
044000*01  WDGZRY6  -COPY WDGZRY6.                                              
044100     EJECT                                                                
044200*01  XXJK     -COPY WDGX4322    -PRE XXJK-                                
044300 01  IMS-WS.                                                              
044400     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
044500     SKIP3                                                                
044600*                        **** STATUS-KOD FRÅN IMS                         
044700     03  STATUS-KUNDORDER-SEK-WS PIC X(2).                                
044800         88  KUNDORDER-SEK-FINNS             VALUE '  '.                  
044900         88  KUNDORDER-SEK-SAKNAS            VALUE 'GE' 'GB'.             
045000     03  STATUS-WS               PIC X(2).                                
045100         88  SEGMENT-FINNS                   VALUE '  '.                  
045200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
045300         88  END-OF-DATA                     VALUE 'GB'.                  
045400     SKIP3                                                                
045500     03  GODK-STATUSKODER.                                                
045600         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
045700     SKIP3                                                                
045800 01  SSA1                        PIC X(96).                               
045900 01  SSA2                        PIC X(64).                               
046000 01  SSA3                        PIC X(64).                               
046100     EJECT                                                                
046200*                            IMS FUNKTIONSKODER                           
046300*01  -COPY W0003                                                          
046400     EJECT                                                                
046500*                            DLI INPUT-OUTPUT AREA                        
046600 01  IO-AREA-1                   PIC X(8)    VALUE 'IO-AREA1'.            
046700 01  DLI-IO-AREA.                                                         
046800     03  IO-AREA                 PIC X(352)  VALUE SPACE.                 
046900                                                                          
047000                                                                          
047100*    03  WLEMBB01 -COPY WDK501     -RED IO-AREA.                          
047200     EJECT                                                                
047300*    03  WDE401   -COPY WDE401     -RED IO-AREA.                          
047400     EJECT                                                                
047500*    03  WDE601   -COPY WDE601     -RED IO-AREA.                          
047600     EJECT                                                                
047700*    03  WDE611   -COPY WDE611     -RED IO-AREA.                          
047800     EJECT                                                                
047900*    03  WLXXDV11 -COPY WDGX4726   -RED IO-AREA.                          
048000     EJECT                                                                
048100*    03  WLXXDV21 -COPY WDGX4727   -RED IO-AREA.                          
048200     EJECT                                                                
048300*    03  WLXXKH01 -COPY WDGX4447   -RED IO-AREA.                          
048400     SKIP2                                                                
048500*    03  WLXXKH11 -COPY WDGX4448   -RED IO-AREA.                          
048600     EJECT                                                                
048700*    03  4487-AREA -COPY WDGX4487   -RED IO-AREA.                         
048800     SKIP2                                                                
048900*    03  WDGX4490 -COPY WDGX4490   -RED IO-AREA.                          
049000     EJECT                                                                
049100 01  DLI-IO-AREA2.                                                        
049200     03  IO-AREA2                PIC X(200)  VALUE SPACE.                 
049300                                                                          
049400                                                                          
049500*    03  WDGZ01   -COPY WDGZ01   -PRE LOGG-  -RED IO-AREA2.               
049600     EJECT                                                                
049700*    03  WLXXJK01 -COPY WDGX01         -RED IO-AREA2.                     
049800                                                                          
049900                                                                          
050000*    03  WLXXJK11 -COPY WDGX4322       -RED IO-AREA2.                     
050100     EJECT                                                                
050200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
050300 01  DLI-IO-AREA3.                                                        
050400     03  IO-AREA3                PIC X(259)  VALUE SPACE.                 
050500     SKIP2                                                                
050600     03  WDE601   -COPY WDE601   -PRE OGAG-  -RED IO-AREA3.               
050700     EJECT                                                                
050800     03  WDE401   -COPY WDE401   -PRE OGAG-  -RED IO-AREA3.               
050900     EJECT                                                                
051000     03  WLORQA01 -COPY WDQ301   -PRE ORQA-  -RED IO-AREA3.               
051100     EJECT                                                                
051200     EJECT                                                                
051800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ212'.         
051900     EJECT                                                                
052000 01  DLI-IO-WDQ212.                                                       
052100*    03  -COPY WDQ212                                                     
052200     EJECT                                                                
052300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
052400 01  DLI-IO-AREA5.                                                        
052500     03  IO-AREA5                PIC X(500)  VALUE SPACE.                 
052600     SKIP2                                                                
052700     03  WLXXDU11 -COPY WDGX4302      -RED IO-AREA5.                      
052800                                                                          
052900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
053000 01   DLI-IO-AREA-B601.                                                   
053100*     03  -COPY WDB601                                                    
053200                                                                          
053300 LINKAGE SECTION.                                                         
053400*01  -COPY W0009     -PRE MSG-                                            
053500     EJECT                                                                
053600*01  -COPY W0009     -PRE ALT-                                            
053700     EJECT                                                                
053800*01  -COPY W0009     -PRE 4333-                                           
053900     EJECT                                                                
054000*01  -COPY W0008     -PRE WDE4-                                           
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300*01  -COPY W0008     -PRE WDE4A-                                          
054400     05  FILLER                  PIC X.                                   
054500     EJECT                                                                
054600*01  -COPY W0008     -PRE WDE6-                                           
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01  -COPY W0008     -PRE WDE4E-                                          
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01  -COPY W0008     -PRE EMBB-                                           
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01  -COPY W0008     -PRE XXDV-                                           
055600     05  FILLER                  PIC X.                                   
055700     EJECT                                                                
055800*01  -COPY W0008     -PRE ZZAC-                                           
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01  -COPY W0008     -PRE XXJK-                                           
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01  -COPY W0008     -PRE WDE62-                                          
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01  -COPY W0008     -PRE ORQA-                                           
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0008     -PRE ORQI-                                           
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300*01  -COPY W0008     -PRE XXKH-                                           
057400     05  FILLER                  PIC X.                                   
057500     EJECT                                                                
057600*01  -COPY W0008     -PRE 4487-                                           
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900*01  -COPY W0008     -PRE XXDU-                                           
058000     05  FILLER                  PIC X.                                   
058100     EJECT                                                                
058200*01  -COPY W0008     -PRE XXDU2-                                          
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500*01  -COPY W0008     -PRE WDB6-                                           
058600     05  FILLER                  PIC X.                                   
058700     EJECT                                                                
058800 PROCEDURE DIVISION USING MSG-PCB   ALT-PCB                               
058900                          4333-PCB WDE4-PCB                               
059000                          WDE4A-PCB WDE6-PCB WDE4E-PCB                    
059100                          EMBB-PCB  XXDV-PCB ZZAC-PCB                     
059200                          XXJK-PCB  WDE62-PCB ORQA-PCB ORQI-PCB           
059300                          XXKH-PCB  4487-PCB XXDU-PCB XXDU2-PCB           
059400                          WDB6-PCB.                                       
059500                                                                          
059600     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB                              
059700                           4333-PCB  WDE4-PCB                             
059800                           WDE4A-PCB WDE6-PCB WDE4E-PCB                   
059900                           EMBB-PCB  XXDV-PCB ZZAC-PCB                    
060000                           XXJK-PCB  WDE62-PCB ORQA-PCB ORQI-PCB          
060100                           XXKH-PCB  4487-PCB XXDU-PCB XXDU2-PCB          
060200                           WDB6-PCB.                                      
060300                                                                          
060400                                                                          
060500     PERFORM IMS-GET-MSG                                                  
060600                                                                          
060700     IF SEGMENT-FINNS                                                     
060800         MOVE NEJ       TO WS-FEL-FUNNET                                  
060900         PERFORM A-INIT-SPARA-INPUT                                       
061000                                                                          
061100********* FIX START    OBS TVÅ ALTERNATIV PÅ IF                           
061200*********     ALTERNATIV ETT FUNGERADE INTE 890725                        
061300*        IF  MID-IDDISTR-IN    = '0435'                                   
061400*        AND MID-IDKUNDNR-IN   = '000000'                                 
061500*        AND MID-IDORDNR-IN    = '88180'                                  
061600*        AND MSG-SIGNON-USERID = 'R059701 '                               
061700*        IF MSG-SIGNON-USERID = 'V059701 '                                
061800*            MOVE 'N'          TO WS-KDFAKTYP                             
061900*            MOVE +31546       TO WS-IDPRODNR                             
062000*            MOVE 98           TO TEST-IDDISTR                            
062100*            MOVE 'SVE'        TO WS-DISTRIKT-TYP                         
062200*            PERFORM BG-UPPDAT-4726-4727                                  
062300*            MOVE 'AUTOMATTRAN UPPLAGD' TO MOD-TEMFSINF                   
062400*        END-IF                                                           
062500********* FIX SLUT                                                        
062600                                                                          
062700         PERFORM B-BEHANDLA-INDATA                                        
062800                                                                          
062900         IF FEL-EJ-FUNNET                                                 
063000           PERFORM S03-RENSA-MODFAELT                                     
063100           MOVE MED-001(INDX) TO MOD-TEMFSINF                             
063200         END-IF                                                           
063300                                                                          
063400         PERFORM   IMS-INSERT-MSG                                         
063500                                                                          
063600     END-IF                                                               
063700                                                                          
063800     MOVE ZERO TO RETURN-CODE                                             
063900                                                                          
064000     GOBACK                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 A-INIT-SPARA-INPUT SECTION.                                              
064400                                                                          
064500     IF MSG-DUBBLA-TRANSKODER                                             
064600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I33801               
064700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
064800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
064900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
065000         MOVE MSG-IDPFK TO MFS-IDPFK                                      
065100     ELSE                                                                 
065200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I33801                
065300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
065400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR WS-MFS-KDMFSFOR              
065500         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
065600     END-IF                                                               
065700     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
065800     MOVE NEJ               TO IDPRODNR-IFYLLT-SW                         
065900                                                                          
066000     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
066100     ACCEPT WS-TIDPUNKT                   FROM TIME                       
066200                                                                          
066300     MOVE WS-TIDPUNKT(1:6)  TO      WS-TISKPTID                           
066400                                                                          
066500     IF MID-IDDISTR-IN = ALL '+'                                          
066600         MOVE MID-IDDISTR-UT TO WS-IDDISTR                                
066700         INSPECT WS-IDDISTR REPLACING ALL  SPACE BY ZERO                  
066800     ELSE                                                                 
066900         MOVE MID-IDDISTR-IN TO WS-IDDISTR                                
067000     END-IF                                                               
067100                                                                          
067200     IF MID-IDKUNDNR-IN = ALL '+'                                         
067300         MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                              
067400         INSPECT WS-IDKUNDNR REPLACING ALL  SPACE BY ZERO                 
067500     ELSE                                                                 
067600         MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                              
067700     END-IF                                                               
067800                                                                          
067900     IF MID-IDORDNR-IN = ALL '+'                                          
068000         MOVE MID-IDORDNR-UT TO WS-IDORDNR                                
068100         INSPECT WS-IDORDNR REPLACING ALL  SPACE BY ZERO                  
068200     ELSE                                                                 
068300         MOVE MID-IDORDNR-IN TO WS-IDORDNR                                
068400     END-IF                                                               
068500                                                                          
068600     IF MID-IDKOLLI-IN = ALL '+'                                          
068700         MOVE MID-IDKOLLI-UT TO WS-IDKOLLI WS-IDKOLLI-NUM5                
068800         INSPECT WS-IDKOLLI REPLACING ALL  SPACE BY ZERO                  
068900     ELSE                                                                 
069000         MOVE MID-IDKOLLI-IN TO WS-IDKOLLI WS-IDKOLLI-NUM5                
069100     END-IF                                                               
069200                                                                          
069300     IF MID-IDDC-IN = ALL '+'                                             
069400       IF MID-IDDC-UT = SPACE                                             
069500         MOVE JA             TO WS-FEL-FUNNET                             
069600       ELSE                                                               
069700         MOVE MID-IDDC-UT                 TO WS-IDDC                      
069800       END-IF                                                             
069900     ELSE                                                                 
070000       MOVE MID-IDDC-IN                   TO WS-IDDC                      
070100     END-IF                                                               
070200                                                                          
070300     IF WS-IDDC IS > SPACE                                                
070400       CONTINUE                                                           
070500     ELSE                                                                 
070600       MOVE JA             TO WS-FEL-FUNNET                               
070700     END-IF                                                               
070800                                                                          
070900     IF MID-IDPRODNR-IN = ALL '+'                                         
071000       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
071100       INSPECT WS-IDPRODNR REPLACING ALL SPACE BY ZERO                    
071200       IF MID-IDPRODNR-UT > ZERO                                          
071300         MOVE JA            TO IDPRODNR-IFYLLT-SW                         
071400       END-IF                                                             
071500     ELSE                                                                 
071600       MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                                
071700       MOVE JA              TO IDPRODNR-IFYLLT-SW                         
071800     END-IF                                                               
071900     IF WS-IDPRODNR NOT NUMERIC                                           
072000       IF FEL-EJ-FUNNET                                                   
072100         MOVE JA                   TO WS-FEL-FUNNET                       
072200       END-IF                                                             
072300     END-IF                                                               
072400                                                                          
072500     MOVE LOW-VALUE           TO MSG-AREA                                 
072600     MOVE 'W4O338N1'          TO MFS-IDMOD                                
072700     MOVE '4338'              TO MOD-IDTRANS                              
072800     MOVE 4338-MOD-LAENGD     TO MSG-KVLL                                 
072900                                                                          
073000     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
073100     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
073200                                                                          
073300     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
073400     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
073500                                                                          
073600     MOVE WS-IDORDNR TO MOD-IDORDNR-UT                                    
073700     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
073800                                                                          
073900     MOVE WS-IDKOLLI TO MOD-IDKOLLI-UT                                    
074000     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
074100                                                                          
074200     MOVE WS-IDPRODNR    TO MOD-IDPRODNR-UT                               
074300     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
074400                                                                          
074500     MOVE WS-IDDC    TO MOD-IDDC-UT                                       
074600                                                                          
074700     IF SWEDISH-TEXT                                                      
074800         MOVE +1 TO INDX                                                  
074900     ELSE                                                                 
075000         MOVE +2 TO INDX                                                  
075100     END-IF                                                               
075200                                                                          
075300     MOVE ZERO                  TO LOGG-IDLOGLOP                          
075400                                                                          
075500     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
075600                                   MOD-IDKUNDNR-IN                        
075700                                   MOD-IDORDNR-IN                         
075800                                   MOD-IDKOLLI-IN                         
075900                                   MOD-IDPRODNR-IN                        
076000                                   MOD-IDDC-IN                            
076100                                   MOD-TEMFSFEL                           
076200                                   MOD-TEMFSINF                           
076300     MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPRTVAL-AF                        
076400                                   MOD-KDPRTVAL-FS                        
076500     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRTVAL-AF-ATTR                   
076600                                   MOD-KDPRTVAL-FS-ATTR                   
076700     .                                                                    
076800     EJECT                                                                
076900 B-BEHANDLA-INDATA SECTION.                                               
077000                                                                          
077100     MOVE WS-IDKUNDNR           TO WS-IDKUNDNR-NUM                        
077200                                                                          
077300     IF WS-IDDISTR  NUMERIC AND                                           
077400        WS-IDKUNDNR NUMERIC AND                                           
077500        WS-IDORDNR  NUMERIC AND                                           
077600        WS-IDKOLLI  NUMERIC AND                                           
077700        FEL-EJ-FUNNET                                                     
077800                                                                          
077900        IF IDPRODNR-IFYLLT                                                
078000                                                                          
078100          MOVE WS-IDPRODNR           TO W-E601-IDPRODNR                   
078200          PERFORM IMS-GU-OGAG-VORD                                        
078300          IF SEGMENT-FINNS                                                
078400            MOVE OGAG-VORD-IDDISTR   TO WS-IDDISTR-NUM                    
078500            MOVE OGAG-VORD-IDKUNDNR  TO WS-IDKUNDNR-7                     
078600            MOVE WS-IDKUNDNR-7(2:6)  TO WS-IDKUNDNR                       
078700            MOVE OGAG-VORD-IDPRODNR TO W-WDE4E1KY-MAX                     
078800                                   W-IDPRODNR-WDE4E-MIN                   
078900            PERFORM IMS-GN-WDE401-ESEQ                                    
079000            IF SEGMENT-FINNS                                              
079100              MOVE OGAG-KORD-IDORDNR5 TO WS-IDORDNR                       
079200                                                                          
079300            END-IF                                                        
079400          ELSE                                                            
079500            MOVE SPACE               TO WS-IDDISTR                        
079600                                           WS-IDKUNDNR                    
079700                                           WS-IDKUNDRF                    
079800          END-IF                                                          
079900        END-IF                                                            
080000                                                                          
080100        MOVE WS-IDDISTR-NUM          TO TEST-IDDISTR                      
080200                                                                          
080300        IF DIST03-SVERIGE                                                 
080400           MOVE MID-KDKOLLI          TO W-K501-KDKOLLI                    
080500           PERFORM IMS-GU-K501-KVAL                                       
080600                                                                          
080700           IF SEGMENT-SAKNAS                                              
080800              IF FEL-EJ-FUNNET                                            
080900                 MOVE FEL-726(INDX)  TO MOD-TEMFSFEL                      
081000                 MOVE JA             TO WS-FEL-FUNNET                     
081100              END-IF                                                      
081200              PERFORM S01-ROER-EJ-MODFAELT                                
081300              PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                        
081400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKOLLI-ATTR                 
081500           ELSE                                                           
081600              PERFORM BA-HAEMTA-DATA-I-WDK501                             
081700              PERFORM BB-FORMELL-KONTROLL                                 
081800                                                                          
081900              IF FEL-EJ-FUNNET                                            
082000                 PERFORM BI-HAEMTA-STARTNYCKEL                            
082100                                                                          
082200                 IF FEL-EJ-FUNNET                                         
082300                    PERFORM BC-HAEMTA-DATA-I-WDE601                       
082400                    MOVE WS-IDPRODNR   TO W-E601-IDPRODNR                 
082500                    MOVE WS-IDKOLLI    TO W-E611-IDKOLLI                  
082600                    PERFORM IMS-GHU-E601-E611-KVAL                        
082700                                                                          
082800                    IF SEGMENT-SAKNAS                                     
082900                       MOVE FEL-758(INDX) TO MOD-TEMFSFEL                 
083000                       MOVE JA            TO WS-FEL-FUNNET                
083100                       PERFORM S01-ROER-EJ-MODFAELT                       
083200                       PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT               
083300                    ELSE                                                  
083400                                                                          
083500                       IF KOLLI-KDKOLSTA = ZERO AND                       
083600                          KOLLI-FLBANDST = JA                             
083700                          MOVE KOLLI-SUORDV-KOLLI TO                      
083800                                                  WS-SUORDV-KOLLI         
083900                          MOVE KOLLI-SUORDV-LOC     TO                    
084000                                         WS-SUORDV-KOLLI-LOC              
084100                          MOVE KOLLI-SUORDV-LOCPREL TO                    
084200                                         WS-SUORDV-KOLLI-LOCPREL          
084300                          PERFORM BE-REDIGERA-WDE611                      
084400                          PERFORM IMS-REPLACE-E611                        
084500                          PERFORM BJ-UPPDAT-KDORDSTA                      
084600                                                                          
084700                          IF DIST03-SVERIGE-2                             
084800                            PERFORM BH-SKAPA-4322                         
084900                          END-IF                                          
085000                                                                          
085100                          MOVE WS-IDPRODNR   TO W-E601-IDPRODNR           
085200                          MOVE WS-IDKOLLI    TO W-E611-IDKOLLI            
085300                          PERFORM IMS-GHU-E601-KVAL                       
085400                          PERFORM BD-REDIGERA-WDE601                      
085500                          PERFORM IMS-REPLACE-E601                        
085600                                                                          
085700                          IF WS-FLAUTFAK = JA AND                         
085800                            (DIST03-SVERIGE-2 OR                          
085900                              VORD-KDORDSTA = 3 )                         
086000                             PERFORM BG-UPPDAT-4726-4727                  
086100                          END-IF                                          
086200                          IF WS-PRT-KDSVAR-FOLJEFL = RAETT                
086300                            PERFORM BF-PRINTA-FOELJESEDEL                 
086400                          END-IF                                          
086500                          IF WS-PRT-KDSVAR-ADRESSFL = RAETT               
086600                            PERFORM BK-PRINTA-ADRESSFLAGGA                
086700                          END-IF                                          
086800                          PERFORM BL-EV-BORTTAG-HTYP-4490-4302            
086900                       ELSE                                               
087000                          IF FEL-EJ-FUNNET                                
087100                             MOVE FEL-798(INDX) TO MOD-TEMFSFEL           
087200                             MOVE JA            TO WS-FEL-FUNNET          
087300                          END-IF                                          
087400                          PERFORM S01-ROER-EJ-MODFAELT                    
087500                          PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT            
087600                       END-IF                                             
087700                    END-IF                                                
087800                 END-IF                                                   
087900              END-IF                                                      
088000           END-IF                                                         
088100        ELSE                                                              
088200           IF FEL-EJ-FUNNET                                               
088300              IF NOT WS-GODKAEND-BILD                                     
088400                 PERFORM C-RENSA-NYCKLAR                                  
088500              END-IF                                                      
088600              MOVE FEL-797(INDX)     TO MOD-TEMFSFEL                      
088700              MOVE JA                TO WS-FEL-FUNNET                     
088800           END-IF                                                         
088900           PERFORM S01-ROER-EJ-MODFAELT                                   
089000           PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                           
089100        END-IF                                                            
089200     ELSE                                                                 
089300        IF NOT WS-GODKAEND-BILD                                           
089400           PERFORM C-RENSA-NYCKLAR                                        
089500        END-IF                                                            
089600        MOVE FEL-749(INDX)       TO MOD-TEMFSFEL                          
089700        MOVE JA                  TO WS-FEL-FUNNET                         
089800        PERFORM S01-ROER-EJ-MODFAELT                                      
089900        PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                              
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300 BA-HAEMTA-DATA-I-WDK501 SECTION.                                         
090400                                                                          
090500                                                                          
090600     MOVE EMB-KDKOLLI    TO WS-KDKOLLI                                    
090700     MOVE EMB-KDKOLLID   TO WS-KDKOLLID                                   
090800     MOVE EMB-KVLOCK     TO WS-KVLOCK                                     
090900     MOVE EMB-VKTARA     TO WS-EMB-VKTARA-ONE-CASE                        
091000     .                                                                    
091100     EJECT                                                                
091200 BB-FORMELL-KONTROLL SECTION.                                             
091300                                                                          
091400                                                                          
091500     PERFORM S01-ROER-EJ-MODFAELT                                         
091600     MOVE MFS-ALFA-FAELT-RAETT         TO MOD-KDKOLLI-ATTR                
091700                                          MOD-KDPRTVAL-FS-ATTR            
091800     MOVE MFS-NUM-FAELT-RAETT          TO MOD-KDEMBTYP-ATTR               
091900                                          MOD-DIKOLLIL-ATTR               
092000                                          MOD-DIKOLLIB-ATTR               
092100                                          MOD-DIKOLLIH-ATTR               
092200                                                                          
092300     IF MID-KDPRTVAL-AF = ALL '+'                                         
092400       IF FEL-EJ-FUNNET                                                   
092500         MOVE JA                  TO WS-FEL-FUNNET                        
092600         MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                         
092700       END-IF                                                             
092800       MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDPRTVAL-AF-ATTR                 
092900       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPRTVAL-AF                      
093000     ELSE                                                                 
093100                                                                          
093200       IF MID-KDPRTVAL-AF = 'U '                                          
093300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-AF-ATTR                
093400         MOVE MID-KDPRTVAL-AF      TO WS-KDPRTVAL-AF                      
093500       ELSE                                                               
093600         MOVE '4'                  TO WS-SYSTDEL                          
093700         MOVE 'KF'                 TO WS-LISTTYP                          
093800         MOVE WS-IDDC              TO WS-DC                               
093900         MOVE MID-KDPRTVAL-AF      TO WS-KDPRT                            
094000                                                                          
094100         MOVE 001                  TO PRT-KDCALL                          
094200         MOVE WS-IDPRTLST          TO PRT-IDPRTLST                        
094300                                                                          
094400         CALL W006PRT USING PRT-W006PRT                                   
094500                                                                          
094600         IF PRT-KDSVAR = RAETT                                            
094700           MOVE PRT-KDSVAR         TO WS-PRT-KDSVAR-ADRESSFL              
094800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-AF-ATTR              
094900           MOVE MID-KDPRTVAL-AF    TO WS-KDPRTVAL-AF                      
095000         ELSE                                                             
095100           IF FEL-EJ-FUNNET                                               
095200             MOVE JA                TO WS-FEL-FUNNET                      
095300             MOVE FEL-748 (INDX)    TO MOD-TEMFSFEL                       
095400           END-IF                                                         
095500           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDPRTVAL-AF-ATTR               
095600         END-IF                                                           
095700       END-IF                                                             
095800       MOVE MFS-ROER-EJ-FAELT       TO MOD-KDPRTVAL-AF                    
095900     END-IF                                                               
096000                                                                          
096100                                                                          
096200     IF MID-KDPRTVAL-FS = ALL '+'                                         
096300       IF FEL-EJ-FUNNET                                                   
096400           MOVE FEL-748(INDX)        TO MOD-TEMFSFEL                      
096500           MOVE JA                   TO WS-FEL-FUNNET                     
096600       END-IF                                                             
096700       MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDPRTVAL-FS-ATTR              
096800       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDPRTVAL-FS                   
096900     ELSE                                                                 
097000                                                                          
097100       IF MID-KDPRTVAL-FS = 'U '                                          
097200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FS-ATTR                
097300         MOVE MID-KDPRTVAL-FS      TO WS-KDPRTVAL-FS                      
097400       ELSE                                                               
097500         MOVE '4'                  TO WS-SYSTDEL                          
097600         MOVE 'FS'                 TO WS-LISTTYP                          
097700         MOVE WS-IDDC              TO WS-DC                               
097800         MOVE MID-KDPRTVAL-FS      TO WS-KDPRT                            
097900                                                                          
098000         MOVE 001                  TO PRT-KDCALL                          
098100         MOVE WS-IDPRTLST          TO PRT-IDPRTLST                        
098200                                                                          
098300         CALL W006PRT USING PRT-W006PRT                                   
098400                                                                          
098500         IF PRT-KDSVAR = RAETT                                            
098600           MOVE PRT-KDSVAR           TO WS-PRT-KDSVAR-FOLJEFL             
098700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FS-ATTR              
098800           MOVE MID-KDPRTVAL-FS      TO WS-KDPRTVAL-FS                    
098900         ELSE                                                             
099000           IF FEL-EJ-FUNNET                                               
099100               MOVE FEL-748(INDX)    TO MOD-TEMFSFEL                      
099200               MOVE JA               TO WS-FEL-FUNNET                     
099300           END-IF                                                         
099400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-FS-ATTR              
099500         END-IF                                                           
099600       END-IF                                                             
099700       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDPRTVAL-FS                   
099800     END-IF                                                               
099900                                                                          
100000                                                                          
100100     IF MID-KDEMBTYP = ALL '+'                                            
100200         IF EMB-KDEMBTYP = ZERO                                           
100300             IF FEL-EJ-FUNNET                                             
100400                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
100500                 MOVE JA               TO WS-FEL-FUNNET                   
100600             END-IF                                                       
100700             MOVE MFS-NUM-FAELT-FEL    TO MOD-KDEMBTYP-ATTR               
100800         ELSE                                                             
100900             MOVE EMB-KDEMBTYP         TO MOD-KDEMBTYP                    
101000                                          WS-KDEMBTYP                     
101100         END-IF                                                           
101200     ELSE                                                                 
101300         INSPECT MID-KDEMBTYP REPLACING LEADING SPACE BY ZERO             
101400         IF MID-KDEMBTYP NUMERIC                                          
101500             IF MID-KDEMBTYP = ZERO                                       
101600                 IF EMB-KDEMBTYP = ZERO                                   
101700                     IF FEL-EJ-FUNNET                                     
101800                         MOVE FEL-761(INDX) TO MOD-TEMFSFEL               
101900                         MOVE JA            TO WS-FEL-FUNNET              
102000                     END-IF                                               
102100                     MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-ATTR          
102200                 ELSE                                                     
102300                     MOVE EMB-KDEMBTYP      TO MOD-KDEMBTYP               
102400                                               WS-KDEMBTYP                
102500                 END-IF                                                   
102600             ELSE                                                         
102700                 MOVE MID-KDEMBTYP          TO WS-KDEMBTYP                
102800             END-IF                                                       
102900         ELSE                                                             
103000             IF FEL-EJ-FUNNET                                             
103100                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
103200                 MOVE JA                    TO WS-FEL-FUNNET              
103300             END-IF                                                       
103400             MOVE MFS-NUM-FAELT-FEL         TO MOD-KDEMBTYP-ATTR          
103500         END-IF                                                           
103600     END-IF                                                               
103700                                                                          
103800     IF MID-DIKOLLIL = ALL '+'                                            
103900         IF EMB-DIKOLLIL = ZERO                                           
104000             IF FEL-EJ-FUNNET                                             
104100                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
104200                 MOVE JA               TO WS-FEL-FUNNET                   
104300             END-IF                                                       
104400             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIL-ATTR               
104500         ELSE                                                             
104600             MOVE EMB-DIKOLLIL         TO MOD-DIKOLLIL                    
104700                                          WS-DIKOLLIL                     
104800         END-IF                                                           
104900     ELSE                                                                 
105000         INSPECT MID-DIKOLLIL REPLACING LEADING SPACE BY ZERO             
105100         IF MID-DIKOLLIL NUMERIC                                          
105200             IF MID-DIKOLLIL = ZERO                                       
105300                 IF EMB-DIKOLLIL = ZERO                                   
105400                     IF FEL-EJ-FUNNET                                     
105500                         MOVE FEL-761(INDX) TO MOD-TEMFSFEL               
105600                         MOVE JA            TO WS-FEL-FUNNET              
105700                     END-IF                                               
105800                     MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-ATTR          
105900                 ELSE                                                     
106000                     MOVE EMB-DIKOLLIL      TO MOD-DIKOLLIL               
106100                                               WS-DIKOLLIL                
106200                 END-IF                                                   
106300             ELSE                                                         
106400                 MOVE MID-DIKOLLIL          TO WS-DIKOLLIL                
106500             END-IF                                                       
106600         ELSE                                                             
106700             IF FEL-EJ-FUNNET                                             
106800                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
106900                 MOVE JA                    TO WS-FEL-FUNNET              
107000             END-IF                                                       
107100             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIL-ATTR          
107200         END-IF                                                           
107300     END-IF                                                               
107400                                                                          
107500     IF MID-DIKOLLIB = ALL '+'                                            
107600         IF EMB-DIKOLLIB = ZERO                                           
107700             IF FEL-EJ-FUNNET                                             
107800                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
107900                 MOVE JA               TO WS-FEL-FUNNET                   
108000             END-IF                                                       
108100             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIB-ATTR               
108200         ELSE                                                             
108300             MOVE EMB-DIKOLLIB         TO MOD-DIKOLLIB                    
108400                                          WS-DIKOLLIB                     
108500         END-IF                                                           
108600     ELSE                                                                 
108700         INSPECT MID-DIKOLLIB REPLACING LEADING SPACE BY ZERO             
108800         IF MID-DIKOLLIB NUMERIC                                          
108900             IF MID-DIKOLLIB = ZERO                                       
109000                 IF EMB-DIKOLLIB = ZERO                                   
109100                     IF FEL-EJ-FUNNET                                     
109200                         MOVE FEL-761(INDX) TO MOD-TEMFSFEL               
109300                         MOVE JA            TO WS-FEL-FUNNET              
109400                     END-IF                                               
109500                     MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-ATTR          
109600                 ELSE                                                     
109700                     MOVE EMB-DIKOLLIB      TO MOD-DIKOLLIB               
109800                                               WS-DIKOLLIB                
109900                 END-IF                                                   
110000             ELSE                                                         
110100                 MOVE MID-DIKOLLIB          TO WS-DIKOLLIB                
110200             END-IF                                                       
110300         ELSE                                                             
110400             IF FEL-EJ-FUNNET                                             
110500                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
110600                 MOVE JA                    TO WS-FEL-FUNNET              
110700             END-IF                                                       
110800             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIB-ATTR          
110900         END-IF                                                           
111000     END-IF                                                               
111100                                                                          
111200     IF MID-DIKOLLIH = ALL '+'                                            
111300         IF EMB-DIKOLLIH = ZERO                                           
111400             IF FEL-EJ-FUNNET                                             
111500                 MOVE FEL-761(INDX)    TO MOD-TEMFSFEL                    
111600                 MOVE JA               TO WS-FEL-FUNNET                   
111700             END-IF                                                       
111800             MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIH-ATTR               
111900         ELSE                                                             
112000             MOVE EMB-DIKOLLIH         TO MOD-DIKOLLIH                    
112100                                          WS-DIKOLLIH                     
112200         END-IF                                                           
112300     ELSE                                                                 
112400         INSPECT MID-DIKOLLIH REPLACING LEADING SPACE BY ZERO             
112500         IF MID-DIKOLLIH NUMERIC                                          
112600             IF MID-DIKOLLIH = ZERO                                       
112700                 IF EMB-DIKOLLIH = ZERO                                   
112800                     IF FEL-EJ-FUNNET                                     
112900                         MOVE FEL-761(INDX) TO MOD-TEMFSFEL               
113000                         MOVE JA            TO WS-FEL-FUNNET              
113100                     END-IF                                               
113200                     MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-ATTR          
113300                 ELSE                                                     
113400                     MOVE EMB-DIKOLLIH      TO MOD-DIKOLLIH               
113500                                               WS-DIKOLLIH                
113600                 END-IF                                                   
113700             ELSE                                                         
113800                 MOVE MID-DIKOLLIH          TO WS-DIKOLLIH                
113900             END-IF                                                       
114000         ELSE                                                             
114100             IF FEL-EJ-FUNNET                                             
114200                 MOVE FEL-763(INDX)         TO MOD-TEMFSFEL               
114300                 MOVE JA                    TO WS-FEL-FUNNET              
114400             END-IF                                                       
114500             MOVE MFS-NUM-FAELT-FEL         TO MOD-DIKOLLIH-ATTR          
114600         END-IF                                                           
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 BC-HAEMTA-DATA-I-WDE601 SECTION.                                         
115100                                                                          
115200                                                                          
115300     MOVE VORD-IDDC       TO WS-IDDC                                      
115400     MOVE VORD-IDPRODNR   TO WS-IDPRODNR                                  
115500     MOVE VORD-KDORDKL    TO WS-KDORDKL                                   
115600     MOVE VORD-FLAUTFAK   TO WS-FLAUTFAK                                  
115700     MOVE VORD-KDFAKTYP   TO WS-KDFAKTYP                                  
115800                                                                          
115900     IF VORD-IDDISTR > 799                                                
116000         MOVE 'EXP'       TO WS-DISTRIKT-TYP                              
116100     ELSE                                                                 
116200         MOVE 'SVE'       TO WS-DISTRIKT-TYP                              
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600 BD-REDIGERA-WDE601        SECTION.                                       
116700                                                                          
116800     ADD 1                TO VORD-KVKOLLI                                 
116900                                                                          
117000     PERFORM BDB-EV-FRAKTSEDEL-SVERIGE                                    
117100     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
117200         IF VORD-KVKOLLI   = VORD-KVKOLPAC                                
117300         AND VORD-KDORDSTA NOT > 2                                        
117400           PERFORM BDA-KTRL-VORD-FARDIGPACKAD                             
117500           IF  WS-VORD-FARDIGPACKAD = JA                                  
117600             PERFORM S04-GENERERA-KLAR-SV4                                
117700             MOVE 3       TO VORD-KDORDSTA                                
117800           END-IF                                                         
117900         END-IF                                                           
118000     END-IF                                                               
118100                                                                          
118200     ACCEPT VORD-TIPACKN-SK FROM DATE                                     
118300                                                                          
118400     COMPUTE VORD-VLORDBTO =                                              
118500     VORD-VLORDBTO + WS-VLORDBTO                                          
118600                                                                          
118700     ADD WS-SUORDV-KOLLI TO VORD-SUORDV-PACK                              
118800                                                                          
118900     COMPUTE VORD-VKORDBTO =                                              
119000             VORD-VKORDBTO + WS-EMB-VKTARA-ONE-CASE                       
119100                                                                          
119200     ADD WS-SUORDV-KOLLI-LOC     TO VORD-SUORDV-PACK-LOC                  
119300     ADD WS-SUORDV-KOLLI-LOCPREL TO VORD-SUORDV-PACK-LOCPREL              
119400     .                                                                    
119500     EJECT                                                                
119600 BDA-KTRL-VORD-FARDIGPACKAD SECTION.                                      
119700     SKIP2                                                                
119800     MOVE JA                 TO WS-VORD-FARDIGPACKAD                      
119900                                                                          
120000     MOVE VORD-IDPRODNR      TO W-E601-IDPRODNR                           
120100     PERFORM IMS-GU-OGAG-VORD                                             
120200                                                                          
120300     MOVE OGAG-VORD-IDDC     TO W-Q301-MIN-IDDC                           
120400                                W-Q301-MAX-IDDC                           
120500     MOVE OGAG-VORD-IDPRODNR TO W-Q301-MIN-IDPRODNR                       
120600                                W-Q301-MAX-IDPRODNR                       
120700     MOVE OGAG-VORD-IDPRODNR TO W-WDE4E1KY-MAX                            
120800                                W-IDPRODNR-WDE4E-MIN                      
120900                                                                          
121000     PERFORM IMS-GN-WDE401-ESEQ                                           
121100                                                                          
121200     IF  SEGMENT-FINNS                                                    
121300*      * IDORDER SPARAS FRÅN 1:A KORD FÖR NYCKEL TILL WDQ3                
121400       MOVE OGAG-KORD-IDORDER  TO W-Q301-MIN-IDORDER                      
121500                                  W-Q301-MAX-IDORDER                      
121600                                                                          
121700       PERFORM UNTIL ((NOT SEGMENT-FINNS)                                 
121800               OR     WS-VORD-FARDIGPACKAD = NEJ)                         
121900                                                                          
122000                                                                          
122100         IF (OGAG-KORD-KVORDRAD-PACK <                                    
122200             OGAG-KORD-KVORDRAD + OGAG-KORD-KVORDRAD-LEVPL)               
122300                                                                          
122400           MOVE NEJ            TO WS-VORD-FARDIGPACKAD                    
122500         ELSE                                                             
122600           PERFORM IMS-GN-WDE401-ESEQ                                     
122700         END-IF                                                           
122800       END-PERFORM                                                        
122900                                                                          
123000       IF  WS-VORD-FARDIGPACKAD = JA                                      
123100                                                                          
123200         PERFORM IMS-GU-ORQA-ODEL                                         
123300                                                                          
123400         PERFORM UNTIL ((NOT SEGMENT-FINNS)                               
123500                 OR   WS-VORD-FARDIGPACKAD = NEJ)                         
123600                                                                          
123700           IF  ORQA-ODEL-KDODELSTA = 'R'                                  
123800             MOVE NEJ        TO WS-VORD-FARDIGPACKAD                      
123900           ELSE                                                           
124000             PERFORM IMS-GN-ORQA-ODEL                                     
124100           END-IF                                                         
124200         END-PERFORM                                                      
124300       END-IF                                                             
124400     ELSE                                                                 
124500       MOVE NEJ            TO WS-VORD-FARDIGPACKAD                        
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900 BDB-EV-FRAKTSEDEL-SVERIGE  SECTION.                                      
125000                                                                          
125100     MOVE VORD-IDDISTR       TO TEST-IDDISTR                              
125200                                                                          
125300     IF WS-IDDC NOT = W-IDDC-B6                                           
125400        MOVE WS-IDDC TO W-IDDC-B6                                         
125500        PERFORM IMS-GU-WDB601                                             
125600     END-IF                                                               
125700                                                                          
125800     IF  VORD-KDORDSTA < +3                                               
125900     AND VORD-KVKOLLI  > +0                                               
126000     AND DIST03-SVERIGE                                                   
126100     AND DCS-CDC                                                          
126200                                                                          
126300       MOVE VORD-KDFRAKT   TO WS-KDFRAKT                                  
126400       MOVE WS-KDFRAKT     TO FRAK01-KDFRAKT                              
126500       IF  FRAK01-SVERIGE2                                                
126600       OR  FRAK01-NORDEN                                                  
126700       OR  FRAK01-KDFRAKT21                                               
126800       OR  FRAK01-KDFRAKT62                                               
126900       OR (WS-IDKOLLI-NUM5 > 349 AND WS-IDKOLLI-NUM5 < 400)               
127000          IF   VORD-FLDIRLEV = NEJ                                        
127100          AND  VORD-KDFRAKT  NOT = +17                                    
127200            IF NOT DIS128-FRAKTS                                          
127300              MOVE JA TO VORD-FLFRAKTS                                    
127400            END-IF                                                        
127500          END-IF                                                          
127600       END-IF                                                             
127700     END-IF                                                               
127800     .                                                                    
127900     SKIP2                                                                
128000 BE-REDIGERA-WDE611 SECTION.                                              
128100                                                                          
128200                                                                          
128300     MOVE ZERO               TO KOLLI-IDKOLLI-FLER                        
128400     MOVE WS-IDTRPTNR        TO KOLLI-IDTRPTNR                            
128500     MOVE WS-IDDC            TO KOLLI-IDDC                                
128600     MOVE 'SVE'              TO KOLLI-ADFLGEO                             
128700     MOVE 1                  TO KOLLI-ADFLOMR                             
128800                                KOLLI-ADRUTNIV                            
128900     MOVE ZERO               TO KOLLI-ADVMODUL                            
129000                                KOLLI-ADHMODUL                            
129100                                KOLLI-DIHMODUL                            
129200                                KOLLI-DIDMODUL                            
129300     MOVE NEJ                TO KOLLI-FLUTLAST                            
129400     MOVE 1                  TO KOLLI-KDKOLSTA                            
129500     ACCEPT KOLLI-TIPACKN FROM DATE                                       
129600     ACCEPT WS-TIPACTID-8 FROM TIME                                       
129700     MOVE WS-TIPACTID-8      TO WS-TIPACTID-8-6                           
129800                                                                          
129900     MOVE WS-TIPACTID-6      TO KOLLI-TIPACTID                            
130000     MOVE WS-DIKOLLIL        TO KOLLI-DIKOLLIL                            
130100     MOVE WS-DIKOLLIB        TO KOLLI-DIKOLLIB                            
130200     MOVE WS-DIKOLLIH        TO KOLLI-DIKOLLIH                            
130300     MOVE WS-KDEMBTYP        TO KOLLI-KDEMBTYP                            
130400     MOVE WS-KDKOLLI         TO KOLLI-KDKOLLI                             
130500                                                                          
130600     COMPUTE WS-VLORDBTO ROUNDED =                                        
130700     WS-DIKOLLIL * WS-DIKOLLIH * WS-DIKOLLIB / 1000000                    
130800                                                                          
130900     MOVE WS-VLORDBTO        TO KOLLI-VLORDBTO-KOLLI                      
131000                                                                          
131100     COMPUTE KOLLI-VKORDBTO-KOLLI =                                       
131200             KOLLI-VKORDBTO-KOLLI + WS-EMB-VKTARA-ONE-CASE                
131300     .                                                                    
131400     EJECT                                                                
131500 BF-PRINTA-FOELJESEDEL SECTION.                                           
131600                                                                          
131700     MOVE WS-IDDISTR         TO 4341-MID-IDDISTR-UT                       
131800     MOVE WS-IDKUNDNR        TO 4341-MID-IDKUNDNR-UT                      
131900     MOVE WS-IDORDNR         TO 4341-MID-IDORDNR-UT                       
132000     MOVE WS-IDKOLLI         TO 4341-MID-IDKOLLI-UT                       
132100     MOVE ZERO               TO 4341-MID-IDKOLLI-TOM-UT                   
132200     MOVE WS-KDPRTVAL-FS     TO 4341-MID-KDPRTVAL-UT                      
132300     MOVE WS-IDDC            TO 4341-MID-IDDC-UT                          
132400     MOVE '++++'             TO 4341-MID-IDDISTR-IN                       
132500     MOVE '++++++'           TO 4341-MID-IDKUNDNR-IN                      
132600     MOVE '+++++'            TO 4341-MID-IDORDNR-IN                       
132700     MOVE '+++++'            TO 4341-MID-IDKOLLI-IN                       
132800     MOVE '+++++'            TO 4341-MID-IDKOLLI-TOM-IN                   
132900     MOVE '++'               TO 4341-MID-KDPRTVAL-IN                      
133000     MOVE '++'               TO 4341-MID-IDDC-IN                          
133100     MOVE 'N'                TO 4341-MID-FL-SVENSK-FSEDEL                 
133200                                                                          
133300     COMPUTE 4341-MID-KVLL = LENGTH OF 4341-MID-W4I34101 + 17             
133400     MOVE LOW-VALUE             TO 4341-MID-Z1                            
133500                                   4341-MID-Z2                            
133600     MOVE 'W4T341  '         TO 4341-MID-TRANSKOD                         
133700     MOVE '433H'             TO 4341-MID-IDTRANS                          
133800     MOVE 1                  TO 4341-MID-KDMFSFOR                         
133900                                                                          
134000     PERFORM IMS-INSERT-ALT-MSG                                           
134100     .                                                                    
134200     EJECT                                                                
134300 BG-UPPDAT-4726-4727 SECTION.                                             
134400                                                                          
134500                                                                          
134600     IF WS-IDDC NOT = W-IDDC-B6                                           
134700        MOVE WS-IDDC TO W-IDDC-B6                                         
134800        PERFORM IMS-GU-WDB601                                             
134900     END-IF                                                               
135000                                                                          
135100       MOVE '4726'              TO W-4726-IDHTYP                          
135200       MOVE WS-IDDISTR          TO TEST-IDDISTR                           
135300       IF DIST19-SATS                                                     
135400           MOVE NEJ             TO W-4726-FLBATCH                         
135500       ELSE                                                               
135600*          IF SVERIGE-DISTRIKT                                            
135700*             IF DCS-CDC                                                  
135800*             OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')                        
135900*             OR DIST18-SKROT                                             
136000*                 MOVE JA       TO W-4726-FLBATCH                         
136100*              ELSE                                                       
136200*                 MOVE NEJ      TO W-4726-FLBATCH                         
136300*             END-IF                                                      
136400*           ELSE                                                          
136500              MOVE NEJ          TO W-4726-FLBATCH                         
136600*          END-IF                                                         
136700       END-IF                                                             
136800       MOVE LOW-VALUE           TO W-4726-LOWVALUE                        
136900                                                                          
137000       PERFORM IMS-GU-4726-ROT-KVAL                                       
137100                                                                          
137200       MOVE WS-IDDISTR          TO W-4726-IDDISTR                         
137300       MOVE WS-IDKUNDNR         TO W-4726-IDKUNDNR                        
137400       MOVE WS-IDDC             TO W-4726-IDDC                            
137500       MOVE WS-KDFAKTYP         TO W-4726-KDFAKTYP                        
137600                                                                          
137700       PERFORM IMS-GNP-4726-UNDSEG-KVAL                                   
137800                                                                          
137900       IF SEGMENT-SAKNAS                                                  
138000           MOVE WS-IDDISTR      TO AUTFAKT-IDDISTR                        
138100           MOVE WS-IDKUNDNR     TO AUTFAKT-IDKUNDNR                       
138200           MOVE WS-IDDC         TO AUTFAKT-IDDC                           
138300           MOVE WS-KDFAKTYP     TO AUTFAKT-KDFAKTYP                       
138400                                                                          
138500           PERFORM IMS-INSERT-4726-UNDSEG                                 
138600                                                                          
138700       END-IF                                                             
138800       MOVE WS-IDPRODNR         TO AUTFAKT-IDPRODNR                       
138900       MOVE ZERO                TO AUTFAKT-IDSKEPPN                       
139000                                   AUTFAKT-PRFRAKT                        
139100                                                                          
139200       IF SVERIGE-DISTRIKT                                                
139300           MOVE NEJ             TO AUTFAKT-FLLASTA                        
139400       ELSE                                                               
139500           MOVE JA              TO AUTFAKT-FLLASTA                        
139600       END-IF                                                             
139700                                                                          
139800       PERFORM IMS-INSERT-4727                                            
139900     .                                                                    
140000     EJECT                                                                
140100 BH-SKAPA-4322 SECTION.                                                   
140200                                                                          
140300     IF NOT DIST47-INTERNA                                                
140400     OR DIST21-TYRE                                                       
140500        MOVE WS-IDPRODNR         TO XXJK-4322-IDPRODNR                    
140600        MOVE WS-IDKOLLI          TO XXJK-4322-IDKOLLI                     
140700        MOVE XXJK-4322-WDGX4322  TO 4322-WDGX4322                         
140800        PERFORM IMS-ISRT-4322-SEGM                                        
140900     END-IF                                                               
141000     .                                                                    
141100     EJECT                                                                
141200 BI-HAEMTA-STARTNYCKEL SECTION.                                           
141300                                                                          
141400*                                                                         
141500     MOVE 'N'               TO WS-SLINGA-KLAR                             
141600*                                                                         
141700     MOVE WS-IDDISTR        TO W-E4A1-IDDISTR                             
141800     MOVE WS-IDKUNDNR       TO W-E4A1-IDKUNDNR                            
141900     MOVE SPACE             TO W-E4A1-IDKUNDRF                            
142000     MOVE WS-IDKUNDRF       TO W-E4A1-IDKUNDRF                            
142100     PERFORM IMS-GU-E4A1-SEK-KVAL                                         
142200                                                                          
142300     IF KUNDORDER-SEK-FINNS                                               
142400        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
142500                      SLINGA-KLAR                                         
142600        MOVE KORD-IDDISTR   TO W-E401-IDDISTR                             
142700        MOVE KORD-IDKUNDNR  TO W-E401-IDKUNDNR                            
142800        MOVE KORD-IDORDNR5  TO W-E401-IDORDNR                             
142900        MOVE KORD-IDPRODNR  TO W-E401-IDPRODNR                            
143000        MOVE KORD-IDPLKLST  TO W-E401-IDPLKLST                            
143100        PERFORM IMS-GU-E401-KVAL                                          
143200        MOVE KORD-KVORDRAD-LEVPL   TO WS-SPAR-KVORDRAD-LEVPL              
143300        MOVE KORD-IDORDER          TO W-201-IDORDER                       
143400*                                                                         
143500        MOVE KORD-IDPRODNR TO W-E601-IDPRODNR                             
143600        PERFORM IMS-GU-E601                                               
143700                                                                          
143800        IF SEGMENT-FINNS                                                  
143900        AND VORD-IDDC              = WS-IDDC                              
144000        AND WS-SPAR-KVORDRAD-LEVPL = ZERO                                 
144100           MOVE 'J'           TO WS-SLINGA-KLAR                           
144200           MOVE VORD-IDPRODNR TO W-E601-IDPRODNR                          
144300        ELSE                                                              
144400*                                                                         
144500           PERFORM IMS-GN-E4A1-SEK-KVAL                                   
144600        END-IF                                                            
144700        END-PERFORM                                                       
144800     END-IF                                                               
144900*                                                                         
145000     IF NOT SLINGA-KLAR                                                   
145100                                                                          
145200        IF IDPRODNR-IFYLLT                                                
145300          MOVE FEL-764(INDX)  TO MOD-TEMFSFEL                             
145400        ELSE                                                              
145500          MOVE FEL-762(INDX)  TO MOD-TEMFSFEL                             
145600        END-IF                                                            
145700        MOVE JA               TO WS-FEL-FUNNET                            
145800        PERFORM S01-ROER-EJ-MODFAELT                                      
145900        PERFORM S02-SAETT-LAES-IGEN-ATTRIBUT                              
146000     END-IF                                                               
146100     .                                                                    
146200     EJECT                                                                
146300 BJ-UPPDAT-KDORDSTA SECTION.                                              
146400                                                                          
146500     MOVE WS-IDDC              TO W-212-IDDC                              
146600     PERFORM IMS-GHU-WDQ212                                               
146800     IF SEGMENT-FINNS                                                     
146900        IF ARB-KDORDSTA = 'U '                                            
147000           MOVE 'U*'            TO ARB-KDORDSTA                           
147100        END-IF                                                            
147200        PERFORM IMS-REPL-WDQ212                                           
147300     ELSE                                                                 
147400        MOVE 'RADENS DC SAKNAS' TO FELTEXT                                
147500        CALL FELLOG USING RKOD-ABEND                                      
147600     END-IF                                                               
147700                                                                          
149500     .                                                                    
149600     EJECT                                                                
149700 BK-PRINTA-ADRESSFLAGGA SECTION.                                          
149800                                                                          
149900     IF WS-KDPRTVAL-AF NOT = 'U '                                         
150000        MOVE WS-IDDISTR         TO 4333-MID-IDDISTR-UT                    
150100        MOVE WS-IDKUNDNR        TO 4333-MID-IDKUNDNR-UT                   
150200        MOVE WS-IDORDNR         TO 4333-MID-IDORDNR-UT                    
150300        MOVE WS-IDKOLLI         TO 4333-MID-IDKOLLI-UT                    
150400        MOVE WS-IDDC            TO 4333-MID-IDDC-UT                       
150500        MOVE SPACE              TO 4333-MID-IDPRODNR-UT                   
150600        MOVE WS-KDPRTVAL-AF     TO 4333-MID-KDPRTVAL-UT                   
150700        MOVE ZERO               TO 4333-MID-IDKOLLI-TOM                   
150800        MOVE '++++'             TO 4333-MID-IDDISTR-IN                    
150900        MOVE '++++++'           TO 4333-MID-IDKUNDNR-IN                   
151000        MOVE '+++++'            TO 4333-MID-IDORDNR-IN                    
151100                                   4333-MID-IDKOLLI-IN                    
151200        MOVE '++'               TO 4333-MID-IDDC-IN                       
151300        MOVE '+++++++'          TO 4333-MID-IDPRODNR-IN                   
151400        MOVE '++'               TO 4333-MID-KDPRTVAL-IN                   
151500                                                                          
151600        COMPUTE 4333-MID-KVLL = LENGTH OF 4333-MID-W4I33301 + 17          
151700        MOVE LOW-VALUE          TO 4333-MID-Z1                            
151800                                   4333-MID-Z2                            
151900        MOVE 'W4T333  '         TO 4333-MID-TRANSKOD                      
152000        MOVE '433B'             TO 4333-MID-IDTRANS                       
152100        MOVE 1                  TO 4333-MID-KDMFSFOR                      
152200                                                                          
152300        PERFORM IMS-INSERT-4333-MSG                                       
152400     END-IF                                                               
152500     .                                                                    
152600     EJECT                                                                
152700 BL-EV-BORTTAG-HTYP-4490-4302    SECTION.                                 
152800                                                                          
152900     MOVE WS-IDPRODNR          TO W-4301-IDPRODNR                         
153000     MOVE JA                   TO 4490-BORTTAG-SW                         
153100                                  FOERSTA-SW                              
153200                                                                          
153300     PERFORM IMS-GU-XXDU01                                                
153400     IF SEGMENT-FINNS                                                     
153500       MOVE WS-IDKOLLI         TO W-4302-IDKOLLI                          
153600       PERFORM IMS-GNP-XXDU11                                             
153700                                                                          
153800       IF SEGMENT-FINNS                                                   
153900         MOVE WS-IDDISTR         TO W-E401-IDDISTR                        
154000         MOVE WS-IDKUNDNR        TO W-E401-IDKUNDNR                       
154100         MOVE WS-IDORDNR         TO W-E401-IDORDNR                        
154200         MOVE WS-IDPRODNR        TO W-E401-IDPRODNR                       
154300                                                                          
154400         PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                      
154500           MOVE 4302-IDPLKLST    TO W-E401-IDPLKLST                       
154600                                    W-4302-IDPLKLST                       
154700                                    WS-IDPLKLST                           
154800           PERFORM IMS-GU-E401-KVAL                                       
154900           IF SEGMENT-FINNS                                               
155000             MOVE WS-IDKOLLI     TO W-4302-IDKOLLI                        
155100             PERFORM BLA-BORTTAG-HTYP-4302                                
155200             IF KORD-KVORDRAD    NOT = KORD-KVORDRAD-PACK                 
155300               MOVE NEJ          TO 4490-BORTTAG-SW                       
155400             END-IF                                                       
155500           END-IF                                                         
155600                                                                          
155700           PERFORM IMS-GU-XXDU01-XXDU2                                    
155800           PERFORM IMS-GNP-XXDU11-IDPLKLST-XXDU2                          
155900*          LÄS MED IDPLKLST FÖR ATT FÅ EV. ANDRA IDKOLLIN                 
156000           IF SEGMENT-SAKNAS AND SEG-4490-BORTTAG                         
156100             PERFORM BLB-BORTTAG-4490                                     
156200           END-IF                                                         
156300           MOVE JA                TO 4490-BORTTAG-SW                      
156400           PERFORM IMS-GNP-XXDU11-IDKOLLI                                 
156500*          LÄS MED SAMMA IDKOLLI FÖR ATT FÅ EV. ANDRA IDPLKLSTOR          
156600         END-PERFORM                                                      
156700                                                                          
156800         PERFORM IMS-GNP-XXDU11-FIRST                                     
156900         IF SEGMENT-SAKNAS                                                
157000           PERFORM IMS-GHU-XXDU01                                         
157100           PERFORM IMS-DLET-XXDU                                          
157200         END-IF                                                           
157300       END-IF                                                             
157400     END-IF                                                               
157500     .                                                                    
157600     SKIP2                                                                
157700 BLA-BORTTAG-HTYP-4302       SECTION.                                     
157800                                                                          
157900     PERFORM IMS-GHU-XXDU01                                               
158000     PERFORM IMS-GHNP-XXDU11                                              
158100     PERFORM IMS-DLET-XXDU                                                
158200     .                                                                    
158300     SKIP2                                                                
158400 BLB-BORTTAG-4490      SECTION.                                           
158500                                                                          
158600     MOVE W-201-IDORDER        TO W-Q301-IDORDER                          
158700     MOVE WS-IDDC              TO W-Q301-IDDC                             
158800     MOVE WS-IDPRODNR          TO W-Q301-IDPRODNR                         
158900     MOVE WS-IDPLKLST          TO W-Q301-IDPLKLST                         
159000     PERFORM IMS-GU-ORQA01                                                
159100                                                                          
159200     MOVE ORQA-ODEL-IDDC       TO W-4447-IDDC                             
159300     MOVE ORQA-ODEL-IDPRC      TO W-4448-IDPRC                            
159400     PERFORM IMS-GU-XXKH11                                                
159500                                                                          
159600     MOVE ORQA-ODEL-IDDC       TO W-4487-IDDC                             
159700     MOVE 4448-KDPRCGRP        TO W-4488-KDPRCGRP                         
159800     MOVE ORQA-ODEL-DARFS      TO W-4490-DARFS                            
159900     MOVE ORQA-ODEL-IDPRODNR   TO W-4490-IDPRODNR                         
160000     MOVE ORQA-ODEL-IDPLKLST   TO W-4490-IDPLKLST                         
160100     PERFORM IMS-GHU-WDGX4490                                             
160200                                                                          
160300     IF SEGMENT-FINNS                                                     
160400        PERFORM IMS-DLET-WDGX4490                                         
160500     END-IF                                                               
160600     .                                                                    
160700     SKIP2                                                                
160800 C-RENSA-NYCKLAR SECTION.                                                 
160900     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                          
161000                                  MOD-IDKUNDNR-UT                         
161100                                  MOD-IDORDNR-UT                          
161200                                  MOD-IDKOLLI-UT                          
161300                                  MOD-IDDC-UT                             
161400                                  MOD-IDPRODNR-UT                         
161500     .                                                                    
161600     EJECT                                                                
161700 S01-ROER-EJ-MODFAELT SECTION.                                            
161800                                                                          
161900                                                                          
162000     MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKOLLI                               
162100                                MOD-KDEMBTYP                              
162200                                MOD-DIKOLLIL                              
162300                                MOD-DIKOLLIB                              
162400                                MOD-DIKOLLIH                              
162500     .                                                                    
162600     EJECT                                                                
162700 S02-SAETT-LAES-IGEN-ATTRIBUT SECTION.                                    
162800                                                                          
162900                                                                          
163000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKOLLI-ATTR                       
163100                                   MOD-KDEMBTYP-ATTR                      
163200                                   MOD-DIKOLLIL-ATTR                      
163300                                   MOD-DIKOLLIB-ATTR                      
163400                                   MOD-DIKOLLIH-ATTR                      
163500     .                                                                    
163600     EJECT                                                                
163700 S03-RENSA-MODFAELT SECTION.                                              
163800                                                                          
163900                                                                          
164000     MOVE MFS-RENSA-FAELT    TO MOD-KDKOLLI                               
164100                                MOD-KDEMBTYP                              
164200                                MOD-DIKOLLIL                              
164300                                MOD-DIKOLLIB                              
164400                                MOD-DIKOLLIH                              
164500     .                                                                    
164600     EJECT                                                                
164700 S04-GENERERA-KLAR-SV4 SECTION.                                           
164800                                                                          
164900     IF LOGG-IDLOGLOP = 9                                                 
165000       MOVE 0                TO LOGG-IDLOGLOP                             
165100     END-IF                                                               
165200                                                                          
165300     ACCEPT LOGG-TIAAMMDD    FROM DATE                                    
165400     ACCEPT LOGG-TIKLOCK     FROM TIME                                    
165500     ADD +1                  TO LOGG-IDLOGLOP                             
165600     MOVE 'RY6'              TO RY6-IDPTYP                                
165700                                LOGG-IDPTYP                               
165800     MOVE VORD-IDDISTR       TO RY6-IDDISTR                               
165900     MOVE VORD-IDKUNDNR      TO RY6-IDKUNDNR                              
166000     MOVE WS-IDKUNDRF        TO RY6-IDKUNDRF                              
166100     MOVE VORD-IDPRODNR      TO RY6-IDPRODNR                              
166200     MOVE VORD-IDDC          TO RY6-IDDC                                  
166300     MOVE SPACE              TO LOGG-SORTPOST                             
166400     MOVE RY6-WDGZRY6        TO LOGG-LOGGPOST                             
166500                                                                          
166600     PERFORM IMS-INSERT-KLAR-SV4                                          
166700                                                                          
166800     PERFORM UNTIL SEGMENT-FINNS                                          
166900       IF LOGG-IDLOGLOP = 9                                               
167000         MOVE 0              TO LOGG-IDLOGLOP                             
167100         ACCEPT LOGG-TIKLOCK FROM TIME                                    
167200       END-IF                                                             
167300       ADD 1                 TO LOGG-IDLOGLOP                             
167400       PERFORM IMS-INSERT-KLAR-SV4                                        
167500     END-PERFORM                                                          
167600     SKIP3                                                                
167700     .                                                                    
167800     EJECT                                                                
167900* IMS SEKTIONER                                                           
168000*                                                                         
168100 IMS-GET-MSG SECTION.                                                     
168200     MOVE '  QC' TO GODK-STATUSKODER                                      
168300     CALL CBLTDLI USING GU                                                
168400                          MSG-PCB                                         
168500                          MSG-IO-AREA                                     
168600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     SKIP3                                                                
168900     .                                                                    
169000 IMS-INSERT-MSG SECTION.                                                  
169100                                                                          
169200     IF NOT ENGLISH-TEXT                                                  
169300       MOVE '0' TO MFS-KDHUVOMR                                           
169400     END-IF                                                               
169500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
169600     MOVE SPACE TO GODK-STATUSKODER                                       
169700     CALL CBLTDLI USING ISRT                                              
169800                          MSG-PCB                                         
169900                          MSG-IO-AREA                                     
170000                          MFS-IDMOD                                       
170100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     SKIP3                                                                
170400     .                                                                    
170500 IMS-INSERT-ALT-MSG SECTION.                                              
170600     MOVE LOW-VALUE TO 4341-MID-Z1 4341-MID-Z2                            
170700     MOVE SPACE TO GODK-STATUSKODER                                       
170800     CALL CBLTDLI USING ISRT                                              
170900                          ALT-PCB                                         
171000                          4341-MID-IO-AREA                                
171100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400 IMS-INSERT-4333-MSG SECTION.                                             
171500     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
171600     MOVE SPACE TO GODK-STATUSKODER                                       
171700     CALL CBLTDLI USING ISRT                                              
171800                          4333-PCB                                        
171900                          4333-MID-IO-AREA                                
172000     MOVE 4333-STATUS-CODE TO STATUS-WS                                   
172100     PERFORM IMS-STATUSKONTROLL                                           
172200     .                                                                    
172300     EJECT                                                                
172400 IMS-GU-K501-KVAL SECTION.                                                
172500     STRING 'WLEMBB01(KDKOLLI  =' W-K501-KDKOLLI-X ')'                    
172600            DELIMITED BY SIZE INTO SSA1                                   
172700     MOVE '  GE' TO GODK-STATUSKODER                                      
172800     CALL CBLTDLI USING GU                                                
172900                          EMBB-PCB                                        
173000                          DLI-IO-AREA                                     
173100                          SSA1                                            
173200     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
173300     PERFORM IMS-STATUSKONTROLL                                           
173400     .                                                                    
173500     EJECT                                                                
173600 IMS-GU-E4A1-SEK-KVAL SECTION.                                            
173700     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
173800            DELIMITED BY SIZE INTO SSA1                                   
173900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
174000     CALL CBLTDLI USING GU                                                
174100                          WDE4A-PCB                                       
174200                          DLI-IO-AREA                                     
174300                          SSA1                                            
174400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
174500                               STATUS-KUNDORDER-SEK-WS                    
174600     PERFORM IMS-STATUSKONTROLL                                           
174700     SKIP3                                                                
174800     .                                                                    
174900 IMS-GN-E4A1-SEK-KVAL SECTION.                                            
175000     STRING 'WDE401  (WDE4ASEQ =' W-E4A1-WDE4KEY-X ')'                    
175100            DELIMITED BY SIZE INTO SSA1                                   
175200     MOVE '  GE' TO GODK-STATUSKODER                                      
175300     CALL CBLTDLI USING GN                                                
175400                          WDE4A-PCB                                       
175500                          DLI-IO-AREA                                     
175600                          SSA1                                            
175700     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
175800                               STATUS-KUNDORDER-SEK-WS                    
175900     PERFORM IMS-STATUSKONTROLL                                           
176000     .                                                                    
176100     EJECT                                                                
176200 IMS-GU-E401-KVAL SECTION.                                                
176300     STRING 'WDE401  (WDE401KY =' W-E401-WDE4KEY-X ')'                    
176400            DELIMITED BY SIZE INTO SSA1                                   
176500     MOVE '    ' TO GODK-STATUSKODER                                      
176600     CALL CBLTDLI USING GU                                                
176700                          WDE4-PCB                                        
176800                          DLI-IO-AREA                                     
176900                          SSA1                                            
177000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
177100     PERFORM IMS-STATUSKONTROLL                                           
177200     SKIP3                                                                
177300     .                                                                    
177400 IMS-GU-E601 SECTION.                                                     
177500     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
177600            DELIMITED BY SIZE INTO SSA1                                   
177700     MOVE '  GE' TO GODK-STATUSKODER                                      
177800     CALL CBLTDLI USING GU                                                
177900                          WDE6-PCB                                        
178000                          DLI-IO-AREA                                     
178100                          SSA1                                            
178200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
178300     PERFORM IMS-STATUSKONTROLL                                           
178400     SKIP3                                                                
178500     .                                                                    
178600 IMS-GHU-E601-KVAL SECTION.                                               
178700     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
178800            DELIMITED BY SIZE INTO SSA1                                   
178900*    STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
179000*           DELIMITED BY SIZE INTO SSA2                                   
179100     MOVE '  GE' TO GODK-STATUSKODER                                      
179200     CALL CBLTDLI USING GHU                                               
179300                          WDE6-PCB                                        
179400                          DLI-IO-AREA                                     
179500                          SSA1                                            
179600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
179700     PERFORM IMS-STATUSKONTROLL                                           
179800     SKIP3                                                                
179900     .                                                                    
180000 IMS-REPLACE-E601 SECTION.                                                
180100     MOVE '  ' TO GODK-STATUSKODER                                        
180200     CALL CBLTDLI USING REPL                                              
180300                          WDE6-PCB                                        
180400                          DLI-IO-AREA                                     
180500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
180600     PERFORM IMS-STATUSKONTROLL                                           
180700     SKIP3                                                                
180800     .                                                                    
180900 IMS-GHU-E601-E611-KVAL SECTION.                                          
181000     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
181100            DELIMITED BY SIZE INTO SSA1                                   
181200     STRING 'WDE611  (IDKOLLI  =' W-E611-IDKOLLI-X ')'                    
181300            DELIMITED BY SIZE INTO SSA2                                   
181400     MOVE '  GE' TO GODK-STATUSKODER                                      
181500     CALL CBLTDLI USING GHU                                               
181600                          WDE6-PCB                                        
181700                          DLI-IO-AREA                                     
181800                          SSA1                                            
181900                          SSA2                                            
182000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     SKIP3                                                                
182300     .                                                                    
182400 IMS-REPLACE-E611 SECTION.                                                
182500     MOVE '  ' TO GODK-STATUSKODER                                        
182600     CALL CBLTDLI USING REPL                                              
182700                          WDE6-PCB                                        
182800                          DLI-IO-AREA                                     
182900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
183000     PERFORM IMS-STATUSKONTROLL                                           
183100     .                                                                    
183200     EJECT                                                                
183300 IMS-GU-4726-ROT-KVAL SECTION.                                            
183400     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
183500            DELIMITED BY SIZE INTO SSA1                                   
183600     MOVE '  ' TO GODK-STATUSKODER                                        
183700     CALL CBLTDLI USING GU                                                
183800                          XXDV-PCB                                        
183900                          DLI-IO-AREA                                     
184000                          SSA1                                            
184100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     SKIP3                                                                
184400     .                                                                    
184500 IMS-GNP-4726-UNDSEG-KVAL SECTION.                                        
184600     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
184700            DELIMITED BY SIZE INTO SSA1                                   
184800     MOVE '  GE' TO GODK-STATUSKODER                                      
184900     CALL CBLTDLI USING GNP                                               
185000                          XXDV-PCB                                        
185100                          DLI-IO-AREA                                     
185200                          SSA1                                            
185300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     EJECT                                                                
185700 IMS-INSERT-4726-UNDSEG SECTION.                                          
185800     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
185900            DELIMITED BY SIZE INTO SSA1                                   
186000     MOVE   'WLXXDV11 '         TO SSA2                                   
186100     MOVE '  ' TO GODK-STATUSKODER                                        
186200     CALL CBLTDLI USING ISRT                                              
186300                          XXDV-PCB                                        
186400                          DLI-IO-AREA                                     
186500                          SSA1                                            
186600                          SSA2                                            
186700     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     SKIP3                                                                
187000     .                                                                    
187100 IMS-INSERT-4727 SECTION.                                                 
187200     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
187300            DELIMITED BY SIZE INTO SSA1                                   
187400     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
187500            DELIMITED BY SIZE INTO SSA2                                   
187600     MOVE   'WLXXDV21 '         TO SSA3                                   
187700     MOVE '  II' TO GODK-STATUSKODER                                      
187800     CALL CBLTDLI USING ISRT                                              
187900                          XXDV-PCB                                        
188000                          DLI-IO-AREA                                     
188100                          SSA1                                            
188200                          SSA2                                            
188300                          SSA3                                            
188400     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
188500     PERFORM IMS-STATUSKONTROLL                                           
188600     .                                                                    
188700     EJECT                                                                
188800 IMS-INSERT-KLAR-SV4 SECTION.                                             
188900     MOVE 'WLZZAC01'      TO SSA1                                         
189000     MOVE '  II'          TO GODK-STATUSKODER                             
189100     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA2 SSA1                   
189200     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
189300     PERFORM IMS-STATUSKONTROLL                                           
189400     SKIP3                                                                
189500     .                                                                    
189600 IMS-ISRT-4322-SEGM SECTION.                                              
189700     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
189800        DELIMITED BY SIZE INTO SSA1                                       
189900     MOVE 'WLXXJK11*L' TO SSA2                                            
190000     MOVE '  ' TO GODK-STATUSKODER                                        
190100     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA2 SSA1 SSA2              
190200     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     EJECT                                                                
190600 IMS-GU-OGAG-VORD SECTION.                                                
190700                                                                          
190800     STRING 'WDE601  (IDPRODNR =' W-E601-IDPRODNR-X ')'                   
190900            DELIMITED BY SIZE INTO SSA1                                   
191000     MOVE '  GE' TO GODK-STATUSKODER                                      
191100     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA3 SSA1                    
191200     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
191300     PERFORM IMS-STATUSKONTROLL                                           
191400     .                                                                    
191500     SKIP3                                                                
191600 IMS-GN-WDE401-ESEQ SECTION.                                              
191700                                                                          
191800         STRING 'WDE401  (WDE4ESEQ =' W-WDE4E1KY-MAX-X ')'                
191900                DELIMITED BY SIZE INTO SSA1                               
192000         MOVE '  GE' TO GODK-STATUSKODER                                  
192100         CALL CBLTDLI USING GN WDE4E-PCB DLI-IO-AREA3 SSA1                
192200         MOVE WDE4E-STATUS-CODE TO STATUS-WS                              
192300         PERFORM IMS-STATUSKONTROLL                                       
192400         .                                                                
192500 IMS-GU-ORQA01    SECTION.                                                
192600                                                                          
192700     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-X ')'                        
192800            DELIMITED BY SIZE INTO SSA1                                   
192900     MOVE '  GE' TO GODK-STATUSKODER                                      
193000     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA3 SSA1                     
193100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     SKIP3                                                                
193500 IMS-GU-ORQA-ODEL SECTION.                                                
193600                                                                          
193700     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
193800                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
193900            DELIMITED BY SIZE INTO SSA1                                   
194000     MOVE '  GE' TO GODK-STATUSKODER                                      
194100     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA3 SSA1                     
194200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     SKIP3                                                                
194600 IMS-GN-ORQA-ODEL SECTION.                                                
194700                                                                          
194800     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
194900                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
195000            DELIMITED BY SIZE INTO SSA1                                   
195100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
195200     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA3 SSA1                     
195300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     .                                                                    
195600     EJECT                                                                
195700 IMS-GHU-WDQ212    SECTION.                                               
195800     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
195900            DELIMITED BY SIZE INTO SSA1                                   
196000     STRING 'WLORQI12(IDDC     =' W-WDQ212-X ')'                          
196100            DELIMITED BY SIZE INTO SSA2                                   
196200     MOVE '  GE' TO GODK-STATUSKODER                                      
196300     CALL CBLTDLI USING GHU   ORQI-PCB DLI-IO-WDQ212 SSA1 SSA2            
196400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700 IMS-REPL-WDQ212      SECTION.                                            
196800     MOVE '  ' TO GODK-STATUSKODER                                        
196900     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-WDQ212                       
197000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
197100     PERFORM IMS-STATUSKONTROLL                                           
197200     .                                                                    
197300     EJECT                                                                
197400 IMS-GU-XXKH11      SECTION.                                              
197500                                                                          
197600     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY-X ')'                    
197700            DELIMITED BY SIZE INTO SSA1                                   
197800     STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY-X ')'                    
197900            DELIMITED BY SIZE INTO SSA2                                   
198000     MOVE '  ' TO GODK-STATUSKODER                                        
198100     CALL CBLTDLI USING GU   XXKH-PCB DLI-IO-AREA  SSA1 SSA2              
198200     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
198300     PERFORM IMS-STATUSKONTROLL                                           
198400     .                                                                    
198500 IMS-GHU-WDGX4490    SECTION.                                             
198600                                                                          
198700     STRING 'WDR401  (WDGXKEY  =' W-4487-WDGXKEY-X ')'                    
198800            DELIMITED BY SIZE INTO SSA1                                   
198900     STRING 'WDGX4488(KDPRCGRP =' W-4488-WDGXKEY-X ')'                    
199000            DELIMITED BY SIZE INTO SSA2                                   
199100     STRING 'WDGX4490(KY4490   =' W-4490-WDGXKEY-X ')'                    
199200            DELIMITED BY SIZE INTO SSA3                                   
199300     MOVE '  GE' TO GODK-STATUSKODER                                      
199400     CALL CBLTDLI USING GHU  4487-PCB DLI-IO-AREA  SSA1 SSA2 SSA3         
199500     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800 IMS-DLET-WDGX4490    SECTION.                                            
199900                                                                          
200000     MOVE '  ' TO GODK-STATUSKODER                                        
200100     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-AREA                         
200200     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500 IMS-GU-XXDU01      SECTION.                                              
200600                                                                          
200700     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
200800            DELIMITED BY SIZE INTO SSA1                                   
200900     MOVE '  GE' TO GODK-STATUSKODER                                      
201000     CALL CBLTDLI USING GU XXDU-PCB DLI-IO-AREA5 SSA1                     
201100     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
201200     PERFORM IMS-STATUSKONTROLL                                           
201300     .                                                                    
201400 IMS-GHU-XXDU01      SECTION.                                             
201500                                                                          
201600     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
201700            DELIMITED BY SIZE INTO SSA1                                   
201800     MOVE '  GE' TO GODK-STATUSKODER                                      
201900     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA5 SSA1                    
202000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
202100     PERFORM IMS-STATUSKONTROLL                                           
202200     .                                                                    
202300 IMS-GU-XXDU01-XXDU2    SECTION.                                          
202400                                                                          
202500     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
202600            DELIMITED BY SIZE INTO SSA1                                   
202700     MOVE '  GE' TO GODK-STATUSKODER                                      
202800     CALL CBLTDLI USING GU XXDU2-PCB DLI-IO-AREA5 SSA1                    
202900     MOVE XXDU2-STATUS-CODE TO STATUS-WS                                  
203000     PERFORM IMS-STATUSKONTROLL                                           
203100     .                                                                    
203200 IMS-GHNP-XXDU11      SECTION.                                            
203300                                                                          
203400     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
203500            DELIMITED BY SIZE INTO SSA1                                   
203600     MOVE '  GE' TO GODK-STATUSKODER                                      
203700     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA5 SSA1                   
203800     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
203900     PERFORM IMS-STATUSKONTROLL                                           
204000     .                                                                    
204100 IMS-GNP-XXDU11    SECTION.                                               
204200                                                                          
204300     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
204400            DELIMITED BY SIZE INTO SSA1                                   
204500     MOVE '  GE' TO GODK-STATUSKODER                                      
204600     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA5 SSA1                    
204700     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
204800     PERFORM IMS-STATUSKONTROLL                                           
204900     .                                                                    
205000 IMS-GNP-XXDU11-IDKOLLI     SECTION.                                      
205100*    LÄS MED SAMMA IDKOLLI OCH NÄSTA PLOCKLISTA                           
205200*                                                                         
205300     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
205400            DELIMITED BY SIZE INTO SSA1                                   
205500     MOVE '  GE' TO GODK-STATUSKODER                                      
205600     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA5 SSA1                    
205700     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
205800     PERFORM IMS-STATUSKONTROLL                                           
205900     .                                                                    
206000 IMS-GNP-XXDU11-FIRST SECTION.                                            
206100                                                                          
206200     MOVE 'WLXXDU11*F '  TO SSA1                                          
206300     MOVE '  GE' TO GODK-STATUSKODER                                      
206400     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA5 SSA1                    
206500     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
206600     PERFORM IMS-STATUSKONTROLL                                           
206700     .                                                                    
206800 IMS-GNP-XXDU11-IDPLKLST-XXDU2   SECTION.                                 
206900*    LÄS NÄSTA 4302 MED SAMMA PLOCKLISTA                                  
207000*                                                                         
207100     STRING 'WLXXDU11(IDPLKLST =' W-4302-IDPLKLST-X ')'                   
207200            DELIMITED BY SIZE INTO SSA1                                   
207300     MOVE '  GE' TO GODK-STATUSKODER                                      
207400     CALL CBLTDLI USING GNP XXDU2-PCB DLI-IO-AREA5 SSA1                   
207500     MOVE XXDU2-STATUS-CODE TO STATUS-WS                                  
207600     PERFORM IMS-STATUSKONTROLL                                           
207700     .                                                                    
207800 IMS-DLET-XXDU       SECTION.                                             
207900                                                                          
208000     MOVE '  ' TO GODK-STATUSKODER                                        
208100     CALL CBLTDLI USING DLET XXDU-PCB DLI-IO-AREA5                        
208200     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
208300     PERFORM IMS-STATUSKONTROLL                                           
208400     .                                                                    
208500     EJECT                                                                
208600                                                                          
208700 IMS-GU-WDB601    SECTION.                                                
208800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
208900          DELIMITED BY SIZE INTO SSA1                                     
209000     MOVE '  GE' TO GODK-STATUSKODER                                      
209100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
209200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
209300     PERFORM IMS-STATUSKONTROLL                                           
209400     IF SEGMENT-SAKNAS                                                    
209500        MOVE SPACE TO DCS-KDDC                                            
209600     END-IF                                                               
209700     .                                                                    
209800 IMS-STATUSKONTROLL SECTION.                                              
209900     SET STATUS-IX TO 1                                                   
210000     SEARCH GODK-STATUS AT END CALL FELLOG                                
210100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
210200     END-SEARCH                                                           
210300     .                                                                    
