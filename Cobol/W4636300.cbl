000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4636300.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   00/11/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DIRECT BUSINESS.                                                 
000900*        DELAR UPP KONTROLLERADE OCH KORREKTA TRANSAR I                   
001000*        EN FIL PER VIPS/SC.                                              
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- KONTROLLERAD INFIL                                         
002600     SELECT W46367                     ASSIGN TO W46363D1.                
002700     SKIP2                                                                
002800*          --- TYSKLAND                                                   
002900     SELECT W4636A                     ASSIGN TO W46363DA.                
003000     SKIP2                                                                
003100*          --- ENGLAND                                                    
003200     SELECT W4636B                     ASSIGN TO W46363DB.                
003300     SKIP2                                                                
003400*          --- BELGIEN                                                    
003500     SELECT W4636C                     ASSIGN TO W46363DC.                
003600     SKIP2                                                                
003700*          --- ÖSTERIKE                                                   
003800     SELECT W4636D                     ASSIGN TO W46363DD.                
003900     SKIP2                                                                
004000*          --- DANMARK                                                    
004100     SELECT W4636E                     ASSIGN TO W46363DE.                
004200     SKIP2                                                                
004300*          --- FINNLAND                                                   
004400     SELECT W4636F                     ASSIGN TO W46363DF.                
004500     SKIP2                                                                
004600*          --- FRANKRIKE                                                  
004700     SELECT W4636G                     ASSIGN TO W46363DG.                
004800     SKIP2                                                                
004900*          --- GREKLAND                                                   
005000     SELECT W4636H                     ASSIGN TO W46363DH.                
005100     SKIP2                                                                
005200*          --- IRLAND                                                     
005300     SELECT W4636I                     ASSIGN TO W46363DI.                
005400     SKIP2                                                                
005500*          --- ITALIEN                                                    
005600     SELECT W4636J                     ASSIGN TO W46363DJ.                
005700     SKIP2                                                                
005800*          --- HOLLAND                                                    
005900     SELECT W4636K                     ASSIGN TO W46363DK.                
006000     SKIP2                                                                
006100*          --- NORGE                                                      
006200     SELECT W4636L                     ASSIGN TO W46363DL.                
006300     SKIP2                                                                
006400*          --- PORTUGAL                                                   
006500     SELECT W4636M                     ASSIGN TO W46363DM.                
006600     SKIP2                                                                
006700*          --- SPANIEN                                                    
006800     SELECT W4636N                     ASSIGN TO W46363DN.                
006900     SKIP2                                                                
007000*          --- SVERIGE                                                    
007100     SELECT W4636O                     ASSIGN TO W46363DO.                
007200     SKIP2                                                                
007300*          --- SCHWEIZ                                                    
007400     SELECT W4636P                     ASSIGN TO W46363DP.                
007500*                                                                         
007600*          --- JAPAN                                                      
007700     SELECT W4636Q                     ASSIGN TO W46363DQ.                
007800     SKIP2                                                                
007900*          --- POLEN                                                      
008000     SELECT W4636R                     ASSIGN TO W46363DR.                
008100     SKIP2                                                                
008110*          --- TURKIET                                                    
008120     SELECT W4636S                     ASSIGN TO W46363DS.                
008130     SKIP2                                                                
008140*          --- MEXICO                                                     
008150     SELECT W4636U                     ASSIGN TO W46363DU.                
008160     SKIP2                                                                
008191*          --- BRASIL                                                     
008192     SELECT W4636V                     ASSIGN TO W46363DV.                
008193     SKIP2                                                                
008194*          --- SOUTH AFRICA                                               
008195     SELECT W4636W                     ASSIGN TO W46363DW.                
008196     SKIP2                                                                
008200*          --- TYSKLAND, PRISFIL                                          
008300     SELECT W463PA                     ASSIGN TO W46363PA.                
008400     SKIP2                                                                
008500*          --- ENGLAND, PRISFIL                                           
008600     SELECT W463PB                     ASSIGN TO W46363PB.                
008700     SKIP2                                                                
008800*          --- BELGIEN, PRISFIL                                           
008900     SELECT W463PC                     ASSIGN TO W46363PC.                
009000     SKIP2                                                                
009100*          --- ÖSTERIKE, PRISFIL                                          
009200     SELECT W463PD                     ASSIGN TO W46363PD.                
009300     SKIP2                                                                
009400*          --- DANMARK, PRISFIL                                           
009500     SELECT W463PE                     ASSIGN TO W46363PE.                
009600     SKIP2                                                                
009700*          --- FINNLAND, PRISFIL                                          
009800     SELECT W463PF                     ASSIGN TO W46363PF.                
009900     SKIP2                                                                
010000*          --- FRANKRIKE, PRISFIL                                         
010100     SELECT W463PG                     ASSIGN TO W46363PG.                
010200     SKIP2                                                                
010300*          --- GREKLAND, PRISFIL                                          
010400     SELECT W463PH                     ASSIGN TO W46363PH.                
010500     SKIP2                                                                
010600*          --- IRLAND, PRISFIL                                            
010700     SELECT W463PI                     ASSIGN TO W46363PI.                
010800     SKIP2                                                                
010900*          --- ITALIEN, PRISFIL                                           
011000     SELECT W463PJ                     ASSIGN TO W46363PJ.                
011100     SKIP2                                                                
011200*          --- HOLLAND, PRISFIL                                           
011300     SELECT W463PK                     ASSIGN TO W46363PK.                
011400     SKIP2                                                                
011500*          --- NORGE, PRISFIL                                             
011600     SELECT W463PL                     ASSIGN TO W46363PL.                
011700     SKIP2                                                                
011800*          --- PORTUGAL, PRISFIL                                          
011900     SELECT W463PM                     ASSIGN TO W46363PM.                
012000     SKIP2                                                                
012100*          --- SPANIEN, PRISFIL                                           
012200     SELECT W463PN                     ASSIGN TO W46363PN.                
012300     SKIP2                                                                
012400*          --- SVERIGE, PRISFIL                                           
012500     SELECT W463PO                     ASSIGN TO W46363PO.                
012600     SKIP2                                                                
012700*          --- SCHWEIZ, PRISFIL                                           
012800     SELECT W463PP                     ASSIGN TO W46363PP.                
012900*                                                                         
013000*          --- JAPAN,   PRISFIL                                           
013100     SELECT W463PQ                     ASSIGN TO W46363PQ.                
013200*                                                                         
013300*          --- POLEN,   PRISFIL                                           
013400     SELECT W463PR                     ASSIGN TO W46363PR.                
013410*                                                                         
013420*          --- TURKIET, PRISFIL                                           
013430     SELECT W463PS                     ASSIGN TO W46363PS.                
013500*                                                                         
013510*          --- TJECKIEN, PRISFIL                                          
013520     SELECT W463PT                     ASSIGN TO W46363PT.                
013530*                                                                         
013540*          --- MEXICO, PRISFIL                                            
013550     SELECT W463PU                     ASSIGN TO W46363PU.                
013560*                                                                         
013570*          --- BRASIL, PRISFIL                                            
013580     SELECT W463PV                     ASSIGN TO W46363PV.                
013590*                                                                         
013591*          --- S.AFRICA,PRISFIL                                           
013592     SELECT W463PW                     ASSIGN TO W46363PW.                
013593*                                                                         
013600 DATA DIVISION.                                                           
013700     SKIP2                                                                
013800 FILE SECTION.                                                            
013900     SKIP3                                                                
014000 FD  W46367                                                               
014100     RECORDING       F                                                    
014200     BLOCK CONTAINS  0.                                                   
014300                                                                          
014400 01  IN-POST           PIC X(220).                                        
014500     SKIP3                                                                
014600 FD  W4636A                                                               
014700     RECORDING       F                                                    
014800     BLOCK CONTAINS  0.                                                   
014900                                                                          
015000 01  UT-A-POST         PIC X(190).                                        
015100     SKIP3                                                                
015200 FD  W4636B                                                               
015300     RECORDING       F                                                    
015400     BLOCK CONTAINS  0.                                                   
015500                                                                          
015600 01  UT-B-POST         PIC X(190).                                        
015700     SKIP3                                                                
015800 FD  W4636C                                                               
015900     RECORDING       F                                                    
016000     BLOCK CONTAINS  0.                                                   
016100                                                                          
016200 01  UT-C-POST         PIC X(190).                                        
016300     SKIP3                                                                
016400 FD  W4636D                                                               
016500     RECORDING       F                                                    
016600     BLOCK CONTAINS  0.                                                   
016700                                                                          
016800 01  UT-D-POST         PIC X(190).                                        
016900     SKIP3                                                                
017000 FD  W4636E                                                               
017100     RECORDING       F                                                    
017200     BLOCK CONTAINS  0.                                                   
017300                                                                          
017400 01  UT-E-POST         PIC X(190).                                        
017500     SKIP3                                                                
017600 FD  W4636F                                                               
017700     RECORDING       F                                                    
017800     BLOCK CONTAINS  0.                                                   
017900                                                                          
018000 01  UT-F-POST         PIC X(190).                                        
018100     SKIP3                                                                
018200 FD  W4636G                                                               
018300     RECORDING       F                                                    
018400     BLOCK CONTAINS  0.                                                   
018500                                                                          
018600 01  UT-G-POST         PIC X(190).                                        
018700     SKIP3                                                                
018800 FD  W4636H                                                               
018900     RECORDING       F                                                    
019000     BLOCK CONTAINS  0.                                                   
019100                                                                          
019200 01  UT-H-POST         PIC X(190).                                        
019300     SKIP3                                                                
019400 FD  W4636I                                                               
019500     RECORDING       F                                                    
019600     BLOCK CONTAINS  0.                                                   
019700                                                                          
019800 01  UT-I-POST         PIC X(190).                                        
019900     SKIP3                                                                
020000 FD  W4636J                                                               
020100     RECORDING       F                                                    
020200     BLOCK CONTAINS  0.                                                   
020300                                                                          
020400 01  UT-J-POST         PIC X(190).                                        
020500     SKIP3                                                                
020600 FD  W4636K                                                               
020700     RECORDING       F                                                    
020800     BLOCK CONTAINS  0.                                                   
020900                                                                          
021000 01  UT-K-POST         PIC X(190).                                        
021100     SKIP3                                                                
021200 FD  W4636L                                                               
021300     RECORDING       F                                                    
021400     BLOCK CONTAINS  0.                                                   
021500                                                                          
021600 01  UT-L-POST         PIC X(190).                                        
021700     SKIP3                                                                
021800 FD  W4636M                                                               
021900     RECORDING       F                                                    
022000     BLOCK CONTAINS  0.                                                   
022100                                                                          
022200 01  UT-M-POST         PIC X(190).                                        
022300     SKIP3                                                                
022400 FD  W4636N                                                               
022500     RECORDING       F                                                    
022600     BLOCK CONTAINS  0.                                                   
022700                                                                          
022800 01  UT-N-POST         PIC X(190).                                        
022900     SKIP3                                                                
023000 FD  W4636O                                                               
023100     RECORDING       F                                                    
023200     BLOCK CONTAINS  0.                                                   
023300                                                                          
023400 01  UT-O-POST         PIC X(190).                                        
023500     SKIP3                                                                
023600 FD  W4636P                                                               
023700     RECORDING       F                                                    
023800     BLOCK CONTAINS  0.                                                   
023900                                                                          
024000 01  UT-P-POST         PIC X(190).                                        
024100     SKIP3                                                                
024200 FD  W4636Q                                                               
024300     RECORDING       F                                                    
024400     BLOCK CONTAINS  0.                                                   
024500                                                                          
024600 01  UT-Q-POST         PIC X(190).                                        
024700     SKIP3                                                                
024800 FD  W4636R                                                               
024900     RECORDING       F                                                    
025000     BLOCK CONTAINS  0.                                                   
025100                                                                          
025200 01  UT-R-POST         PIC X(190).                                        
025210     SKIP3                                                                
025220 FD  W4636S                                                               
025230     RECORDING       F                                                    
025240     BLOCK CONTAINS  0.                                                   
025250                                                                          
025260 01  UT-S-POST         PIC X(190).                                        
025300     SKIP3                                                                
025370 FD  W4636U                                                               
025380     RECORDING       F                                                    
025390     BLOCK CONTAINS  0.                                                   
025391                                                                          
025392 01  UT-U-POST         PIC X(190).                                        
025393     SKIP3                                                                
025394 FD  W4636V                                                               
025395     RECORDING       F                                                    
025396     BLOCK CONTAINS  0.                                                   
025397                                                                          
025398 01  UT-V-POST         PIC X(190).                                        
025399     SKIP3                                                                
025400 FD  W4636W                                                               
025500     RECORDING       F                                                    
025600     BLOCK CONTAINS  0.                                                   
025700                                                                          
025710 01  UT-W-POST         PIC X(190).                                        
025720     SKIP3                                                                
025730 FD  W463PA                                                               
025740     RECORDING       F                                                    
025750     BLOCK CONTAINS  0.                                                   
025760                                                                          
025800 01  UT-PA-POST        PIC X(190).                                        
025900     SKIP3                                                                
026000 FD  W463PB                                                               
026100     RECORDING       F                                                    
026200     BLOCK CONTAINS  0.                                                   
026300                                                                          
026400 01  UT-PB-POST        PIC X(190).                                        
026500     SKIP3                                                                
026600 FD  W463PC                                                               
026700     RECORDING       F                                                    
026800     BLOCK CONTAINS  0.                                                   
026900                                                                          
027000 01  UT-PC-POST        PIC X(190).                                        
027100     SKIP3                                                                
027200 FD  W463PD                                                               
027300     RECORDING       F                                                    
027400     BLOCK CONTAINS  0.                                                   
027500                                                                          
027600 01  UT-PD-POST        PIC X(190).                                        
027700     SKIP3                                                                
027800 FD  W463PE                                                               
027900     RECORDING       F                                                    
028000     BLOCK CONTAINS  0.                                                   
028100                                                                          
028200 01  UT-PE-POST        PIC X(190).                                        
028300     SKIP3                                                                
028400 FD  W463PF                                                               
028500     RECORDING       F                                                    
028600     BLOCK CONTAINS  0.                                                   
028700                                                                          
028800 01  UT-PF-POST        PIC X(190).                                        
028900     SKIP3                                                                
029000 FD  W463PG                                                               
029100     RECORDING       F                                                    
029200     BLOCK CONTAINS  0.                                                   
029300                                                                          
029400 01  UT-PG-POST        PIC X(190).                                        
029500     SKIP3                                                                
029600 FD  W463PH                                                               
029700     RECORDING       F                                                    
029800     BLOCK CONTAINS  0.                                                   
029900                                                                          
030000 01  UT-PH-POST        PIC X(190).                                        
030100     SKIP3                                                                
030200 FD  W463PI                                                               
030300     RECORDING       F                                                    
030400     BLOCK CONTAINS  0.                                                   
030500                                                                          
030600 01  UT-PI-POST        PIC X(190).                                        
030700     SKIP3                                                                
030800 FD  W463PJ                                                               
030900     RECORDING       F                                                    
031000     BLOCK CONTAINS  0.                                                   
031100                                                                          
031200 01  UT-PJ-POST        PIC X(190).                                        
031300     SKIP3                                                                
031400 FD  W463PK                                                               
031500     RECORDING       F                                                    
031600     BLOCK CONTAINS  0.                                                   
031700                                                                          
031800 01  UT-PK-POST        PIC X(190).                                        
031900     SKIP3                                                                
032000 FD  W463PL                                                               
032100     RECORDING       F                                                    
032200     BLOCK CONTAINS  0.                                                   
032300                                                                          
032400 01  UT-PL-POST        PIC X(190).                                        
032500     SKIP3                                                                
032600 FD  W463PM                                                               
032700     RECORDING       F                                                    
032800     BLOCK CONTAINS  0.                                                   
032900                                                                          
033000 01  UT-PM-POST         PIC X(190).                                       
033100     SKIP3                                                                
033200 FD  W463PN                                                               
033300     RECORDING       F                                                    
033400     BLOCK CONTAINS  0.                                                   
033500                                                                          
033600 01  UT-PN-POST        PIC X(190).                                        
033700     SKIP3                                                                
033800 FD  W463PO                                                               
033900     RECORDING       F                                                    
034000     BLOCK CONTAINS  0.                                                   
034100                                                                          
034200 01  UT-PO-POST        PIC X(190).                                        
034300     SKIP3                                                                
034400 FD  W463PP                                                               
034500     RECORDING       F                                                    
034600     BLOCK CONTAINS  0.                                                   
034700                                                                          
034800 01  UT-PP-POST        PIC X(190).                                        
034900     SKIP3                                                                
035000 FD  W463PQ                                                               
035100     RECORDING       F                                                    
035200     BLOCK CONTAINS  0.                                                   
035300                                                                          
035400 01  UT-PQ-POST        PIC X(190).                                        
035500                                                                          
035600 FD  W463PR                                                               
035700     RECORDING       F                                                    
035800     BLOCK CONTAINS  0.                                                   
035900                                                                          
036000 01  UT-PR-POST        PIC X(190).                                        
036010                                                                          
036020 FD  W463PS                                                               
036030     RECORDING       F                                                    
036040     BLOCK CONTAINS  0.                                                   
036050                                                                          
036060 01  UT-PS-POST        PIC X(190).                                        
036100                                                                          
036110 FD  W463PT                                                               
036120     RECORDING       F                                                    
036130     BLOCK CONTAINS  0.                                                   
036140                                                                          
036150 01  UT-PT-POST        PIC X(190).                                        
036154                                                                          
036155 FD  W463PU                                                               
036156     RECORDING       F                                                    
036157     BLOCK CONTAINS  0.                                                   
036158                                                                          
036159 01  UT-PU-POST        PIC X(190).                                        
036160                                                                          
036161 FD  W463PV                                                               
036162     RECORDING       F                                                    
036163     BLOCK CONTAINS  0.                                                   
036164                                                                          
036165 01  UT-PV-POST        PIC X(190).                                        
036166                                                                          
036167 FD  W463PW                                                               
036168     RECORDING       F                                                    
036169     BLOCK CONTAINS  0.                                                   
036170                                                                          
036171 01  UT-PW-POST        PIC X(190).                                        
036172                                                                          
036180     EJECT                                                                
036200 WORKING-STORAGE SECTION.                                                 
036300                                                                          
036400                                                                          
036500*    -- CHECKED BY WY2000                                                 
036600 77  IDPGM                       PIC X(8)    VALUE 'W4636300'.            
036700 77  JA                          PIC X       VALUE 'J'.                   
036800 77  NEJ                         PIC X       VALUE 'N'.                   
036900                                                                          
037000 77  W46367-EOF-SW               PIC X       VALUE 'N'.                   
037100     88  END-OF-W46367                       VALUE 'J'.                   
037200                                                                          
037300     SKIP2                                                                
037400 01  DYNAMISKA-SUBPROGRAM.                                                
037500*                                                                         
037600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
037700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
037800     SKIP2                                                                
037900*    --- PARAMETRAR TILL ABEND                                            
038000                                                                          
038100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
038200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
038300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
038400     SKIP2                                                                
038500 01  FELTEXT.                                                             
038600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
038700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
038800     EJECT                                                                
038900*    --- PARAMETRAR TILL POSTSUM                                          
039000*                                                                         
039100*01  -COPY W0005   -PRE  POSTSUM-                                         
039200     EJECT                                                                
039300 01  IN-AREA-START               PIC X(24)   VALUE                        
039400                                 'IN-AREA-START  '.                       
039500     SKIP2                                                                
039600                                                                          
039700 01  IN-AREA.                                                             
039800     03 IN-S-DEL                 PIC X(30).                               
039900     03 IN-URSP-POST             PIC X(160).                              
040000     03 IN-PRODTYP               PIC X(1).                                
040100     03 IN-BONUSBASE             PIC 9(11).                               
040200     03 IN-KDPRODSL              PIC 9(2).                                
040300     03 IN-IDFKNGRP              PIC 9(4).                                
040400     03 IN-IDPARTNR              PIC 9(7).                                
040500     03 IN-BASEDISC              PIC X(5).                                
040600     EJECT                                                                
040700 01  UTOK-AREA-START             PIC X(24)   VALUE                        
040800                                 'UTOK-AREA-START  '.                     
040900     SKIP2                                                                
041000                                                                          
041100 01  UT-AREA.                                                             
041200     03 UT-URSP-POST             PIC X(160).                              
041300*    03 -COPY WINVBBB0       -RED UT-URSP-POST                            
041400     03 UT-PRODTYP               PIC X(1).                                
041500     03 UT-BONUSBASE             PIC 9(11).                               
041600     03 UT-KDPRODSL              PIC 9(2).                                
041700     03 UT-IDFKNGRP              PIC 9(4).                                
041800     03 UT-IDPARTNR              PIC 9(7).                                
041900     03 UT-BASEDISC              PIC X(5).                                
042000     EJECT                                                                
042100 PROCEDURE DIVISION.                                                      
042200 MAIN SECTION.                                                            
042300                                                                          
042400     PERFORM A-INIT                                                       
042500                                                                          
042600     PERFORM S01-LAES-W46367                                              
042700                                                                          
042800     IF NOT END-OF-W46367                                                 
042900       PERFORM UNTIL END-OF-W46367                                        
043000         MOVE IN-URSP-POST  TO UT-URSP-POST                               
043100         MOVE IN-PRODTYP    TO UT-PRODTYP                                 
043200         MOVE IN-BONUSBASE  TO UT-BONUSBASE                               
043300         MOVE IN-KDPRODSL   TO UT-KDPRODSL                                
043400         MOVE IN-IDFKNGRP   TO UT-IDFKNGRP                                
043500         MOVE IN-IDPARTNR   TO UT-IDPARTNR                                
043600         MOVE IN-BASEDISC   TO UT-BASEDISC                                
043700                                                                          
043800         IF B0-RECORDTYPE NOT = 'P'                                       
043900           EVALUATE B0-BILL-LOC                                           
044000             WHEN 'GER' PERFORM S1A-SKRIV-W4636A                          
044100             WHEN 'BRI' PERFORM S1B-SKRIV-W4636B                          
044200             WHEN 'BEL' PERFORM S1C-SKRIV-W4636C                          
044300             WHEN 'AUS' PERFORM S1D-SKRIV-W4636D                          
044400             WHEN 'DEN' PERFORM S1E-SKRIV-W4636E                          
044500             WHEN 'FIN' PERFORM S1F-SKRIV-W4636F                          
044600             WHEN 'FRA' PERFORM S1G-SKRIV-W4636G                          
044700             WHEN 'GRE' PERFORM S1H-SKRIV-W4636H                          
044800             WHEN 'IRE' PERFORM S1I-SKRIV-W4636I                          
044900             WHEN 'ITA' PERFORM S1J-SKRIV-W4636J                          
045000             WHEN 'NET' PERFORM S1K-SKRIV-W4636K                          
045100             WHEN 'NOR' PERFORM S1L-SKRIV-W4636L                          
045200             WHEN 'POR' PERFORM S1M-SKRIV-W4636M                          
045300             WHEN 'SPA' PERFORM S1N-SKRIV-W4636N                          
045400             WHEN 'SWE' PERFORM S1O-SKRIV-W4636O                          
045500             WHEN 'SWI' PERFORM S1P-SKRIV-W4636P                          
045600             WHEN 'JPA' PERFORM S1Q-SKRIV-W4636Q                          
045700             WHEN 'POL' PERFORM S1R-SKRIV-W4636R                          
045710             WHEN 'TUR' PERFORM S1S-SKRIV-W4636S                          
045720             WHEN 'CZE' PERFORM S1T-SKRIV-W4636T                          
045730             WHEN 'MEX' PERFORM S1U-SKRIV-W4636U                          
045740             WHEN 'BRA' PERFORM S1V-SKRIV-W4636V                          
045750             WHEN 'ZAF' PERFORM S1W-SKRIV-W4636W                          
045800             WHEN OTHER DISPLAY 'FIL MED BILLINGTRANSAR'                  
045900                        DISPLAY 'FÖR ' B0-BILL-LOC                        
046000                                ' KAN EJ SKRIVAS '                        
046100                        PERFORM S99-ABEND                                 
046200           END-EVALUATE                                                   
046300         ELSE                                                             
046400           EVALUATE B0-BILL-LOC                                           
046500             WHEN 'GER' PERFORM S1PA-SKRIV-W463PA                         
046600             WHEN 'BRI' PERFORM S1PB-SKRIV-W463PB                         
046700             WHEN 'BEL' PERFORM S1PC-SKRIV-W463PC                         
046800             WHEN 'AUS' PERFORM S1PD-SKRIV-W463PD                         
046900             WHEN 'DEN' PERFORM S1PE-SKRIV-W463PE                         
047000             WHEN 'FIN' PERFORM S1PF-SKRIV-W463PF                         
047100             WHEN 'FRA' PERFORM S1PG-SKRIV-W463PG                         
047200             WHEN 'GRE' PERFORM S1PH-SKRIV-W463PH                         
047300             WHEN 'IRE' PERFORM S1PI-SKRIV-W463PI                         
047400             WHEN 'ITA' PERFORM S1PJ-SKRIV-W463PJ                         
047500             WHEN 'NET' PERFORM S1PK-SKRIV-W463PK                         
047600             WHEN 'NOR' PERFORM S1PL-SKRIV-W463PL                         
047700             WHEN 'POR' PERFORM S1PM-SKRIV-W463PM                         
047800             WHEN 'SPA' PERFORM S1PN-SKRIV-W463PN                         
047900             WHEN 'SWE' PERFORM S1PO-SKRIV-W463PO                         
048000             WHEN 'SWI' PERFORM S1PP-SKRIV-W463PP                         
048100             WHEN 'JPA' PERFORM S1PQ-SKRIV-W463PQ                         
048200             WHEN 'POL' PERFORM S1PR-SKRIV-W463PR                         
048210             WHEN 'TUR' PERFORM S1PS-SKRIV-W463PS                         
048220             WHEN 'CZE' PERFORM S1PT-SKRIV-W463PT                         
048230             WHEN 'MEX' PERFORM S1PU-SKRIV-W463PU                         
048240             WHEN 'BRA' PERFORM S1PV-SKRIV-W463PV                         
048250             WHEN 'ZAF' PERFORM S1PW-SKRIV-W463PW                         
048300             WHEN OTHER DISPLAY 'FIL MED PRICETRANSAR'                    
048400                        DISPLAY 'FÖR ' B0-BILL-LOC                        
048500                                ' KAN EJ SKRIVAS '                        
048600                        PERFORM S99-ABEND                                 
048700           END-EVALUATE                                                   
048800         END-IF                                                           
048900                                                                          
049000         PERFORM S01-LAES-W46367                                          
049100       END-PERFORM                                                        
049200     END-IF                                                               
049300                                                                          
049400     PERFORM Z-FINIT                                                      
049500                                                                          
049600     MOVE ZERO TO RETURN-CODE                                             
049700     GOBACK                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 A-INIT SECTION.                                                          
050100                                                                          
050200     OPEN INPUT  W46367                                                   
050300                                                                          
050400     OPEN OUTPUT W4636A                                                   
050500                 W4636B                                                   
050600                 W4636C                                                   
050700                 W4636D                                                   
050800                 W4636E                                                   
050900                 W4636F                                                   
051000                 W4636G                                                   
051100                 W4636H                                                   
051200                 W4636I                                                   
051300                 W4636J                                                   
051400                 W4636K                                                   
051500                 W4636L                                                   
051600                 W4636M                                                   
051700                 W4636N                                                   
051800                 W4636O                                                   
051900                 W4636P                                                   
052000                 W4636Q                                                   
052100                 W4636R                                                   
052110                 W4636S                                                   
052130                 W4636U                                                   
052140                 W4636V                                                   
052150                 W4636W                                                   
052200                 W463PA                                                   
052300                 W463PB                                                   
052400                 W463PC                                                   
052500                 W463PD                                                   
052600                 W463PE                                                   
052700                 W463PF                                                   
052800                 W463PG                                                   
052900                 W463PH                                                   
053000                 W463PI                                                   
053100                 W463PJ                                                   
053200                 W463PK                                                   
053300                 W463PL                                                   
053400                 W463PM                                                   
053500                 W463PN                                                   
053600                 W463PO                                                   
053700                 W463PP                                                   
053800                 W463PQ                                                   
053900                 W463PR                                                   
053910                 W463PS                                                   
053920                 W463PT                                                   
053930                 W463PU                                                   
053940                 W463PV                                                   
053950                 W463PW                                                   
054000                                                                          
054100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
054200     .                                                                    
054300     EJECT                                                                
054400 Z-FINIT SECTION.                                                         
054500     CLOSE W46367                                                         
054600           W4636A                                                         
054700           W4636B                                                         
054800           W4636C                                                         
054900           W4636D                                                         
055000           W4636E                                                         
055100           W4636F                                                         
055200           W4636G                                                         
055300           W4636H                                                         
055400           W4636I                                                         
055500           W4636J                                                         
055600           W4636K                                                         
055700           W4636L                                                         
055800           W4636M                                                         
055900           W4636N                                                         
056000           W4636O                                                         
056100           W4636P                                                         
056200           W4636Q                                                         
056300           W4636R                                                         
056310           W4636S                                                         
056330           W4636U                                                         
056340           W4636V                                                         
056350           W4636W                                                         
056400           W463PA                                                         
056500           W463PB                                                         
056600           W463PC                                                         
056700           W463PD                                                         
056800           W463PE                                                         
056900           W463PF                                                         
057000           W463PG                                                         
057100           W463PH                                                         
057200           W463PI                                                         
057300           W463PJ                                                         
057400           W463PK                                                         
057500           W463PL                                                         
057600           W463PM                                                         
057700           W463PN                                                         
057800           W463PO                                                         
057900           W463PP                                                         
058000           W463PQ                                                         
058100           W463PR                                                         
058110           W463PS                                                         
058120           W463PT                                                         
058130           W463PU                                                         
058140           W463PV                                                         
058150           W463PW                                                         
058200     SKIP2                                                                
058300     MOVE 'S' TO POSTSUM-OPKOD                                            
058400     CALL POSTSUM USING POSTSUM-PARM                                      
058500     .                                                                    
058600     EJECT                                                                
058700 S01-LAES-W46367  SECTION.                                                
058800                                                                          
058900     READ W46367 INTO IN-AREA                                             
059000     AT END                                                               
059100        MOVE HIGH-VALUE TO IN-AREA                                        
059200        SET END-OF-W46367 TO TRUE                                         
059300                                                                          
059400     NOT AT END                                                           
059500        MOVE 'W46367' TO POSTSUM-FDNAMN                                   
059600        MOVE 'W46363D1' TO POSTSUM-DDNAMN2                                
059700        MOVE SPACE TO POSTSUM-TRANSTYP                                    
059800        CALL POSTSUM USING POSTSUM-PARM                                   
059900     END-READ                                                             
060000     .                                                                    
060100     EJECT                                                                
060200 S1A-SKRIV-W4636A SECTION.                                                
060300                                                                          
060400     WRITE UT-A-POST FROM UT-AREA                                         
060500                                                                          
060600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
060700     MOVE 'W4636A' TO POSTSUM-FDNAMN                                      
060800     MOVE 'W46363DA' TO POSTSUM-DDNAMN2                                   
060900     CALL POSTSUM USING POSTSUM-PARM                                      
061000     .                                                                    
061100     EJECT                                                                
061200 S1B-SKRIV-W4636B SECTION.                                                
061300                                                                          
061400     WRITE UT-B-POST FROM UT-AREA                                         
061500                                                                          
061600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
061700     MOVE 'W4636B' TO POSTSUM-FDNAMN                                      
061800     MOVE 'W46363DB' TO POSTSUM-DDNAMN2                                   
061900     CALL POSTSUM USING POSTSUM-PARM                                      
062000     .                                                                    
062100     EJECT                                                                
062200 S1C-SKRIV-W4636C SECTION.                                                
062300                                                                          
062400     WRITE UT-C-POST FROM UT-AREA                                         
062500                                                                          
062600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
062700     MOVE 'W4636C' TO POSTSUM-FDNAMN                                      
062800     MOVE 'W46363DC' TO POSTSUM-DDNAMN2                                   
062900     CALL POSTSUM USING POSTSUM-PARM                                      
063000     .                                                                    
063100     EJECT                                                                
063200 S1D-SKRIV-W4636D SECTION.                                                
063300                                                                          
063400     WRITE UT-D-POST FROM UT-AREA                                         
063500                                                                          
063600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
063700     MOVE 'W4636D' TO POSTSUM-FDNAMN                                      
063800     MOVE 'W46363DD' TO POSTSUM-DDNAMN2                                   
063900     CALL POSTSUM USING POSTSUM-PARM                                      
064000     .                                                                    
064100     EJECT                                                                
064200 S1E-SKRIV-W4636E SECTION.                                                
064300                                                                          
064400     WRITE UT-E-POST FROM UT-AREA                                         
064500                                                                          
064600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
064700     MOVE 'W4636E' TO POSTSUM-FDNAMN                                      
064800     MOVE 'W46363DE' TO POSTSUM-DDNAMN2                                   
064900     CALL POSTSUM USING POSTSUM-PARM                                      
065000     .                                                                    
065100     EJECT                                                                
065200 S1F-SKRIV-W4636F SECTION.                                                
065300                                                                          
065400     WRITE UT-F-POST FROM UT-AREA                                         
065500                                                                          
065600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
065700     MOVE 'W4636F' TO POSTSUM-FDNAMN                                      
065800     MOVE 'W46363DF' TO POSTSUM-DDNAMN2                                   
065900     CALL POSTSUM USING POSTSUM-PARM                                      
066000     .                                                                    
066100     EJECT                                                                
066200 S1G-SKRIV-W4636G SECTION.                                                
066300                                                                          
066400     WRITE UT-G-POST FROM UT-AREA                                         
066500                                                                          
066600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
066700     MOVE 'W4636G' TO POSTSUM-FDNAMN                                      
066800     MOVE 'W46363DG' TO POSTSUM-DDNAMN2                                   
066900     CALL POSTSUM USING POSTSUM-PARM                                      
067000     .                                                                    
067100     EJECT                                                                
067200 S1H-SKRIV-W4636H SECTION.                                                
067300                                                                          
067400     WRITE UT-H-POST FROM UT-AREA                                         
067500                                                                          
067600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
067700     MOVE 'W4636H' TO POSTSUM-FDNAMN                                      
067800     MOVE 'W46363DH' TO POSTSUM-DDNAMN2                                   
067900     CALL POSTSUM USING POSTSUM-PARM                                      
068000     .                                                                    
068100     EJECT                                                                
068200 S1I-SKRIV-W4636I SECTION.                                                
068300                                                                          
068400     WRITE UT-I-POST FROM UT-AREA                                         
068500                                                                          
068600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
068700     MOVE 'W4636I' TO POSTSUM-FDNAMN                                      
068800     MOVE 'W46363DI' TO POSTSUM-DDNAMN2                                   
068900     CALL POSTSUM USING POSTSUM-PARM                                      
069000     .                                                                    
069100     EJECT                                                                
069200 S1J-SKRIV-W4636J SECTION.                                                
069300                                                                          
069400     WRITE UT-J-POST FROM UT-AREA                                         
069500                                                                          
069600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
069700     MOVE 'W4636J' TO POSTSUM-FDNAMN                                      
069800     MOVE 'W46363DJ' TO POSTSUM-DDNAMN2                                   
069900     CALL POSTSUM USING POSTSUM-PARM                                      
070000     .                                                                    
070100     EJECT                                                                
070200 S1K-SKRIV-W4636K SECTION.                                                
070300                                                                          
070400     WRITE UT-K-POST FROM UT-AREA                                         
070500                                                                          
070600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
070700     MOVE 'W4636K' TO POSTSUM-FDNAMN                                      
070800     MOVE 'W46363DK' TO POSTSUM-DDNAMN2                                   
070900     CALL POSTSUM USING POSTSUM-PARM                                      
071000     .                                                                    
071100     EJECT                                                                
071200 S1L-SKRIV-W4636L SECTION.                                                
071300                                                                          
071400     WRITE UT-L-POST FROM UT-AREA                                         
071500                                                                          
071600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
071700     MOVE 'W4636L' TO POSTSUM-FDNAMN                                      
071800     MOVE 'W46363DL' TO POSTSUM-DDNAMN2                                   
071900     CALL POSTSUM USING POSTSUM-PARM                                      
072000     .                                                                    
072100     EJECT                                                                
072200 S1M-SKRIV-W4636M SECTION.                                                
072300                                                                          
072400     WRITE UT-M-POST FROM UT-AREA                                         
072500                                                                          
072600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
072700     MOVE 'W4636M' TO POSTSUM-FDNAMN                                      
072800     MOVE 'W46363DM' TO POSTSUM-DDNAMN2                                   
072900     CALL POSTSUM USING POSTSUM-PARM                                      
073000     .                                                                    
073100     EJECT                                                                
073200 S1N-SKRIV-W4636N SECTION.                                                
073300                                                                          
073400     WRITE UT-N-POST FROM UT-AREA                                         
073500                                                                          
073600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
073700     MOVE 'W4636N' TO POSTSUM-FDNAMN                                      
073800     MOVE 'W46363DN' TO POSTSUM-DDNAMN2                                   
073900     CALL POSTSUM USING POSTSUM-PARM                                      
074000     .                                                                    
074100     EJECT                                                                
074200 S1O-SKRIV-W4636O SECTION.                                                
074300                                                                          
074400     WRITE UT-O-POST FROM UT-AREA                                         
074500                                                                          
074600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
074700     MOVE 'W4636O' TO POSTSUM-FDNAMN                                      
074800     MOVE 'W46363DO' TO POSTSUM-DDNAMN2                                   
074900     CALL POSTSUM USING POSTSUM-PARM                                      
075000     .                                                                    
075100     EJECT                                                                
075200 S1P-SKRIV-W4636P SECTION.                                                
075300                                                                          
075400     WRITE UT-P-POST FROM UT-AREA                                         
075500                                                                          
075600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
075700     MOVE 'W4636P' TO POSTSUM-FDNAMN                                      
075800     MOVE 'W46363DP' TO POSTSUM-DDNAMN2                                   
075900     CALL POSTSUM USING POSTSUM-PARM                                      
076000     .                                                                    
076100     EJECT                                                                
076200 S1Q-SKRIV-W4636Q SECTION.                                                
076300                                                                          
076400     WRITE UT-Q-POST FROM UT-AREA                                         
076500                                                                          
076600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
076700     MOVE 'W4636Q' TO POSTSUM-FDNAMN                                      
076800     MOVE 'W46363DQ' TO POSTSUM-DDNAMN2                                   
076900     CALL POSTSUM USING POSTSUM-PARM                                      
077000     .                                                                    
077100     EJECT                                                                
077200 S1R-SKRIV-W4636R SECTION.                                                
077300                                                                          
077400     WRITE UT-R-POST FROM UT-AREA                                         
077500                                                                          
077600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
077700     MOVE 'W4636R' TO POSTSUM-FDNAMN                                      
077800     MOVE 'W46363DR' TO POSTSUM-DDNAMN2                                   
077900     CALL POSTSUM USING POSTSUM-PARM                                      
078000     .                                                                    
078100     EJECT                                                                
078110 S1S-SKRIV-W4636S SECTION.                                                
078120                                                                          
078130     WRITE UT-S-POST FROM UT-AREA                                         
078140                                                                          
078150     MOVE SPACE TO POSTSUM-TRANSTYP                                       
078160     MOVE 'W4636S' TO POSTSUM-FDNAMN                                      
078170     MOVE 'W46363DS' TO POSTSUM-DDNAMN2                                   
078180     CALL POSTSUM USING POSTSUM-PARM                                      
078190     .                                                                    
078191     EJECT                                                                
078192 S1T-SKRIV-W4636T SECTION.                                                
078193*FILE TO VIPS-CZ HAS BEEN REMOVED FROM THIS VCOM PROCESS                  
078194*W46366 WILL HANDLE IT THRU MQ                                            
078200     .                                                                    
078201     EJECT                                                                
078202 S1U-SKRIV-W4636U SECTION.                                                
078203                                                                          
078204     WRITE UT-U-POST FROM UT-AREA                                         
078205                                                                          
078206     MOVE SPACE TO POSTSUM-TRANSTYP                                       
078207     MOVE 'W4636U' TO POSTSUM-FDNAMN                                      
078208     MOVE 'W46363DU' TO POSTSUM-DDNAMN2                                   
078209     CALL POSTSUM USING POSTSUM-PARM                                      
078210     .                                                                    
078211     EJECT                                                                
078212 S1V-SKRIV-W4636V SECTION.                                                
078213                                                                          
078214     WRITE UT-V-POST FROM UT-AREA                                         
078215                                                                          
078216     MOVE SPACE TO POSTSUM-TRANSTYP                                       
078217     MOVE 'W4636V' TO POSTSUM-FDNAMN                                      
078218     MOVE 'W46363DV' TO POSTSUM-DDNAMN2                                   
078219     CALL POSTSUM USING POSTSUM-PARM                                      
078220     .                                                                    
078221     EJECT                                                                
078222 S1W-SKRIV-W4636W SECTION.                                                
078223                                                                          
078224     WRITE UT-W-POST FROM UT-AREA                                         
078225                                                                          
078226     MOVE SPACE TO POSTSUM-TRANSTYP                                       
078227     MOVE 'W4636W' TO POSTSUM-FDNAMN                                      
078228     MOVE 'W46363DW' TO POSTSUM-DDNAMN2                                   
078229     CALL POSTSUM USING POSTSUM-PARM                                      
078230     .                                                                    
078231     EJECT                                                                
078240 S1PA-SKRIV-W463PA SECTION.                                               
078300                                                                          
078400     WRITE UT-PA-POST FROM UT-AREA                                        
078500                                                                          
078600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
078700     MOVE 'W463PA' TO POSTSUM-FDNAMN                                      
078800     MOVE 'W46363PA' TO POSTSUM-DDNAMN2                                   
078900     CALL POSTSUM USING POSTSUM-PARM                                      
079000     .                                                                    
079100     EJECT                                                                
079200 S1PB-SKRIV-W463PB SECTION.                                               
079300                                                                          
079400     WRITE UT-PB-POST FROM UT-AREA                                        
079500                                                                          
079600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
079700     MOVE 'W463PB' TO POSTSUM-FDNAMN                                      
079800     MOVE 'W46363PB' TO POSTSUM-DDNAMN2                                   
079900     CALL POSTSUM USING POSTSUM-PARM                                      
080000     .                                                                    
080100     EJECT                                                                
080200 S1PC-SKRIV-W463PC SECTION.                                               
080300                                                                          
080400     WRITE UT-PC-POST FROM UT-AREA                                        
080500                                                                          
080600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
080700     MOVE 'W463PC' TO POSTSUM-FDNAMN                                      
080800     MOVE 'W46363PC' TO POSTSUM-DDNAMN2                                   
080900     CALL POSTSUM USING POSTSUM-PARM                                      
081000     .                                                                    
081100     EJECT                                                                
081200 S1PD-SKRIV-W463PD SECTION.                                               
081300                                                                          
081400     WRITE UT-PD-POST FROM UT-AREA                                        
081500                                                                          
081600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
081700     MOVE 'W463PD' TO POSTSUM-FDNAMN                                      
081800     MOVE 'W46363PD' TO POSTSUM-DDNAMN2                                   
081900     CALL POSTSUM USING POSTSUM-PARM                                      
082000     .                                                                    
082100     EJECT                                                                
082200 S1PE-SKRIV-W463PE SECTION.                                               
082300                                                                          
082400     WRITE UT-PE-POST FROM UT-AREA                                        
082500                                                                          
082600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
082700     MOVE 'W463PE' TO POSTSUM-FDNAMN                                      
082800     MOVE 'W46363PE' TO POSTSUM-DDNAMN2                                   
082900     CALL POSTSUM USING POSTSUM-PARM                                      
083000     .                                                                    
083100     EJECT                                                                
083200 S1PF-SKRIV-W463PF SECTION.                                               
083300                                                                          
083400     WRITE UT-PF-POST FROM UT-AREA                                        
083500                                                                          
083600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
083700     MOVE 'W463PF' TO POSTSUM-FDNAMN                                      
083800     MOVE 'W46363PF' TO POSTSUM-DDNAMN2                                   
083900     CALL POSTSUM USING POSTSUM-PARM                                      
084000     .                                                                    
084100     EJECT                                                                
084200 S1PG-SKRIV-W463PG SECTION.                                               
084300                                                                          
084400     WRITE UT-PG-POST FROM UT-AREA                                        
084500                                                                          
084600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
084700     MOVE 'W463PG' TO POSTSUM-FDNAMN                                      
084800     MOVE 'W46363PG' TO POSTSUM-DDNAMN2                                   
084900     CALL POSTSUM USING POSTSUM-PARM                                      
085000     .                                                                    
085100     EJECT                                                                
085200 S1PH-SKRIV-W463PH SECTION.                                               
085300                                                                          
085400     WRITE UT-PH-POST FROM UT-AREA                                        
085500                                                                          
085600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
085700     MOVE 'W463PH' TO POSTSUM-FDNAMN                                      
085800     MOVE 'W46363PH' TO POSTSUM-DDNAMN2                                   
085900     CALL POSTSUM USING POSTSUM-PARM                                      
086000     .                                                                    
086100     EJECT                                                                
086200 S1PI-SKRIV-W463PI SECTION.                                               
086300                                                                          
086400     WRITE UT-PI-POST FROM UT-AREA                                        
086500                                                                          
086600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
086700     MOVE 'W463PI' TO POSTSUM-FDNAMN                                      
086800     MOVE 'W46363PI' TO POSTSUM-DDNAMN2                                   
086900     CALL POSTSUM USING POSTSUM-PARM                                      
087000     .                                                                    
087100     EJECT                                                                
087200 S1PJ-SKRIV-W463PJ SECTION.                                               
087300                                                                          
087400     WRITE UT-PJ-POST FROM UT-AREA                                        
087500                                                                          
087600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
087700     MOVE 'W463PJ' TO POSTSUM-FDNAMN                                      
087800     MOVE 'W46363PJ' TO POSTSUM-DDNAMN2                                   
087900     CALL POSTSUM USING POSTSUM-PARM                                      
088000     .                                                                    
088100     EJECT                                                                
088200 S1PK-SKRIV-W463PK SECTION.                                               
088300                                                                          
088400     WRITE UT-PK-POST FROM UT-AREA                                        
088500                                                                          
088600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
088700     MOVE 'W463PK' TO POSTSUM-FDNAMN                                      
088800     MOVE 'W46363PK' TO POSTSUM-DDNAMN2                                   
088900     CALL POSTSUM USING POSTSUM-PARM                                      
089000     .                                                                    
089100     EJECT                                                                
089200 S1PL-SKRIV-W463PL SECTION.                                               
089300                                                                          
089400     WRITE UT-PL-POST FROM UT-AREA                                        
089500                                                                          
089600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
089700     MOVE 'W463PL' TO POSTSUM-FDNAMN                                      
089800     MOVE 'W46363PL' TO POSTSUM-DDNAMN2                                   
089900     CALL POSTSUM USING POSTSUM-PARM                                      
090000     .                                                                    
090100     EJECT                                                                
090200 S1PM-SKRIV-W463PM SECTION.                                               
090300                                                                          
090400     WRITE UT-PM-POST FROM UT-AREA                                        
090500                                                                          
090600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
090700     MOVE 'W463PM' TO POSTSUM-FDNAMN                                      
090800     MOVE 'W46363PM' TO POSTSUM-DDNAMN2                                   
090900     CALL POSTSUM USING POSTSUM-PARM                                      
091000     .                                                                    
091100     EJECT                                                                
091200 S1PN-SKRIV-W463PN SECTION.                                               
091300                                                                          
091400     WRITE UT-PN-POST FROM UT-AREA                                        
091500                                                                          
091600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
091700     MOVE 'W463PN' TO POSTSUM-FDNAMN                                      
091800     MOVE 'W46363PN' TO POSTSUM-DDNAMN2                                   
091900     CALL POSTSUM USING POSTSUM-PARM                                      
092000     .                                                                    
092100     EJECT                                                                
092200 S1PO-SKRIV-W463PO SECTION.                                               
092300                                                                          
092400     WRITE UT-PO-POST FROM UT-AREA                                        
092500                                                                          
092600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
092700     MOVE 'W463PO' TO POSTSUM-FDNAMN                                      
092800     MOVE 'W46363PO' TO POSTSUM-DDNAMN2                                   
092900     CALL POSTSUM USING POSTSUM-PARM                                      
093000     .                                                                    
093100     EJECT                                                                
093200 S1PP-SKRIV-W463PP SECTION.                                               
093300                                                                          
093400     WRITE UT-PP-POST FROM UT-AREA                                        
093500                                                                          
093600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
093700     MOVE 'W463PP' TO POSTSUM-FDNAMN                                      
093800     MOVE 'W46363PP' TO POSTSUM-DDNAMN2                                   
093900     CALL POSTSUM USING POSTSUM-PARM                                      
094000     .                                                                    
094100     EJECT                                                                
094200 S1PQ-SKRIV-W463PQ SECTION.                                               
094300                                                                          
094400     WRITE UT-PQ-POST FROM UT-AREA                                        
094500                                                                          
094600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
094700     MOVE 'W463PQ' TO POSTSUM-FDNAMN                                      
094800     MOVE 'W46363PQ' TO POSTSUM-DDNAMN2                                   
094900     CALL POSTSUM USING POSTSUM-PARM                                      
095000     .                                                                    
095100     EJECT                                                                
095200 S1PR-SKRIV-W463PR SECTION.                                               
095300                                                                          
095400     WRITE UT-PR-POST FROM UT-AREA                                        
095500                                                                          
095600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
095700     MOVE 'W463PR' TO POSTSUM-FDNAMN                                      
095800     MOVE 'W46363PR' TO POSTSUM-DDNAMN2                                   
095900     CALL POSTSUM USING POSTSUM-PARM                                      
096000     .                                                                    
096100     EJECT                                                                
096110 S1PS-SKRIV-W463PS SECTION.                                               
096120                                                                          
096130     WRITE UT-PS-POST FROM UT-AREA                                        
096140                                                                          
096150     MOVE SPACE TO POSTSUM-TRANSTYP                                       
096160     MOVE 'W463PS' TO POSTSUM-FDNAMN                                      
096170     MOVE 'W46363PS' TO POSTSUM-DDNAMN2                                   
096180     CALL POSTSUM USING POSTSUM-PARM                                      
096190     .                                                                    
096191     EJECT                                                                
096192 S1PT-SKRIV-W463PT SECTION.                                               
096193                                                                          
096194     WRITE UT-PT-POST FROM UT-AREA                                        
096195                                                                          
096196     MOVE SPACE TO POSTSUM-TRANSTYP                                       
096197     MOVE 'W463PT' TO POSTSUM-FDNAMN                                      
096198     MOVE 'W46363PT' TO POSTSUM-DDNAMN2                                   
096199     CALL POSTSUM USING POSTSUM-PARM                                      
096200     .                                                                    
096201     EJECT                                                                
096202 S1PU-SKRIV-W463PU SECTION.                                               
096203                                                                          
096204     WRITE UT-PU-POST FROM UT-AREA                                        
096205                                                                          
096206     MOVE SPACE TO POSTSUM-TRANSTYP                                       
096207     MOVE 'W463PU' TO POSTSUM-FDNAMN                                      
096208     MOVE 'W46363PU' TO POSTSUM-DDNAMN2                                   
096209     CALL POSTSUM USING POSTSUM-PARM                                      
096210     .                                                                    
096211     EJECT                                                                
096212 S1PV-SKRIV-W463PV SECTION.                                               
096213                                                                          
096214     WRITE UT-PV-POST FROM UT-AREA                                        
096215                                                                          
096216     MOVE SPACE TO POSTSUM-TRANSTYP                                       
096217     MOVE 'W463PV' TO POSTSUM-FDNAMN                                      
096218     MOVE 'W46363PV' TO POSTSUM-DDNAMN2                                   
096219     CALL POSTSUM USING POSTSUM-PARM                                      
096220     .                                                                    
096221     EJECT                                                                
096222 S1PW-SKRIV-W463PW SECTION.                                               
096223                                                                          
096224     WRITE UT-PW-POST FROM UT-AREA                                        
096225                                                                          
096226     MOVE SPACE TO POSTSUM-TRANSTYP                                       
096227     MOVE 'W463PW' TO POSTSUM-FDNAMN                                      
096228     MOVE 'W46363PW' TO POSTSUM-DDNAMN2                                   
096229     CALL POSTSUM USING POSTSUM-PARM                                      
096230     .                                                                    
096231     EJECT                                                                
096240 S99-ABEND SECTION.                                                       
096300                                                                          
096400     SKIP2                                                                
096500     MOVE 'S' TO POSTSUM-OPKOD                                            
096600     CALL POSTSUM USING POSTSUM-PARM                                      
096700     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
096800     .                                                                    
