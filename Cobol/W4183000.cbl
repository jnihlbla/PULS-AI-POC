000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4183000.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   95/08/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR RETURTILLSTÅND,KREDITNOTOR OCH TILLÄGGSFAKTUROR.          
001100*        VIA SUBPROGRAM.                                                  
001200*        LÄGGER UPP INVENTERINGAR PÅ INVENTERINGSKÖN (WDH1).              
001300*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
001400*        !!!OBS - BELOPPSGRÄNSER FÖR INVENTERING ÄR HÅRDKODADE !!!        
001500*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
001600*        !!!OBS - SDC OCH LDC KAN HA OLIKA BELOPPSGRÄNSER ÄVEN   !        
001700*        !!!INOM SAMMA LAND, ÄVEN 2 LDC'R I SAMMA LAND !!!!!!!!!!!        
001800*                                                                         
001900*        PROGRAMMET LÄSER      WDA2A                                      
002000*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
002100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002200*        PROGRAMMET LÄSER      WLARTN (WDD5)                              
002300*        PROGRAMMET LÄSER      WLINVA (WDH1)                              
002400*        PROGRAMMET LÄSER      WLFAKC (WDL5)                              
002500*        PROGRAMMET LÄSER      WLORQL (WDQ2C)  ORDERHUVUD                 
002600*        PROGRAMMET LÄSER      WDR5 (WDGX4103)                            
002700*        PROGRAMMET LÄSER      WDK7                                       
002800* PROGRAMMET LÄSER      WDB6                                              
002900*                                                                         
003000*    E-TRACKER: 1572353 2072166 1658417 3298307 3107778                   
003100*    E-TRACKER: 2420793 2913019 1869156 3181138 3739101                   
003200*    E-TRACKER: 3822419 3822403 850114  4823800 5838822                   
003300*    E-TRACKER: 5823276 5984456 880053                                    
003400*                                                                         
003500*    2011-10-13 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1              
003600*                                                                         
003700*    2012-04-18 E-TRACKER 10169986 CHINA WAREHOUSE RÄTTA FEL              
003800*                                                                         
003900*    ABENDKODER:                                                          
004000*        U0016 -  . . . .                                                 
004100*        U1000 -  . . . .                                                 
004200*                                                                         
004300*                                                                         
004400                                                                          
004500 ENVIRONMENT DIVISION.                                                    
004600 INPUT-OUTPUT SECTION.                                                    
004700 FILE-CONTROL.                                                            
004800*          --- NUMMERSERIE IN                                             
004900     SELECT W41801-IN                  ASSIGN TO W41830D1.                
005000     EJECT                                                                
005100*          --- NUMMERSERIE UT                                             
005200     SELECT W41801-UT                  ASSIGN TO W41830D2.                
005300     EJECT                                                                
005400*          --- TILL EKONOMI-LAB  KONTERING                                
005500     SELECT W41833                     ASSIGN TO W41830D3.                
005600     EJECT                                                                
005700*          --- POSTER KLARA FÖR KREDITERING OCH RETILL                    
005800     SELECT W41835                     ASSIGN TO W41830D4.                
005900     EJECT                                                                
006000*          --- TILLÄGGSFAKTUROR TILL ORDER (W412)                         
006100     SELECT W41836                     ASSIGN TO W41830D5.                
006200     EJECT                                                                
006300*          --- KREDITINFO TILL VIPS                                       
006400     SELECT W41837                     ASSIGN TO W41830D6.                
006500     EJECT                                                                
006600*          --- ARTIKELSTATISTIK                                           
006700     SELECT W41839                     ASSIGN TO W41830D7.                
006800     EJECT                                                                
006900*          --- AVVISADE TRANSAR TILL VIPS                                 
007000     SELECT W4183A                     ASSIGN TO W41830D8.                
007100     EJECT                                                                
007200*          --- RETURTILLSTÅND TILL VIPS                                   
007300     SELECT W4183B                     ASSIGN TO W41830D9.                
007400     EJECT                                                                
007500*          --- UPPDATERINGSPOSTER TILL BMP W41835                         
007600     SELECT W41834                     ASSIGN TO W41830DA.                
007700     EJECT                                                                
007800*          --- TEXTINFO TILL DEALER I USA                                 
007900     SELECT W4183C                     ASSIGN TO W41830DB.                
008000     EJECT                                                                
008100*          --- TEXTINFO TILL DEALER I CANADA                              
008200     SELECT W4183D                     ASSIGN TO W41830DC.                
008300     EJECT                                                                
008400*          --- KREDITINFO TILL BYTES ( RUTIN W371D5 )                     
008500     SELECT W418AG                     ASSIGN TO W41830DD.                
008600     EJECT                                                                
008700*          --- TEXTINFO TILL DEALER I BELGIEN                             
008800     SELECT W4183E                     ASSIGN TO W41830DE.                
008900     EJECT                                                                
009000*          --- TEXTINFO TILL DEALER I HOLLAND                             
009100     SELECT W4183F                     ASSIGN TO W41830DF.                
009200     EJECT                                                                
009300*          --- TEXTINFO TILL DEALER I ENGLAND                             
009400     SELECT W4183G                     ASSIGN TO W41830DG.                
009500     EJECT                                                                
009600*          --- OKÄND ART, KOD 12 OCH 22                                   
009700     SELECT W4183H                     ASSIGN TO W41830DH.                
009800     EJECT                                                                
009900*          --- TEXTINFO TILL DEALER I KOREA                               
010000     SELECT W4183I                     ASSIGN TO W41830DI.                
010100     EJECT                                                                
010200*          --- TEXTINFO TILL DEALER I POLEN                               
010300     SELECT W4183J                     ASSIGN TO W41830DJ.                
010400     EJECT                                                                
010500*          --- TEXTINFO TILL DEALER I TYSKLAND                            
010600     SELECT W4183K                     ASSIGN TO W41830DK.                
010700     EJECT                                                                
010800*          --- KREDITRADER TILL BILL-IT (BMP W4183100)                    
010900     SELECT W418AI                     ASSIGN TO W41830DL.                
011000     EJECT                                                                
011100*          --- TEXTINFO TILL DEALER I NORGE                               
011200     SELECT W4183N                     ASSIGN TO W41830DN.                
011300     EJECT                                                                
011400*          --- TEXTINFO TILL DEALER I DANMARK                             
011500     SELECT W4183O                     ASSIGN TO W41830DO.                
011600     EJECT                                                                
011700*          --- TEXTINFO TILL DEALER I AUSTRALIEN                          
011800     SELECT W4183P                     ASSIGN TO W41830DP.                
011900     EJECT                                                                
012000*          --- TEXTINFO TILL DEALER I SPANIEN                             
012100     SELECT W4183Q                     ASSIGN TO W41830DQ.                
012200     EJECT                                                                
012300*          --- TEXTINFO TILL DEALER I SVERIGE                             
012400     SELECT W4183R                     ASSIGN TO W41830DR.                
012500     EJECT                                                                
012600*          --- UTFIL  EKONOMIFIL KOD 12,22,13 OCH 23.27,28                
012700     SELECT W41830                     ASSIGN TO W41830DS.                
012800     EJECT                                                                
012900*          --- UTFIL  EKONOMIFIL KOD 12,22,13 OCH 23                      
013000     SELECT W41832                     ASSIGN TO W41830DM.                
013100     EJECT                                                                
013200*          --- TEXTINFO TILL DEALER I ITALIEN                             
013300     SELECT W4183T                     ASSIGN TO W41830DT.                
013400     EJECT                                                                
013500*          --- TEXTINFO TILL DEALER I FINLAND                             
013600     SELECT W4183U                     ASSIGN TO W41830DU.                
013700     EJECT                                                                
013800*          --- TEXTINFO TILL DEALER I FRANKRIKE                           
013900     SELECT W4183V                     ASSIGN TO W41830DV.                
014000     EJECT                                                                
014100*          --- TEXTINFO TILL DEALER I ÖSTERRIKE                           
014200     SELECT W4183W                     ASSIGN TO W41830DW.                
014300     EJECT                                                                
014400*          --- TEXTINFO TILL DEALER I JAPAN                               
014500     SELECT W4183X                     ASSIGN TO W41830DX.                
014600     EJECT                                                                
014700*          --- TEXTINFO TILL DEALER I SCHWEIZ                             
014800     SELECT W4183Y                     ASSIGN TO W41830DY.                
014900     EJECT                                                                
015000*          --- TEXTINFO TILL DEALER I TAIWAN                              
015100     SELECT W4183Z                     ASSIGN TO W41830DZ.                
015200     EJECT                                                                
015300*          --- TEXTINFO TILL DEALER I TAIWAN, NY (6222)                   
015400     SELECT W4183L                     ASSIGN TO W41830E9.                
015500     EJECT                                                                
015600*          --- TEXTINFO TILL DEALER I PORTUGAL                            
015700     SELECT W418AA                     ASSIGN TO W41830E1.                
015800     EJECT                                                                
015900*          --- TEXTINFO TILL DEALER I MALAYSIA                            
016000     SELECT W418AB                     ASSIGN TO W41830E2.                
016100     EJECT                                                                
016200*          --- TEXTINFO TILL DEALER I THAILAND                            
016300     SELECT W418AC                     ASSIGN TO W41830E3.                
016400     EJECT                                                                
016500*          --- TEXTINFO TILL DEALER I IRLAND                              
016600     SELECT W418AD                     ASSIGN TO W41830E4.                
016700     EJECT                                                                
016800*          --- UTFIL TILL PGM W41843 FÖR VIR-RAPPORT                      
016900     SELECT W41843                     ASSIGN TO W41830E5.                
017000     EJECT                                                                
017100*          --- TEXTINFO TILL DEALER I BRASILIEN                           
017200     SELECT W418AE                     ASSIGN TO W41830E6.                
017300     EJECT                                                                
017400*          --- TEXTINFO TILL DEALER I MEXICO                              
017500     SELECT W418AF                     ASSIGN TO W41830E7.                
017600     EJECT                                                                
017700*          --- TEXTINFO TILL DEALER I TURKIET                             
017800     SELECT W418AH                     ASSIGN TO W41830E8.                
017900     EJECT                                                                
018000*          --- TEXTINFO FÖR DDI - FIX SÅ LÄNGE                            
018100     SELECT W418AN                     ASSIGN TO W41830EA.                
018200     EJECT                                                                
018300*          --- TEXTINFO TILL DEALER I RYSSLAND                            
018400     SELECT W418AP                     ASSIGN TO W41830EB.                
018500     EJECT                                                                
018600*          --- TEXTINFO TILL DEALER I SYDAFRIKA                           
018700     SELECT W418AQ                     ASSIGN TO W41830EC.                
018800     EJECT                                                                
018900     EJECT                                                                
019000*          --- TEXTINFO TILL DEALER I PORTUGAL 1958                       
019100     SELECT W418AS                     ASSIGN TO W41830EE.                
019200     EJECT                                                                
019300*          --- FAKTURARADER KOD 72/98 FÖR HANDLING FEE                    
019400     SELECT W418AT                     ASSIGN TO W41830EF.                
019500     EJECT                                                                
019600*          --- TEXTINFO TILL DEALER I KINA-C1                             
019700     SELECT W418C1                     ASSIGN TO W41830EG.                
019800     EJECT                                                                
019900*          --- UTFIL  EKONOMIFIL KOD 12,22,13 OCH 23                      
020000     SELECT W41831                     ASSIGN TO W41830EH.                
020100     EJECT                                                                
020200*          --- TEXTINFO TILL DEALER I INDIEN                              
020300     SELECT W418IN                     ASSIGN TO W41830EI.                
020400     EJECT                                                                
020500*          --- TEXTINFO TILL DEALER I TJECKIEN                            
020600     SELECT W418CZ                     ASSIGN TO W41830EJ.                
020700     EJECT                                                                
020800*          --- TEXTINFO TILL DEALER I UNGERN                              
020900     SELECT W418HU                     ASSIGN TO W41830EK.                
021000     EJECT                                                                
021100**   --- MQ GENERIC FILE WITH COUNTRY INFO ---  **                        
021200     SELECT W418MQA                    ASSIGN TO W41830MQ.                
021300     EJECT                                                                
021400 DATA DIVISION.                                                           
021500     SKIP2                                                                
021600 FILE SECTION.                                                            
021700     SKIP3                                                                
021800 FD  W41801-IN                                                            
021900     RECORDING       F                                                    
022000     BLOCK CONTAINS  0.                                                   
022100                                                                          
022200*01  -COPY W41801      -L.                                                
022300     SKIP3                                                                
022400 FD  W41801-UT                                                            
022500     RECORDING       F                                                    
022600     BLOCK CONTAINS  0.                                                   
022700                                                                          
022800*01  POST -COPY W41801 -PRE  UT-  -L.                                     
022900     SKIP3                                                                
023000 FD  W41833                                                               
023100     RECORDING       F                                                    
023200     BLOCK CONTAINS  0.                                                   
023300                                                                          
023400*01  POST -COPY W41833  -PRE  720-   -L.                                  
023500     SKIP3                                                                
023600 FD  W41835                                                               
023700     RECORDING       V                                                    
023800     BLOCK CONTAINS  0.                                                   
023900                                                                          
024000*01  POST -COPY W418712      -PRE  712-  -L.                              
024100                                                                          
024200*01  POST -COPY W418713      -PRE  713-  -L.                              
024300                                                                          
024400*01  POST -COPY W418717      -PRE  717-  -L.                              
024500                                                                          
024600*01  POST -COPY W418718      -PRE  718-  -L.                              
024700     SKIP3                                                                
024800 FD  W41836                                                               
024900     RECORDING       V                                                    
025000     BLOCK CONTAINS  0.                                                   
025100                                                                          
025200*01  POST -COPY W418DR5      -PRE  DR5-  -L.                              
025300     SKIP3                                                                
025400 FD  W41837                                                               
025500     RECORDING       V                                                    
025600     BLOCK CONTAINS  0.                                                   
025700                                                                          
025800*01  POST -COPY W461021 -PRE UT37-  -L.                                   
025900     SKIP3                                                                
026000 FD  W41839                                                               
026100     RECORDING       F                                                    
026200     BLOCK CONTAINS  0.                                                   
026300                                                                          
026400*01  POST -COPY W330099 -PRE  UT39-  -L.                                  
026500     SKIP3                                                                
026600 FD  W4183A                                                               
026700     RECORDING       V                                                    
026800     BLOCK CONTAINS  0.                                                   
026900                                                                          
027000*01  POST -COPY W461RKDN -PRE  UT3A-  -L.                                 
027100     SKIP3                                                                
027200 FD  W4183B                                                               
027300     RECORDING       V                                                    
027400     BLOCK CONTAINS  0.                                                   
027500                                                                          
027600*01  POST -COPY W461RKEN -PRE  UT3B-  -L.                                 
027700     SKIP3                                                                
027800 FD  W41834                                                               
027900     RECORDING       F                                                    
028000     BLOCK CONTAINS  0.                                                   
028100                                                                          
028200*01  POST -COPY W41834  -PRE  UT34-  -L.                                  
028300     SKIP3                                                                
028400 FD  W4183C                                                               
028500     RECORDING       F                                                    
028600     BLOCK CONTAINS  0.                                                   
028700                                                                          
028800*01  POST -COPY W418003A -PRE  UT3C-  -L.                                 
028900     EJECT                                                                
029000 FD  W4183D                                                               
029100     RECORDING       F                                                    
029200     BLOCK CONTAINS  0.                                                   
029300                                                                          
029400*01  POST -COPY W418003A -PRE  UT3D-  -L.                                 
029500     EJECT                                                                
029600 FD  W4183E                                                               
029700     RECORDING       F                                                    
029800     BLOCK CONTAINS  0.                                                   
029900                                                                          
030000*01  POST -COPY W418003A -PRE  UT3E-  -L.                                 
030100     EJECT                                                                
030200 FD  W4183F                                                               
030300     RECORDING       F                                                    
030400     BLOCK CONTAINS  0.                                                   
030500                                                                          
030600*01  POST -COPY W418003A -PRE  UT3F-  -L.                                 
030700     EJECT                                                                
030800 FD  W4183G                                                               
030900     RECORDING       F                                                    
031000     BLOCK CONTAINS  0.                                                   
031100                                                                          
031200*01  POST -COPY W418003A -PRE  UT3G-  -L.                                 
031300     EJECT                                                                
031400 FD  W4183H                                                               
031500     RECORDING       F                                                    
031600     BLOCK CONTAINS  0.                                                   
031700                                                                          
031800*01  POST -COPY W4183H   -PRE  UT3H-  -L.                                 
031900     EJECT                                                                
032000 FD  W4183I                                                               
032100     RECORDING       F                                                    
032200     BLOCK CONTAINS  0.                                                   
032300                                                                          
032400*01  POST -COPY W418003A -PRE  UT3I-  -L.                                 
032500     EJECT                                                                
032600 FD  W4183J                                                               
032700     RECORDING       F                                                    
032800     BLOCK CONTAINS  0.                                                   
032900                                                                          
033000*01  POST -COPY W418003A -PRE  UT3J-  -L.                                 
033100     EJECT                                                                
033200 FD  W4183K                                                               
033300     RECORDING       F                                                    
033400     BLOCK CONTAINS  0.                                                   
033500                                                                          
033600*01  POST -COPY W418003A -PRE  UT3K-  -L.                                 
033700     EJECT                                                                
033800     SKIP3                                                                
033900 FD  W418AI                                                               
034000     RECORDING       V                                                    
034100     BLOCK CONTAINS  0.                                                   
034200                                                                          
034300*01  POST -COPY W41831A      -PRE  31A-  -L.                              
034400                                                                          
034500*01  POST -COPY W41831B      -PRE  31B-  -L.                              
034600     EJECT                                                                
034700 FD  W4183N                                                               
034800     RECORDING       F                                                    
034900     BLOCK CONTAINS  0.                                                   
035000                                                                          
035100*01  POST -COPY W418003A -PRE  UT3N-  -L.                                 
035200     EJECT                                                                
035300 FD  W4183O                                                               
035400     RECORDING       F                                                    
035500     BLOCK CONTAINS  0.                                                   
035600                                                                          
035700*01  POST -COPY W418003A -PRE  UT3O-  -L.                                 
035800     EJECT                                                                
035900 FD  W4183P                                                               
036000     RECORDING       F                                                    
036100     BLOCK CONTAINS  0.                                                   
036200                                                                          
036300*01  POST -COPY W418003A -PRE  UT3P-  -L.                                 
036400     EJECT                                                                
036500 FD  W4183Q                                                               
036600     RECORDING       F                                                    
036700     BLOCK CONTAINS  0.                                                   
036800                                                                          
036900*01  POST -COPY W418003A -PRE  UT3Q-  -L.                                 
037000     EJECT                                                                
037100 FD  W4183R                                                               
037200     RECORDING       F                                                    
037300     BLOCK CONTAINS  0.                                                   
037400                                                                          
037500*01  POST -COPY W418003A -PRE  UT3R-  -L.                                 
037600     EJECT                                                                
037700 FD  W4183T                                                               
037800     RECORDING       F                                                    
037900     BLOCK CONTAINS  0.                                                   
038000                                                                          
038100*01  POST -COPY W418003A -PRE  UT3T-  -L.                                 
038200     EJECT                                                                
038300 FD  W4183U                                                               
038400     RECORDING       F                                                    
038500     BLOCK CONTAINS  0.                                                   
038600*01  POST -COPY W418003A -PRE  UT3U-  -L.                                 
038700     EJECT                                                                
038800                                                                          
038900 FD  W4183V                                                               
039000     RECORDING       F                                                    
039100     BLOCK CONTAINS  0.                                                   
039200*01  POST -COPY W418003A -PRE  UT3V-  -L.                                 
039300     EJECT                                                                
039400                                                                          
039500 FD  W4183W                                                               
039600     RECORDING       F                                                    
039700     BLOCK CONTAINS  0.                                                   
039800*01  POST -COPY W418003A -PRE  UT3W-  -L.                                 
039900     EJECT                                                                
040000                                                                          
040100 FD  W4183X                                                               
040200     RECORDING       F                                                    
040300     BLOCK CONTAINS  0.                                                   
040400*01  POST -COPY W418003A -PRE  UT3X-  -L.                                 
040500     EJECT                                                                
040600                                                                          
040700 FD  W4183Y                                                               
040800     RECORDING       F                                                    
040900     BLOCK CONTAINS  0.                                                   
041000*01  POST -COPY W418003A -PRE  UT3Y-  -L.                                 
041100     EJECT                                                                
041200                                                                          
041300 FD  W4183Z                                                               
041400     RECORDING       F                                                    
041500     BLOCK CONTAINS  0.                                                   
041600*01  POST -COPY W418003A -PRE  UT3Z-  -L.                                 
041700     EJECT                                                                
041800                                                                          
041900 FD  W4183L                                                               
042000     RECORDING       F                                                    
042100     BLOCK CONTAINS  0.                                                   
042200*01  POST -COPY W418003A -PRE  UT3L-  -L.                                 
042300     EJECT                                                                
042400                                                                          
042500 FD  W418AA                                                               
042600     RECORDING       F                                                    
042700     BLOCK CONTAINS  0.                                                   
042800*01  POST -COPY W418003A -PRE  UTAA-  -L.                                 
042900     EJECT                                                                
043000                                                                          
043100 FD  W418AB                                                               
043200     RECORDING       F                                                    
043300     BLOCK CONTAINS  0.                                                   
043400*01  POST -COPY W418003A -PRE  UTAB-  -L.                                 
043500     EJECT                                                                
043600                                                                          
043700 FD  W418AC                                                               
043800     RECORDING       F                                                    
043900     BLOCK CONTAINS  0.                                                   
044000*01  POST -COPY W418003A -PRE  UTAC-  -L.                                 
044100     EJECT                                                                
044200                                                                          
044300 FD  W418AD                                                               
044400     RECORDING       F                                                    
044500     BLOCK CONTAINS  0.                                                   
044600*01  POST -COPY W418003A -PRE  UTAD-  -L.                                 
044700     EJECT                                                                
044800                                                                          
044900 FD  W418AE                                                               
045000     RECORDING       F                                                    
045100     BLOCK CONTAINS  0.                                                   
045200*01  POST -COPY W418003A -PRE  UTAE-  -L.                                 
045300     EJECT                                                                
045400                                                                          
045500 FD  W418AF                                                               
045600     RECORDING       F                                                    
045700     BLOCK CONTAINS  0.                                                   
045800*01  POST -COPY W418003A -PRE  UTAF-  -L.                                 
045900     EJECT                                                                
046000                                                                          
046100 FD  W418AG                                                               
046200     RECORDING       F                                                    
046300     BLOCK CONTAINS  0.                                                   
046400*01  POST -COPY W371FAK  -PRE  UTAG-  -L.                                 
046500     EJECT                                                                
046600                                                                          
046700 FD  W418AH                                                               
046800     RECORDING       F                                                    
046900     BLOCK CONTAINS  0.                                                   
047000*01  POST -COPY W418003A -PRE  UTAH-  -L.                                 
047100     EJECT                                                                
047200                                                                          
047300 FD  W41830                                                               
047400     RECORDING       F                                                    
047500     BLOCK CONTAINS  0.                                                   
047600*01  W41830 -COPY W51060  -PRE UT30-   -L.                                
047700     EJECT                                                                
047800                                                                          
047900 FD  W41832                                                               
048000     RECORDING       F                                                    
048100     BLOCK CONTAINS  0.                                                   
048200*01  W41832 -COPY W57060  -PRE UT32-   -L.                                
048300     EJECT                                                                
048400                                                                          
048500 FD  W41843                                                               
048600     RECORDING       F                                                    
048700     BLOCK CONTAINS  0.                                                   
048800*01  POST -COPY W41843  -PRE UT43-  -L.                                   
048900     EJECT                                                                
049000                                                                          
049100 FD  W418AN                                                               
049200     RECORDING       V                                                    
049300     BLOCK CONTAINS  0.                                                   
049400 01  UTAN-POST     PIC X(500).                                            
049500                                                                          
049600                                                                          
049700 FD  W418AP                                                               
049800     RECORDING       F                                                    
049900     BLOCK CONTAINS  0.                                                   
050000*01  POST -COPY W418003A -PRE  UTAP-  -L.                                 
050100     EJECT                                                                
050200                                                                          
050300 FD  W418AQ                                                               
050400     RECORDING       F                                                    
050500     BLOCK CONTAINS  0.                                                   
050600*01  POST -COPY W418003A -PRE  UTAQ-  -L.                                 
050700     EJECT                                                                
050800                                                                          
050900 FD  W418AS                                                               
051000     RECORDING       F                                                    
051100     BLOCK CONTAINS  0.                                                   
051200*01  POST -COPY W418003A -PRE  UTAS-  -L.                                 
051300     EJECT                                                                
051400                                                                          
051500 FD  W418AT                                                               
051600     RECORDING       F                                                    
051700     BLOCK CONTAINS  0.                                                   
051800*01  POST -COPY W418FEE  -PRE  UTAT-  -L.                                 
051900     EJECT                                                                
052000                                                                          
052100 FD  W418C1                                                               
052200     RECORDING       F                                                    
052300     BLOCK CONTAINS  0.                                                   
052400*01  POST -COPY W418003A -PRE  UTC1-  -L.                                 
052500     EJECT                                                                
052600                                                                          
052700 FD  W41831                                                               
052800     RECORDING       F                                                    
052900     BLOCK CONTAINS  0.                                                   
053000*01  W41831 -COPY W51560  -PRE UT32A-   -L.                               
053100     EJECT                                                                
053200                                                                          
053300 FD  W418IN                                                               
053400     RECORDING       F                                                    
053500     BLOCK CONTAINS  0.                                                   
053600*01  POST -COPY W418003A -PRE  UTIN-  -L.                                 
053700     EJECT                                                                
053800                                                                          
053900 FD  W418CZ                                                               
054000     RECORDING       F                                                    
054100     BLOCK CONTAINS  0.                                                   
054200*01  POST -COPY W418003A -PRE  UTCZ-  -L.                                 
054300     EJECT                                                                
054400                                                                          
054500 FD  W418HU                                                               
054600     RECORDING       F                                                    
054700     BLOCK CONTAINS  0.                                                   
054800*01  POST -COPY W418003A -PRE  UTHU-  -L.                                 
054900     EJECT                                                                
055000                                                                          
055100 FD  W418MQA                                                              
055200     RECORDING       F                                                    
055300     BLOCK CONTAINS  0.                                                   
055400*01  POST -COPY W418003B -PRE  UTMQ-  -L.                                 
055500     EJECT                                                                
055600                                                                          
055700                                                                          
055800 WORKING-STORAGE SECTION.                                                 
055900                                                                          
056000*    -- CHECKED BY WY2000                                                 
056100     SKIP3                                                                
056200 77  IDPGM                       PIC X(8)    VALUE 'W4183000'.            
056300 77  JA                          PIC X       VALUE 'J'.                   
056400 77  NEJ                         PIC X       VALUE 'N'.                   
056500*FIX CO                                                                   
056600 77  A211-SAKNAS                 PIC X       VALUE 'N'.                   
056700 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
056800 77  KDANMORS-WS                 PIC X(2)    VALUE SPACE.                 
056900 77  FLDIRLEV-WS                 PIC X(2)    VALUE SPACE.                 
057000 77  FLLSBOK-11-21-WS            PIC X(2)    VALUE SPACE.                 
057100 77  DR5-FLLSBOK-WS              PIC X(2)    VALUE SPACE.                 
057200 77  W-IDPTYP                    PIC X(3)    VALUE SPACE.                 
057300 77  INV-FINNS                   PIC X       VALUE 'N'.                   
057400 77  SKRIV-MOMS-ITL              PIC X       VALUE 'J'.                   
057500 77  MOMS-FRITT                  PIC X       VALUE 'N'.                   
057600 77  MINUS-SALDO-SW              PIC X       VALUE 'N'.                   
057700 77  W-IDKNOTNR-CDC              PIC S9(7)   COMP-3 VALUE +0.             
057800 77  W-IDKNOTNR-SDC              PIC S9(7)   COMP-3 VALUE +0.             
057900 77  W-IDKNOTNR-USA              PIC S9(7)   COMP-3 VALUE +0.             
058000 77  W-IDKNOTNR-CAN              PIC S9(7)   COMP-3 VALUE +0.             
058100 77  W-IDKNOTNR-JAP              PIC S9(7)   COMP-3 VALUE +0.             
058200 77  W-IDKNOTNR-AUS              PIC S9(7)   COMP-3 VALUE +0.             
058300 77  W-IDKNOTNR-ITL              PIC S9(7)   COMP-3 VALUE +0.             
058400 77  W-IDKNOTNR-SE               PIC S9(7)   COMP-3 VALUE +0.             
058500 77  W-IDKNOTNR-NO               PIC S9(7)   COMP-3 VALUE +0.             
058600 77  W-IDKNOTNR-BE               PIC S9(7)   COMP-3 VALUE +0.             
058700 77  WS-DIFF                     PIC S9(7)   COMP-3 VALUE +0.             
058800 77  DUMMY-IDARTNR               PIC S9(9)   VALUE +100 COMP-3.           
058900 77  WS-IDRADNR-VO               PIC S9(3)   VALUE +0 COMP-3.             
059000 77  WS-KVRETINL                 PIC S9(7)   VALUE +0 COMP-3.             
059100 77  WS-KVRADER-RT               PIC S9(5)   VALUE +0 COMP-3.             
059200 77  WS-KVRADER-72               PIC S9(5)   VALUE +0 COMP-3.             
059300 77  WS-KVRADER-98               PIC S9(5)   VALUE +0 COMP-3.             
059400 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
059500 77  WS-KVLEVART                 PIC S9(7)   VALUE +0 COMP-3.             
059600 77  WS-PRARTBTO-SEK             PIC 9(7)V9(2) VALUE 0 COMP-3.            
059700 77  W-DATUM                     PIC 9(8).                                
059800 77  W-DATUM-LOC                 PIC 9(8).                                
059900 77  SPAR-TIFAKT-PULS            PIC 9(6).                                
060000 77  GODK-IX                     PIC S9(3)  VALUE +0   COMP-3.            
060100 77  MAX-GODK-IX                 PIC S9(3)  VALUE +5   COMP-3.            
060200 77  WDB2-IX                     PIC S9(3)  VALUE +0   COMP-3.            
060300 77  IX-DCCLEAR-MAX              PIC S9(3)  VALUE +99  COMP-3.            
060400 77  WC-KDANMORS                 PIC X(8)   VALUE 'KDANMORS'.             
060500 77  WC-IDFKNGRP                 PIC X(8)   VALUE 'IDFKNGRP'.             
060600 77  WC-IDARTNR                  PIC X(8)   VALUE 'IDARTNR '.             
060700 77  WC-IDDC-EXCP                PIC X(8)   VALUE 'IDDC    '.             
060800                                                                          
060900 77  WS-FLLSBOK                  PIC X       VALUE SPACE.                 
061000 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
061100 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
061200                                                                          
061300 77  SKRIV-HUV-RETILL-SW         PIC X       VALUE 'J'.                   
061400     88  SKRIV-HUV-RETILL                    VALUE 'J'.                   
061500                                                                          
061600 77  SKRIV-HUV-EKOFIL-SW         PIC X       VALUE 'J'.                   
061700     88  SKRIV-HUV-EKOFIL                    VALUE 'J'.                   
061800                                                                          
061900 77  SKRIV-HUV-KNOTA-CDC-SW      PIC X       VALUE 'J'.                   
062000     88  SKRIV-HUV-KNOTA-CDC                 VALUE 'J'.                   
062100                                                                          
062200 77  SKRIV-HUV-KNOTA-SDC-SW      PIC X       VALUE 'J'.                   
062300     88  SKRIV-HUV-KNOTA-SDC                 VALUE 'J'.                   
062400                                                                          
062500 77  SKRIV-HUV-KNOTA-SE-SW       PIC X       VALUE 'J'.                   
062600     88  SKRIV-HUV-KNOTA-SE                  VALUE 'J'.                   
062700                                                                          
062800 77  SKRIV-HUV-KNOTA-NO-SW       PIC X       VALUE 'J'.                   
062900     88  SKRIV-HUV-KNOTA-NO                  VALUE 'J'.                   
063000                                                                          
063100 77  SKRIV-HUV-KNOTA-BE-SW       PIC X       VALUE 'J'.                   
063200     88  SKRIV-HUV-KNOTA-BE                  VALUE 'J'.                   
063300                                                                          
063400 77  SKRIV-BYTES-SW              PIC X       VALUE 'N'.                   
063500     88  SKRIV-BYTES-OK                      VALUE 'J'.                   
063600                                                                          
063700 77  RETILL-FINNS-SW             PIC X       VALUE 'N'.                   
063800     88  RETILL-FINNS                        VALUE 'J'.                   
063900                                                                          
064000 77  RET-DDI-EXIT-SW             PIC X       VALUE 'N'.                   
064100     88  RET-DDI-EXIT                        VALUE 'J'.                   
064200                                                                          
064300 77  FARLIGT-GODS-SW             PIC X       VALUE 'N'.                   
064400     88  FARLIGT-GODS-FINNS                  VALUE 'J'.                   
064500                                                                          
064600 77  TILLAEGGSKOSTNADER-SW       PIC X       VALUE 'N'.                   
064700     88  TILLAEGGSK-SKRIVNA                  VALUE 'J'.                   
064800                                                                          
064900 77  TKOST-SKRIVNA-FOERUT-SW     PIC X       VALUE 'N'.                   
065000     88  TKOST-SKRIVNA-FOERUT                VALUE 'J'.                   
065100                                                                          
065200 77  RETUR-ITL-SW                PIC X       VALUE 'N'.                   
065300     88  RETUR-ITL                           VALUE 'J'.                   
065400                                                                          
065500 77  SKAPA-001-SW                PIC X       VALUE 'N'.                   
065600     88 001-POST-SKAPAD                      VALUE 'J'.                   
065700                                                                          
065800 77  BARA-N7X-SW                PIC X       VALUE 'N'.                    
065900     88 BARA-N7X-RADER                      VALUE 'J'.                    
066000                                                                          
066100 77  KLASS-1-SW                 PIC X       VALUE 'N'.                    
066200     88 KLASS-1-OK                          VALUE 'J'.                    
066300                                                                          
066400 77  KLASS-0-SW                 PIC X       VALUE 'N'.                    
066500     88 KLASS-0-OK                          VALUE 'J'.                    
066600                                                                          
066700 77  KNOTA-VIA-BILLIT-SW        PIC X       VALUE 'N'.                    
066800     88 KNOTA-VIA-BILLIT                    VALUE 'J'.                    
066900                                                                          
067000 77  AENDRA-IDDC-RET-SW         PIC X       VALUE 'N'.                    
067100     88 AENDRA-IDDC-RET                     VALUE 'J'.                    
067200                                                                          
067300 77  HANDLING-FEE-72-98-SW      PIC X       VALUE 'N'.                    
067400     88 HANDLING-FEE-72-98                  VALUE 'J'.                    
067500                                                                          
067600 77  GODK-KOD-SW                PIC X.                                    
067700     88  GODK-KOD                           VALUE 'J'.                    
067800     88  EJ-GODK-KOD                        VALUE 'N'.                    
067900     EJECT                                                                
068000                                                                          
068100 77  GODK-ARTIKEL-SW            PIC X.                                    
068200     88  GODK-ARTIKEL                       VALUE 'J'.                    
068300     88  EJ-GODK-ARTIKEL                    VALUE 'N'.                    
068400     EJECT                                                                
068500                                                                          
068600 77  GODK-IDFKNGRP-SW           PIC X.                                    
068700     88  GODK-IDFKNGRP                      VALUE 'J'.                    
068800     88  EJ-GODK-IDFKNGRP                   VALUE 'N'.                    
068900     EJECT                                                                
069000                                                                          
069100 77  GODK-DC-ARTIKEL-SW         PIC X.                                    
069200     88  GODK-DC-ARTIKEL                    VALUE 'J'.                    
069300     88  EJ-GODK-DC-ARTIKEL                 VALUE 'N'.                    
069400     EJECT                                                                
069500                                                                          
069600 77  GODK-DC-LEV-SW             PIC X.                                    
069700     88  GODK-DC-LEV                        VALUE 'J'.                    
069800     88  EJ-GODK-DC-LEV                     VALUE 'N'.                    
069900     EJECT                                                                
070000*                                                                         
070100 01  FILLER                      PIC X(08)   VALUE 'WWKUND16'.            
070200*    -COPY WWKUND16.                                                      
070300                                                                          
070400     EJECT                                                                
070500                                                                          
070600*      --- VALID IDDC CODES                                               
070700*                                                                         
070800*01  -COPY WWDC99                                                         
070900*01  -COPY WWDC99 -PRE MOMS-                                              
071000*01  -COPY WWDCKONS                                                       
071100*01  -COPY WWDCLAND                                                       
071200*01  -COPY WWLANDX2                                                       
071300       EJECT                                                              
071400                                                                          
071500                                                                          
071600 01  INVENTERINGSGRAENSER.                                                
071700     03  WS-INVVARDE             PIC S9(5)   COMP-3.                      
071800                                                                          
071900 01  WS-KOMMENTAR.                                                        
072000     03  INV-K-KDANMORS          PIC X(2).                                
072100     03  FILLER                  PIC X(2).                                
072200     03  INV-K-KVLEVANM          PIC 9(7).                                
072300     03  FILLER                  PIC X.                                   
072400     03  INV-K-IDDISTR           PIC 9(5).                                
072500     03  FILLER                  PIC X.                                   
072600     03  INV-K-IDKUNDNR          PIC 9(7).                                
072700                                                                          
072800 01  BERAEKNINGSFAELT.                                                    
072900     03  WS-MINSKNING            PIC S9(7)   COMP-3  VALUE +0.            
073000     03  WS-KVBYTANT-INL         PIC S9(7)   COMP-3  VALUE +0.            
073100     03  WS-KVKREANT             PIC S9(7)   COMP-3  VALUE +0.            
073200     03  WS-REOMRTAL             PIC S99V999 COMP-3  VALUE +0.            
073300     03  WS-KVJUST               PIC S9(7)   COMP-3.                      
073400                                                                          
073500 01  KONSTANTER.                                                          
073600     03  GEN-IDSTATNR            PIC S9(9) COMP-3 VALUE 87089997.         
073700                                                                          
073800 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
073900 01  FILLER REDEFINES DAGENS-DATUM-SEKEL.                                 
074000     03  DAGENS-DATUM-SEK        PIC 9(2).                                
074100     03  DAGENS-DATUM-AAR-SEK    PIC 9(2).                                
074200     03  DAGENS-DATUM-MAANAD-SEK PIC 9(2).                                
074300     03  DAGENS-DATUM-DAG-SEK    PIC 9(2).                                
074400                                                                          
074500                                                                          
074600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
074700 01  FILLER REDEFINES DAGENS-DATUM.                                       
074800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
074900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
075000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
075100                                                                          
075200 01  DAGENS-DATUM-VVD.                                                    
075300     03  DAGENS-DATUM-VV         PIC 9(2).                                
075400     03  DAGENS-DATUM-D          PIC 9(1).                                
075500                                                                          
075600 01  W-PRARTBTO                  PIC 9(7)V9(2).                           
075700 01  FILLER REDEFINES W-PRARTBTO.                                         
075800    03  W-PRARTBTO-HEL           PIC 9(7).                                
075900    03  W-PRARTBTO-DEC           PIC 9(2).                                
076000                                                                          
076100 01  W-PRARTBTO-X.                                                        
076200    03  W-PRARTBTO-X-HEL         PIC X(7).                                
076300    03  FILLER                   PIC X(1)    VALUE '.'.                   
076400    03  W-PRARTBTO-X-DEC         PIC X(2).                                
076500                                                                          
076600 01  W-PRARTBTO-LOC              PIC 9(7)V9(2).                           
076700 01  FILLER REDEFINES W-PRARTBTO-LOC.                                     
076800    03  W-PRARTBTO-HEL-LOC       PIC 9(7).                                
076900    03  W-PRARTBTO-DEC-LOC       PIC 9(2).                                
077000                                                                          
077100 01  W-PRARTBTO-X-LOC.                                                    
077200    03  W-PRARTBTO-X-HEL-LOC     PIC X(7).                                
077300    03  FILLER                   PIC X(1)    VALUE '.'.                   
077400    03  W-PRARTBTO-X-DEC-LOC     PIC X(2).                                
077500                                                                          
077600     EJECT                                                                
077700                                                                          
077800 01  WS-IDDISTR                  PIC 9(5).                                
077900 01  FILLER REDEFINES WS-IDDISTR.                                         
078000    03                           PIC X(1).                                
078100    03  WS-IDDISTR-ALFA          PIC X(4).                                
078200                                                                          
078300 01  WS-IDKUNDNR                 PIC 9(7).                                
078400 01  FILLER REDEFINES WS-IDKUNDNR.                                        
078500    03                           PIC X(1).                                
078600    03  WS-IDKUNDNR-ALFA         PIC X(6).                                
078700                                                                          
078800 01  WS-IDRAPPNR                 PIC 9(7).                                
078900 01  FILLER REDEFINES WS-IDRAPPNR.                                        
079000    03  WS-IDRAPPNR-ALFA         PIC X(7).                                
079100                                                                          
079200 01  WS-KVLEVANM-BEKR            PIC 9(7).                                
079300 01  FILLER REDEFINES WS-KVLEVANM-BEKR.                                   
079400    03                           PIC X(1).                                
079500    03  WS-KVLEVANM-BEKR-ALFA    PIC X(6).                                
079600                                                                          
079700     EJECT                                                                
079800                                                                          
079900 01  DYNAMISKA-SUBPROGRAM.                                                
080000*                                                                         
080100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
080200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
080300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
080400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
080500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
080600     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
080700     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
080800     03  W009MOMS                PIC X(8)    VALUE 'W009MOMS'.            
080900     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
081000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
081100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
081200     SKIP2                                                                
081300*    --- PARAMETRAR TILL ABEND                                            
081400                                                                          
081500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
081600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
081700     SKIP2                                                                
081800 01  FELTEXT.                                                             
081900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT '.            
082000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
082100     SKIP2                                                                
082200 01  W-KDKREBEH             PIC X(3).                                     
082300 01  FILLER  REDEFINES  W-KDKREBEH.                                       
082400     03  W-BOKST            PIC X(1).                                     
082500     03  W-KOD              PIC 9(2).                                     
082600     EJECT                                                                
082700 01  FILLER              PIC X(16) VALUE 'MOMS-PARAMETRAR '.              
082800*01  -COPY W009MOMS -PRE W009-.                                           
082900     EJECT                                                                
083000 01  FILLER              PIC X(16) VALUE 'W460DIS1-PARAM. '.              
083100*01  -COPY W460DIS1                                                       
083200     EJECT                                                                
083300*01  -COPY W460LISO                                                       
083400     EJECT                                                                
083500*    --- PARAMETRAR TILL W009CIA                                          
083600*01  -COPY W009CIA                                                        
083700     EJECT                                                                
083800*01  -COPY W510CURR                                                       
083900     EJECT                                                                
084000                                                                          
084100 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
084200                                                                          
084300 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
084400*01  FILLER   -COPY WWDIST03   -RED TEST-IDDISTR.                         
084500     EJECT                                                                
084600*01  FILLER   -COPY WWDIST07   -RED TEST-IDDISTR.                         
084700     EJECT                                                                
084800*01  FILLER   -COPY WWDIST16   -RED TEST-IDDISTR.                         
084900     EJECT                                                                
085000*01  FILLER   -COPY WWDIST18   -RED TEST-IDDISTR.                         
085100     EJECT                                                                
085200*01  FILLER   -COPY WWDIST34   -RED TEST-IDDISTR.                         
085300     EJECT                                                                
085400*01  FILLER   -COPY WWDIST35   -RED TEST-IDDISTR.                         
085500     EJECT                                                                
085600*01  FILLER   -COPY WWDIST79   -RED TEST-IDDISTR.                         
085700     EJECT                                                                
085800*01  FILLER   -COPY WWDIST42   -RED TEST-IDDISTR.                         
085900     EJECT                                                                
086000                                                                          
086100*01  -COPY WWIDFTG                                                        
086200     EJECT                                                                
086300                                                                          
086400*    --- PARAMETRAR TILL DATKORT                                          
086500*                                                                         
086600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41830'.              
086700     SKIP2                                                                
086800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
086900     SKIP2                                                                
087000*01  -COPY WDATKORT                                                       
087100     EJECT                                                                
087200*    --- PARAMETRAR TILL POSTSUM                                          
087300*                                                                         
087400*01  -COPY W0005   -PRE  POSTSUM-                                         
087500     EJECT                                                                
087600 01  IN-AREA-START               PIC X(24)   VALUE                        
087700                                 'IN-AREA-START  '.                       
087800     SKIP2                                                                
087900                                                                          
088000*01  AREA -COPY W41801     -PRE IN-                                       
088100     EJECT                                                                
088200 01  UT-AREA-START               PIC X(24)   VALUE                        
088300                                 'UT-AREA-START  '.                       
088400     SKIP2                                                                
088500                                                                          
088600*01  AREA -COPY W41801     -PRE UT-                                       
088700     EJECT                                                                
088800 01  UT33-AREA-START             PIC X(24)   VALUE                        
088900                                 'UT33-AREA-START  '.                     
089000     SKIP2                                                                
089100 01  UT33-AREA.                                                           
089200*   03  FILLER -COPY W41833   -PRE 720-                                   
089300     EJECT                                                                
089400 01  UT35-AREA-START             PIC X(24)   VALUE                        
089500                                 'UT35-AREA-START  '.                     
089600     SKIP2                                                                
089700 01  UT35-AREA.                                                           
089800     03  UT35-AREA-0.                                                     
089900       05  UT35-IDPTYP           PIC X(3).                                
090000       05  FILLER                PIC X(297).                              
090100*   03  FILLER -COPY W418712  -PRE 712-  -RED  UT35-AREA-0                
090200*   03  FILLER -COPY W418713  -PRE 713-  -RED  UT35-AREA-0                
090300*   03  FILLER -COPY W418717  -PRE 717-  -RED  UT35-AREA-0                
090400*   03  FILLER -COPY W418718  -PRE 718-  -RED  UT35-AREA-0                
090500     EJECT                                                                
090600 01  UT36-AREA-START             PIC X(24)   VALUE                        
090700                                 'UT36-AREA-START  '.                     
090800     SKIP2                                                                
090900                                                                          
091000 01  UT36-AREA.                                                           
091100     03  UT36-AREA-0.                                                     
091200       05  UT36-IDPTYP           PIC X(3).                                
091300       05  FILLER                PIC X(77).                               
091400*   03  FILLER -COPY W418DR5  -PRE DR5-  -RED  UT36-AREA-0                
091500     EJECT                                                                
091600 01  UT37-AREA-START             PIC X(24)   VALUE                        
091700                                 'UT37-AREA-START  '.                     
091800     SKIP2                                                                
091900                                                                          
092000*01  AREA -COPY W461021     -PRE UT37-                                    
092100     EJECT                                                                
092200 01  UT39-AREA-START             PIC X(24)   VALUE                        
092300                                 'UT39-AREA-START  '.                     
092400     SKIP2                                                                
092500                                                                          
092600*01  AREA -COPY W330099     -PRE UT39-                                    
092700     EJECT                                                                
092800 01  UT3A-AREA-START             PIC X(24)   VALUE                        
092900                                 'UT3A-AREA-START  '.                     
093000     SKIP2                                                                
093100                                                                          
093200*01  RKD-AREA -COPY W461RKDN                                              
093300     EJECT                                                                
093400 01  UT3B-AREA-START             PIC X(24)   VALUE                        
093500                                 'UT3B-AREA-START  '.                     
093600     SKIP2                                                                
093700                                                                          
093800*01  RKE-AREA -COPY W461RKEN                                              
093900                                                                          
094000     EJECT                                                                
094100 01  UT34-AREA-START             PIC X(24)   VALUE                        
094200                                 'UT34-AREA-START  '.                     
094300     SKIP2                                                                
094400 01  UT34-AREA.                                                           
094500     03  UT34-AREA-0.                                                     
094600       05  UT34-AREA-IDPTYP      PIC X(3).                                
094700       05  FILLER                PIC X(150).                              
094800*   03  FILLER -COPY W41834  -PRE UT34-  -RED  UT34-AREA-0                
094900     EJECT                                                                
095000 01  UT3C-AREA-START             PIC X(24)   VALUE                        
095100                                 'UT3C-AREA-START  '.                     
095200     SKIP2                                                                
095300                                                                          
095400*01  AREA -COPY W418003A    -PRE UT3C-                                    
095500                                                                          
095600     EJECT                                                                
095700 01  UT3D-AREA-START             PIC X(24)   VALUE                        
095800                                 'UT3D-AREA-START  '.                     
095900     SKIP2                                                                
096000                                                                          
096100*01  AREA -COPY W418003A    -PRE UT3D-                                    
096200                                                                          
096300     EJECT                                                                
096400 01  UT3E-AREA-START             PIC X(24)   VALUE                        
096500                                 'UT3E-AREA-START  '.                     
096600     SKIP2                                                                
096700                                                                          
096800*01  AREA -COPY W418003A    -PRE UT3E-                                    
096900                                                                          
097000     EJECT                                                                
097100 01  UT3F-AREA-START             PIC X(24)   VALUE                        
097200                                 'UT3F-AREA-START  '.                     
097300     SKIP2                                                                
097400                                                                          
097500*01  AREA -COPY W418003A    -PRE UT3F-                                    
097600                                                                          
097700     EJECT                                                                
097800 01  UT3G-AREA-START             PIC X(24)   VALUE                        
097900                                 'UT3G-AREA-START  '.                     
098000     SKIP2                                                                
098100                                                                          
098200*01  AREA -COPY W418003A    -PRE UT3G-                                    
098300                                                                          
098400     EJECT                                                                
098500 01  UT3H-AREA-START             PIC X(24)   VALUE                        
098600                                 'UT3H-AREA-START  '.                     
098700     SKIP2                                                                
098800                                                                          
098900*01  AREA -COPY W4183H      -PRE UT3H-                                    
099000                                                                          
099100     EJECT                                                                
099200 01  UT3I-AREA-START             PIC X(24)   VALUE                        
099300                                 'UT3I-AREA-START  '.                     
099400     SKIP2                                                                
099500                                                                          
099600*01  AREA -COPY W418003A    -PRE UT3I-                                    
099700                                                                          
099800     EJECT                                                                
099900 01  UT3J-AREA-START             PIC X(24)   VALUE                        
100000                                 'UT3J-AREA-START  '.                     
100100     SKIP2                                                                
100200                                                                          
100300*01  AREA -COPY W418003A    -PRE UT3J-                                    
100400                                                                          
100500     EJECT                                                                
100600 01  UT3K-AREA-START             PIC X(24)   VALUE                        
100700                                 'UT3K-AREA-START  '.                     
100800     SKIP2                                                                
100900                                                                          
101000*01  AREA -COPY W418003A    -PRE UT3K-                                    
101100                                                                          
101200     EJECT                                                                
101300 01  UT3L-AREA-START             PIC X(24)   VALUE                        
101400                                 'UT3L-AREA-START  '.                     
101500     SKIP2                                                                
101600                                                                          
101700*01  AREA -COPY W418003A    -PRE UT3L-                                    
101800                                                                          
101900     EJECT                                                                
102000 01  UT3M-AREA-START             PIC X(24)   VALUE                        
102100                                 'UT3M-AREA-START  '.                     
102200     SKIP2                                                                
102300                                                                          
102400*01  AREA -COPY W418003A    -PRE UT3M-                                    
102500                                                                          
102600     EJECT                                                                
102700 01  UT3N-AREA-START             PIC X(24)   VALUE                        
102800                                 'UT3N-AREA-START  '.                     
102900     SKIP2                                                                
103000                                                                          
103100*01  AREA -COPY W418003A    -PRE UT3N-                                    
103200                                                                          
103300     EJECT                                                                
103400 01  UT3O-AREA-START             PIC X(24)   VALUE                        
103500                                 'UT3O-AREA-START  '.                     
103600     SKIP2                                                                
103700                                                                          
103800*01  AREA -COPY W418003A    -PRE UT3O-                                    
103900                                                                          
104000     EJECT                                                                
104100 01  UT3P-AREA-START             PIC X(24)   VALUE                        
104200                                 'UT3P-AREA-START  '.                     
104300     SKIP2                                                                
104400                                                                          
104500*01  AREA -COPY W418003A    -PRE UT3P-                                    
104600                                                                          
104700     EJECT                                                                
104800 01  UT3Q-AREA-START             PIC X(24)   VALUE                        
104900                                 'UT3Q-AREA-START  '.                     
105000     SKIP2                                                                
105100                                                                          
105200*01  AREA -COPY W418003A    -PRE UT3Q-                                    
105300                                                                          
105400     EJECT                                                                
105500 01  UT3R-AREA-START             PIC X(24)   VALUE                        
105600                                 'UT3R-AREA-START  '.                     
105700     SKIP2                                                                
105800                                                                          
105900*01  AREA -COPY W418003A    -PRE UT3R-                                    
106000                                                                          
106100     EJECT                                                                
106200 01  UT3T-AREA-START             PIC X(24)   VALUE                        
106300                                 'UT3T-AREA-START  '.                     
106400     SKIP2                                                                
106500                                                                          
106600*01  AREA -COPY W418003A    -PRE UT3T-                                    
106700                                                                          
106800     EJECT                                                                
106900 01  UT3U-AREA-START             PIC X(24)   VALUE                        
107000                                 'UT3U-AREA-START  '.                     
107100     SKIP2                                                                
107200                                                                          
107300*01  AREA -COPY W418003A    -PRE UT3U-                                    
107400                                                                          
107500     EJECT                                                                
107600 01  UT3V-AREA-START             PIC X(24)   VALUE                        
107700                                 'UT3V-AREA-START  '.                     
107800     SKIP2                                                                
107900                                                                          
108000*01  AREA -COPY W418003A    -PRE UT3V-                                    
108100                                                                          
108200     EJECT                                                                
108300 01  UT3W-AREA-START             PIC X(24)   VALUE                        
108400                                 'UT3W-AREA-START  '.                     
108500     SKIP2                                                                
108600                                                                          
108700*01  AREA -COPY W418003A    -PRE UT3W-                                    
108800                                                                          
108900     EJECT                                                                
109000 01  UT3X-AREA-START             PIC X(24)   VALUE                        
109100                                 'UT3X-AREA-START  '.                     
109200     SKIP2                                                                
109300                                                                          
109400*01  AREA -COPY W418003A    -PRE UT3X-                                    
109500                                                                          
109600     EJECT                                                                
109700 01  UT3Y-AREA-START             PIC X(24)   VALUE                        
109800                                 'UT3Y-AREA-START  '.                     
109900     SKIP2                                                                
110000                                                                          
110100*01  AREA -COPY W418003A    -PRE UT3Y-                                    
110200                                                                          
110300     EJECT                                                                
110400 01  UT3Z-AREA-START             PIC X(24)   VALUE                        
110500                                 'UT3Z-AREA-START  '.                     
110600     SKIP2                                                                
110700                                                                          
110800*01  AREA -COPY W418003A    -PRE UT3Z-                                    
110900                                                                          
111000     EJECT                                                                
111100 01  UTAA-AREA-START             PIC X(24)   VALUE                        
111200                                 'UTAA-AREA-START  '.                     
111300     SKIP2                                                                
111400                                                                          
111500*01  AREA -COPY W418003A    -PRE UTAA-                                    
111600                                                                          
111700     EJECT                                                                
111800 01  UTAB-AREA-START             PIC X(24)   VALUE                        
111900                                 'UTAB-AREA-START  '.                     
112000     SKIP2                                                                
112100                                                                          
112200*01  AREA -COPY W418003A    -PRE UTAB-                                    
112300                                                                          
112400     EJECT                                                                
112500 01  UTAC-AREA-START             PIC X(24)   VALUE                        
112600                                 'UTAC-AREA-START  '.                     
112700     SKIP2                                                                
112800                                                                          
112900*01  AREA -COPY W418003A    -PRE UTAC-                                    
113000                                                                          
113100     EJECT                                                                
113200 01  UTAD-AREA-START             PIC X(24)   VALUE                        
113300                                 'UTAD-AREA-START  '.                     
113400     SKIP2                                                                
113500                                                                          
113600*01  AREA -COPY W418003A    -PRE UTAD-                                    
113700                                                                          
113800     EJECT                                                                
113900 01  UTAE-AREA-START             PIC X(24)   VALUE                        
114000                                 'UTAE-AREA-START  '.                     
114100     SKIP2                                                                
114200                                                                          
114300*01  AREA -COPY W418003A    -PRE UTAE-                                    
114400                                                                          
114500     EJECT                                                                
114600 01  UTAF-AREA-START             PIC X(24)   VALUE                        
114700                                 'UTAF-AREA-START  '.                     
114800     SKIP2                                                                
114900                                                                          
115000*01  AREA -COPY W418003A    -PRE UTAF-                                    
115100                                                                          
115200     EJECT                                                                
115300 01  UTAH-AREA-START             PIC X(24)   VALUE                        
115400                                 'UTAH-AREA-START  '.                     
115500     SKIP2                                                                
115600                                                                          
115700*01  AREA -COPY W418003A    -PRE UTAH-                                    
115800                                                                          
115900     EJECT                                                                
116000 01  UTAG-AREA-START             PIC X(24)   VALUE                        
116100                                 'UTAG-AREA-START  '.                     
116200     SKIP2                                                                
116300                                                                          
116400*01  AREA -COPY W371FAK     -PRE UTAG-                                    
116500                                                                          
116600     EJECT                                                                
116700 01  UTAI-AREA-START             PIC X(24)   VALUE                        
116800                                 'UTAI-AREA-START  '.                     
116900     SKIP2                                                                
117000 01  UTAI-AREA.                                                           
117100     03  UTAI-AREA-0.                                                     
117200       05  UTAI-IDPTYP           PIC X(3).                                
117300       05  FILLER                PIC X(200).                              
117400*   03  FILLER -COPY W41831A  -PRE 31A-  -RED  UTAI-AREA-0                
117500*   03  FILLER -COPY W41831B  -PRE 31B-  -RED  UTAI-AREA-0                
117600                                                                          
117700     EJECT                                                                
117800 01  UT30-AREA-START             PIC X(24)   VALUE                        
117900                                 'UT30-AREA-START  '.                     
118000     SKIP2                                                                
118100 01  UT30-AREA.                                                           
118200*    03  FILLER -COPY W51060                                              
118300                                                                          
118400 01  UT32-AREA-START             PIC X(24)   VALUE                        
118500                                 'UT32-AREA-START  '.                     
118600     SKIP2                                                                
118700 01  UT32-AREA.                                                           
118800*    03  FILLER -COPY W57060 -PRE SC-                                     
118900                                                                          
119000 01  UT32A-AREA-START             PIC X(24)   VALUE                       
119100                                 'UT32A-AREA-START '.                     
119200     SKIP2                                                                
119300 01  UT32A-AREA.                                                          
119400*    03  FILLER -COPY W51560 -PRE IN-                                     
119500                                                                          
119600     EJECT                                                                
119700 01  UT43-AREA-START             PIC X(24)   VALUE                        
119800                                 'UT43-AREA-START  '.                     
119900     SKIP2                                                                
120000 01  UT43-AREA.                                                           
120100*    03  FILLER -COPY W41843  -PRE VIR-                                   
120200                                                                          
120300     EJECT                                                                
120400     EJECT                                                                
120500 01  UTAN-AREA-START             PIC X(24)   VALUE                        
120600                                 'UTAN-AREA-START  '.                     
120700 01  UTAN-AREA.                                                           
120800     03 UTAN-IDDISTR        PIC Z(5) VALUE ZERO.                          
120900     03 FILLER              PIC X(1)  VALUE ';'.                          
121000     03 UTAN-IDKUNDNR       PIC Z(7) VALUE ZERO.                          
121100     03 FILLER              PIC X(1)  VALUE ';'.                          
121200     03 UTAN-IDRAPPNR       PIC Z(7) VALUE ZERO.                          
121300     03 FILLER              PIC X(1)  VALUE ';'.                          
121400     03 UTAN-KDVALISO       PIC X(3) VALUE SPACE.                         
121500     03 FILLER              PIC X(1)  VALUE ';'.                          
121600     03 UTAN-IDARTNR        PIC Z(9) VALUE ZERO.                          
121700     03 FILLER              PIC X(1)  VALUE ';'.                          
121800     03 UTAN-IDDC           PIC X(2) VALUE SPACE.                         
121900     03 FILLER              PIC X(1)  VALUE ';'.                          
122000     03 UTAN-IDDC-RET       PIC X(2) VALUE SPACE.                         
122100     03 FILLER              PIC X(1)  VALUE ';'.                          
122200     03 UTAN-IDFAKT         PIC Z(7) VALUE ZERO.                          
122300     03 FILLER              PIC X(1)  VALUE ';'.                          
122400     03 UTAN-IDFAKT-LOC     PIC Z(7) VALUE ZERO.                          
122500     03 FILLER              PIC X(1)  VALUE ';'.                          
122600     03 UTAN-IDKNOTNR       PIC Z(7) VALUE ZERO.                          
122700     03 FILLER              PIC X(1)  VALUE ';'.                          
122800     03 UTAN-IDKONTO        PIC Z(11) VALUE ZERO.                         
122900     03 FILLER              PIC X(1)  VALUE ';'.                          
123000     03 UTAN-IDKST          PIC X(10) VALUE SPACE.                        
123100     03 FILLER              PIC X(1)  VALUE ';'.                          
123200     03 UTAN-IDKUNDRF       PIC X(10) VALUE SPACE.                        
123300     03 FILLER              PIC X(1)  VALUE ';'.                          
123400     03 UTAN-IDORDNR7       PIC Z(7) VALUE ZERO.                          
123500     03 FILLER              PIC X(1)  VALUE ';'.                          
123600     03 UTAN-IDLOPNRM       PIC Z(9) VALUE ZERO.                          
123700     03 FILLER              PIC X(1)  VALUE ';'.                          
123800     03 UTAN-KDANMORS       PIC X(2) VALUE SPACE.                         
123900     03 FILLER              PIC X(1)  VALUE ';'.                          
124000     03 UTAN-KVANTAL-ILI    PIC -(7) VALUE ZERO.                          
124100     03 FILLER              PIC X(1)  VALUE ';'.                          
124200     03 UTAN-KVAVV-KVAL     PIC -(7) VALUE ZERO.                          
124300     03 FILLER              PIC X(1)  VALUE ';'.                          
124400     03 UTAN-KVAVV-KVANT    PIC -(7) VALUE ZERO.                          
124500     03 FILLER              PIC X(1)  VALUE ';'.                          
124600     03 UTAN-KVLEVANM       PIC -(7) VALUE ZERO.                          
124700     03 FILLER              PIC X(1)  VALUE ';'.                          
124800     03 UTAN-KVLEVANM-BEKR  PIC -(7) VALUE ZERO.                          
124900     03 FILLER              PIC X(1)  VALUE ';'.                          
125000     03 UTAN-KVRETINL       PIC -(7) VALUE ZERO.                          
125100     03 FILLER              PIC X(1)  VALUE ';'.                          
125200     03 UTAN-KVRETINL-SKR   PIC -(7) VALUE ZERO.                          
125300     03 FILLER              PIC X(1)  VALUE ';'.                          
125400     03 UTAN-PRARTBTO       PIC -(7).9(2) VALUE ZERO.                     
125500     03 FILLER              PIC X(1)  VALUE ';'.                          
125600     03 UTAN-PRARTBTO-LOC   PIC -(7).9(2) VALUE ZERO.                     
125700     03 FILLER              PIC X(1)  VALUE ';'.                          
125800     03 UTAN-PRFRAKT        PIC -(7).9(2) VALUE ZERO.                     
125900     03 FILLER              PIC X(1)  VALUE ';'.                          
126000     03 UTAN-TIFAKT         PIC Z(7) VALUE ZERO.                          
126100     03 FILLER              PIC X(1)  VALUE ';'.                          
126200     03 UTAN-TIFAKT-LOC     PIC Z(7) VALUE ZERO.                          
126300     03 FILLER              PIC X(1)  VALUE ';'.                          
126400     03 UTAN-KDVAT          PIC X(2) VALUE SPACE.                         
126500     03 FILLER              PIC X(1)  VALUE ';'.                          
126600     03 UTAN-BEART-VIPS     PIC X(25) VALUE SPACE.                        
126700     03 FILLER              PIC X(1)  VALUE ';'.                          
126800                                                                          
126900 01  UTAP-AREA-START             PIC X(24)   VALUE                        
127000                                 'UTAP-AREA-START  '.                     
127100                                                                          
127200*01  AREA -COPY W418003A    -PRE UTAP-                                    
127300                                                                          
127400     EJECT                                                                
127500 01  UTAQ-AREA-START             PIC X(24)   VALUE                        
127600                                 'UTAQ-AREA-START  '.                     
127700                                                                          
127800*01  AREA -COPY W418003A    -PRE UTAQ-                                    
127900                                                                          
128000     EJECT                                                                
128100 01  UTAS-AREA-START             PIC X(24)   VALUE                        
128200                                 'UTAS-AREA-START  '.                     
128300                                                                          
128400*01  AREA -COPY W418003A    -PRE UTAS-                                    
128500                                                                          
128600     EJECT                                                                
128700 01  UTAT-AREA-START             PIC X(24)   VALUE                        
128800                                 'UTAT-AREA-START  '.                     
128900                                                                          
129000*01  AREA -COPY W418FEE     -PRE UTAT-                                    
129100                                                                          
129200     EJECT                                                                
129300 01  UTC1-AREA-START             PIC X(24)   VALUE                        
129400                                 'UTC1-AREA-START  '.                     
129500                                                                          
129600*01  AREA -COPY W418003A    -PRE UTC1-                                    
129700                                                                          
129800     EJECT                                                                
129900 01  UTIN-AREA-START             PIC X(24)   VALUE                        
130000                                 'UTIN-AREA-START  '.                     
130100                                                                          
130200*01  AREA -COPY W418003A    -PRE UTIN-                                    
130300                                                                          
130400     EJECT                                                                
130500 01  UTCZ-AREA-START             PIC X(24)   VALUE                        
130600                                 'UTCZ-AREA-START  '.                     
130700                                                                          
130800*01  AREA -COPY W418003A    -PRE UTCZ-                                    
130900                                                                          
131000     EJECT                                                                
131100 01  UTHU-AREA-START             PIC X(24)   VALUE                        
131200                                 'UTHU-AREA-START  '.                     
131300                                                                          
131400*01  AREA -COPY W418003A    -PRE UTHU-                                    
131500                                                                          
131600     EJECT                                                                
131700 01  UTMQ-AREA-START             PIC X(24)   VALUE                        
131800                                 'UTMQ-AREA-START  '.                     
131900                                                                          
132000*01  AREA -COPY W418003B    -PRE UTMQ-                                    
132100     EJECT                                                                
132200                                                                          
132300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
132400*                                                                         
132500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
132600     SKIP3                                                                
132700 01  NYCKLAR-TILL-DLI.                                                    
132800     03  W-WDA2A1KY-MIN-X.                                                
132900         05  W-KDLEVANM-MIN      PIC  X(1)   VALUE SPACE.                 
133000         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
133100                                                                          
133200     03  W-WDA2A1KY-MAX-X.                                                
133300         05  W-KDLEVANM-MAX      PIC  X(1)   VALUE SPACE.                 
133400         05  FILLER              PIC X(16)   VALUE HIGH-VALUE.            
133500                                                                          
133600     03  W-IDLEVANM-X.                                                    
133700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
133800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
133900         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
134000                                                                          
134100     03  W-WDA211KY-X.                                                    
134200         05  W-IDARTNR-A211      PIC S9(9)   VALUE ZERO COMP-3.           
134300         05  W-IDRADNR-A211      PIC S9(5)   VALUE ZERO COMP-3.           
134400                                                                          
134500     03  W-KDSEGKEY-X.                                                    
134600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
134700                                                                          
134800     03  W-IDARTNR-X.                                                     
134900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
135000                                                                          
135100     03  W-IDARTNR-K7-X.                                                  
135200         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
135300                                                                          
135400     03  W-IDDC-X.                                                        
135500         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
135600                                                                          
135700     03  W-IDLAND-X.                                                      
135800         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
135900                                                                          
136000     03  W-KDLEVATT-X.                                                    
136100         05  W-KDLEVATT          PIC 9    VALUE 1.                        
136200                                                                          
136300     03 W-WDB301-KEY.                                                     
136400        05 W-IDDC-WDB3           PIC  X(2).                               
136500        05 W-IDDISTR-WDB3        PIC S9(5)    COMP-3.                     
136600        05 W-IDKUNDNR-WDB3       PIC S9(7)    COMP-3.                     
136700                                                                          
136800     03 W-WDB301-KEY-DEF.                                                 
136900        05 W-IDDC-WDB3-DEF       PIC  X(2).                               
137000        05 W-IDDISTR-WDB3-DEF    PIC S9(5)    COMP-3.                     
137100        05 W-IDKUNDNR-WDB3-DEF PIC S9(7)   VALUE +9999999 COMP-3.         
137200                                                                          
137300     03 W-IDGMT-X.                                                        
137400        05 W-IDDISTR-WDB2        PIC S9(5)    COMP-3.                     
137500        05 W-IDKUNDNR-WDB2       PIC S9(7)    COMP-3.                     
137600                                                                          
137700     03 W-IDGMT-MIN-X.                                                    
137800        05 W-IDDISTR-WDB2-MIN   PIC S9(5)    COMP-3.                      
137900        05 W-IDKUNDNR-WDB2-MIN  PIC S9(7)    COMP-3.                      
138000                                                                          
138100     03 W-IDGMT-MAX-X.                                                    
138200        05 W-IDDISTR-WDB2-MAX   PIC S9(5)    COMP-3.                      
138300        05 W-IDKUNDNR-WDB2-MAX  PIC S9(7)    COMP-3.                      
138400                                                                          
138500                                                                          
138600     03 W-WDB101KY-X.                                                     
138700        05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                  
138800        05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                   
138900                                                                          
139000        EJECT                                                             
139100     03 W-IDFAKT-X.                                                       
139200       05 W-IDFAKT              PIC S9(7)   COMP-3 VALUE ZERO.            
139300                                                                          
139400     03 W-IDGMTREF-X.                                                     
139500       05 W-IDDISTR-L5          PIC S9(5)   COMP-3 VALUE ZERO.            
139600       05 W-IDKUNDNR-L5         PIC S9(7)   COMP-3 VALUE ZERO.            
139700       05 W-IDKUNDRF-L5         PIC X(10).                                
139800                                                                          
139900     03 W-WDL511KY-X.                                                     
140000       05 W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.           
140100       05 W-IDKOLLI-X.                                                    
140200         07 W-IDKOLLI-L5        PIC S9(5)   VALUE ZERO  COMP-3.           
140300                                                                          
140400     03 W-WDQ2C1KY-X.                                                     
140500        05  W-SEQC-IDDISTR      PIC S9(5)   VALUE +0 COMP-3.              
140600        05  W-SEQC-IDKUNDNR     PIC S9(7)   VALUE +0 COMP-3.              
140700        05  W-SEQC-IDKUNDRF.                                              
140800          07  W-SEQC-IDORDNR7   PIC 9(7)    VALUE ZERO.                   
140900          07  FILLER            PIC X(3)    VALUE SPACE.                  
141000                                                                          
141100     03 W-WDGXKEY-4103-X.                                                 
141200        05  W-IDHTYP-4103       PIC X(4)    VALUE '4103'.                 
141300        05  W-IDDISTR-4103      PIC S9(5)   VALUE ZERO COMP-3.            
141400        05  W-IDKUNDNR-4103     PIC S9(7)   VALUE ZERO COMP-3.            
141500        05  W-IDRAPPNR-4103     PIC  9(7)   VALUE ZERO.                   
141600        05  FILLER              PIC X(12)   VALUE LOW-VALUE.              
141700                                                                          
141800     03 W-WDGXKEY-4104-X.                                                 
141900        05  W-IDDC-4104         PIC X(2)    VALUE SPACE.                  
142000        05  W-KDKRENOT-4104     PIC X(2)    VALUE SPACE.                  
142100                                                                          
142200     03 W-IDDC-B6-RET-X.                                                  
142300        05 W-IDDC-B6-RET        PIC X(2)    VALUE SPACE.                  
142400                                                                          
142500     03  W-IDDC-B6-X.                                                     
142600        05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                  
142700                                                                          
142800     03  W-WDB611KY-X.                                                    
142900        05  W-URV-TEELMT            PIC X(16)  VALUE SPACE.               
143000        05  W-URV-FILLER            PIC X(20)  VALUE SPACE.               
143100        05  W-URV-IDARTNR-EXCP-FILLER REDEFINES W-URV-FILLER.             
143200          07  W-URV-IDARTNR-EXCP    PIC 9(9).                             
143300          07  FILLER                PIC X(11).                            
143400        05  W-URV-IDFKNGRP-EXCP-FILLER REDEFINES W-URV-FILLER.            
143500          07  W-URV-IDFKNGRP-EXCP   PIC 9(4).                             
143600          07  FILLER                PIC X(16).                            
143700        05  W-URV-KDANMORS-RET-FILLER REDEFINES W-URV-FILLER.             
143800          07  W-URV-KDANMORS-RET    PIC X(2).                             
143900          07  FILLER                PIC X(18).                            
144000        05  W-URV-IDDC-EXCP-FILLER REDEFINES W-URV-FILLER.                
144100          07  W-URV-IDDC-EXCP       PIC X(2).                             
144200          07  FILLER                PIC X(18).                            
144300                                                                          
144400     EJECT                                                                
144500 01  W-WDH111KY-MIN.                                                      
144600     03  IDDC-SEARCH-MIN       PIC X(2).                                  
144700     03  KDINVKAT-SEARCH-MIN   PIC S9(3) VALUE ZERO       COMP-3.         
144800     03  TISEGKEY-SEARCH-MIN   PIC S9(9) VALUE ZERO       COMP-3.         
144900     03  DAREGDAT-SORT-MIN     PIC 9(8)  VALUE ZERO.                      
145000                                                                          
145100 01  W-WDH111KY-MAX.                                                      
145200     03  IDDC-SEARCH-MAX       PIC X(2).                                  
145300     03  KDINVKAT-SEARCH-MAX   PIC S9(3) VALUE +999       COMP-3.         
145400     03  TISEGKEY-SEARCH-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
145500     03  DAREGDAT-SORT-MAX     PIC 9(8)  VALUE 99999999.                  
145600                                                                          
145700     EJECT                                                                
145800                                                                          
145900 01  TEST-IDARTNR                PIC 9(9)    COMP-3.                      
146000*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR.                            
146100     EJECT                                                                
146200                                                                          
146300*    --- STATUS-KOD FRÅN IMS                                              
146400 01  STATUS-WS                   PIC XX.                                  
146500     88  SEGMENT-FINNS                       VALUE '  '.                  
146600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
146700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
146800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
146900     SKIP2                                                                
147000 01  GODK-STATUSKODER.                                                    
147100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
147200     SKIP3                                                                
147300 01  SSA1                        PIC X(128).                              
147400 01  SSA2                        PIC X(64).                               
147500 01  SSA3                        PIC X(64).                               
147600     EJECT                                                                
147700*    --- IMS FUNKTIONSKODER                                               
147800*01  -COPY W0003                                                          
147900     EJECT                                                                
148000*    ---  DLI INPUT-OUTPUT AREA                                           
148100 01  FILLER                      PIC X(24)   VALUE                        
148200                                            'DLI-IO-AREA-WDA2A1'.         
148300 01  DLI-IO-AREA-WDA2A1.                                                  
148400*    03  -COPY WDA2A1                                                     
148500     EJECT                                                                
148600 01  FILLER                      PIC X(24)   VALUE                        
148700                                            'DLI-IO-AREA-WDA201'.         
148800 01  DLI-IO-AREA-WDA201.                                                  
148900*    03  -COPY WDA201                                                     
149000     EJECT                                                                
149100 01  FILLER                      PIC X(24)   VALUE                        
149200                                            'DLI-IO-AREA-WDA211'.         
149300 01  DLI-IO-AREA-WDA211.                                                  
149400*    03  -COPY WDA211                                                     
149500     EJECT                                                                
149600 01  FILLER                      PIC X(24)   VALUE                        
149700                                            'DLI-IO-AREA-WDA221'.         
149800 01  DLI-IO-AREA-WDA221.                                                  
149900*    03  -COPY WDA221                                                     
150000     EJECT                                                                
150100 01  FILLER                      PIC X(24)   VALUE                        
150200                                            'DLI-IO-AREA-WDK601'.         
150300     SKIP3                                                                
150400 01  DLI-IO-AREA-WDK601.                                                  
150500*    03  -COPY WDK601                                                     
150600     EJECT                                                                
150700                                                                          
150800 01  FILLER                      PIC X(24)   VALUE                        
150900                                            'DLI-IO-AREA-WDK611'.         
151000     SKIP3                                                                
151100 01  DLI-IO-AREA-WDK611.                                                  
151200*    03  -COPY WDK611                                                     
151300     EJECT                                                                
151400                                                                          
151500 01  FILLER                    PIC X(16)  VALUE 'DLI-IO-WDK621'.          
151600                                                                          
151700 01  DLI-IO-WDK621.                                                       
151800*    03  -COPY WDK621                                                     
151900     EJECT                                                                
152000                                                                          
152100 01  FILLER                      PIC X(24)   VALUE                        
152200                                            'DLI-IO-AREA-WDK711'.         
152300 01  DLI-IO-AREA-WDK711.                                                  
152400*    05  -COPY WDK711                                                     
152500                                                                          
152600 01  FILLER                      PIC X(24)   VALUE                        
152700                                            'DLI-IO-AREA-WDK712'.         
152800 01  DLI-IO-AREA-WDK712.                                                  
152900*    05  -COPY WDK712                                                     
153000                                                                          
153100                                                                          
153200 01  FILLER                      PIC X(24)   VALUE                        
153300                                            'DLI-IO-AREA-WDH101'.         
153400 01  DLI-IO-AREA-WDH101.                                                  
153500     03  IO-AREA-WDH101          PIC X(10)   VALUE SPACE.                 
153600     SKIP3                                                                
153700     03  WDH101 REDEFINES IO-AREA-WDH101.                                 
153800*        05  -COPY WDH101  -PRE INV-                                      
153900     EJECT                                                                
154000 01  FILLER                      PIC X(24)   VALUE                        
154100                                            'DLI-IO-AREA-WDH111'.         
154200 01  DLI-IO-AREA-WDH111.                                                  
154300     03  IO-AREA-WDH111          PIC X(200)  VALUE SPACE.                 
154400     SKIP3                                                                
154500     03  WDH111 REDEFINES IO-AREA-WDH111.                                 
154600*        05  -COPY WDH111                                                 
154700 01  DLI-IO-AREA-WDD501.                                                  
154800     03  IO-AREA-WDD501          PIC X(200)   VALUE SPACE.                
154900     SKIP3                                                                
155000     03  WLARTN01 REDEFINES IO-AREA-WDD501.                               
155100*        05  -COPY WDD501  -PRE FG-                                       
155200 01  DLI-IO-AREA-WDB201.                                                  
155300*    03  -COPY WDB201                                                     
155400     SKIP3                                                                
155500 01  DLI-IO-AREA-WDB301.                                                  
155600     03  IO-AREA-WDB301          PIC X(100)   VALUE SPACE.                
155700     SKIP3                                                                
155800     03  WLGMTB01 REDEFINES IO-AREA-WDB301.                               
155900*        05  -COPY WDB301                                                 
156000     EJECT                                                                
156100 01  DLI-IO-AREA-WDB101.                                                  
156200*    03  -COPY WDB101                                                     
156300     EJECT                                                                
156400                                                                          
156500 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDL501'.                
156600 01  DLI-IO-WDL501.                                                       
156700*    03  -COPY WDL501                                                     
156800     EJECT                                                                
156900 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDL511'.                
157000 01  DLI-IO-WDL511.                                                       
157100*    03  -COPY WDL511                                                     
157200     EJECT                                                                
157300 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDL521'.                
157400 01  DLI-IO-WDL521.                                                       
157500*    03  -COPY WDL521                                                     
157600     EJECT                                                                
157700                                                                          
157800 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDQ2C1'.                
157900                                                                          
158000 01  DLI-IO-WDQ2C1.                                                       
158100*    03  WDQ2C1 -COPY WDQ2C1                                              
158200     EJECT                                                                
158300                                                                          
158400 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDGX4103'.              
158500 01  DLI-IO-WDGX4103.                                                     
158600*    03  -COPY WDGX4103                                                   
158700     EJECT                                                                
158800 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDGX4104'.              
158900 01  DLI-IO-WDGX4104.                                                     
159000*    03  -COPY WDGX4104                                                   
159100     EJECT                                                                
159200 01  FILLER            PIC X(16)    VALUE 'DLI-IO-WDGX4106'.              
159300 01  DLI-IO-WDGX4106.                                                     
159400*    03  -COPY WDGX4106                                                   
159500     EJECT                                                                
159600 01  FILLER            PIC X(16)   VALUE 'DLI-IO-B601-RET '.              
159700 01   DLI-IO-B601-RET.                                                    
159800*     03  -COPY WDB601 -PRE RET-                                          
159900                                                                          
160000     EJECT                                                                
160100 01  FILLER            PIC X(16)   VALUE 'DLI-IO-WDB601'.                 
160200 01   DLI-IO-WDB601.                                                      
160300*     03  -COPY WDB601                                                    
160400     EJECT                                                                
160500 01  FILLER            PIC X(16)   VALUE 'DLI-IO-WDB611'.                 
160600 01  DLI-IO-WDB611.                                                       
160700*    03  -COPY WDB611                                                     
160800     EJECT                                                                
160900                                                                          
161000*    ---  LÄNKAREA TILL W418OKOD                                          
161100     SKIP3                                                                
161200*    03 -COPY W418OKOD           -PRE OKOD-.                              
161300     EJECT                                                                
161400*    --- LÄNKAREA TILL W418ANSV                                           
161500     SKIP3                                                                
161600*01  -COPY W418ANSV                                                       
161700     EJECT                                                                
161800 LINKAGE SECTION.                                                         
161900                                                                          
162000*01  -COPY W0008  -PRE WDA2A-                                             
162100     05  FILLER                  PIC X.                                   
162200     EJECT                                                                
162300*01  -COPY W0008  -PRE KREE-                                              
162400     05  FILLER                  PIC X.                                   
162500     EJECT                                                                
162600*01  -COPY W0008  -PRE ARTC-                                              
162700     05  FILLER                  PIC X.                                   
162800     EJECT                                                                
162900*01  -COPY W0008  -PRE INVA-                                              
163000     05  FILLER                  PIC X.                                   
163100     EJECT                                                                
163200*01  -COPY W0008  -PRE 4113-                                              
163300     05  FILLER                  PIC X.                                   
163400     EJECT                                                                
163500*01  -COPY W0008  -PRE ARTN-                                              
163600     05  FILLER                  PIC X.                                   
163700     EJECT                                                                
163800*01  -COPY W0008  -PRE GMTB-                                              
163900     05  FILLER                  PIC X.                                   
164000     EJECT                                                                
164100*01  -COPY W0008  -PRE GMTA-                                              
164200     05  FILLER                  PIC X.                                   
164300     EJECT                                                                
164400*01  -COPY W0008  -PRE WDB1-                                              
164500     05  FILLER                  PIC X.                                   
164600     EJECT                                                                
164700*01  -COPY W0008  -PRE WDK7-                                              
164800     05  FILLER                  PIC X.                                   
164900     EJECT                                                                
165000*01  -COPY W0008  -PRE 4115-                                              
165100     05  FILLER                  PIC X.                                   
165200     EJECT                                                                
165300*01  -COPY W0008  -PRE 4117-                                              
165400     05  FILLER                  PIC X.                                   
165500     EJECT                                                                
165600*01  -COPY W0008  -PRE WDG2-                                              
165700     05  FILLER                  PIC X.                                   
165800     EJECT                                                                
165900*01  -COPY W0008  -PRE WDL5-                                              
166000     05  FILLER                  PIC X.                                   
166100     EJECT                                                                
166200*01  -COPY W0008  -PRE WDQ2C-                                             
166300     05  FILLER                  PIC X.                                   
166400     EJECT                                                                
166500*01  -COPY W0008   -PRE 4103-                                             
166600     05  FILLER                  PIC X.                                   
166700     EJECT                                                                
166800*01  -COPY W0008   -PRE WDB6-                                             
166900     05  FILLER                  PIC X.                                   
167000     EJECT                                                                
167100 PROCEDURE DIVISION  USING WDA2A-PCB KREE-PCB                             
167200                           ARTC-PCB INVA-PCB                              
167300                           4113-PCB ARTN-PCB                              
167400                           GMTB-PCB GMTA-PCB                              
167500                           WDB1-PCB WDK7-PCB                              
167600                           4115-PCB 4117-PCB                              
167700                           WDG2-PCB WDL5-PCB                              
167800                           WDQ2C-PCB 4103-PCB WDB6-PCB.                   
167900     ENTRY 'DLITCBL' USING WDA2A-PCB KREE-PCB                             
168000                           ARTC-PCB INVA-PCB                              
168100                           4113-PCB ARTN-PCB                              
168200                           GMTB-PCB GMTA-PCB                              
168300                           WDB1-PCB WDK7-PCB                              
168400                           4115-PCB 4117-PCB                              
168500                           WDG2-PCB WDL5-PCB                              
168600                           WDQ2C-PCB 4103-PCB WDB6-PCB.                   
168700                                                                          
168800     PERFORM A-INIT                                                       
168900     PERFORM S01-LAES-W41801                                              
169000     MOVE '3'                             TO W-KDLEVANM-MIN               
169100                                             W-KDLEVANM-MAX               
169200                                                                          
169300*    --- LÄS SAMTLIGA WDA201 VIA WDA2A1 MED KDLEVANM = 3                  
169400*    --- OCH DÄR KDLEVATT NOT = 1. ( EJ ATTESTERADE KN )                  
169500     PERFORM IMS-GU-WDA2A1-3                                              
169600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
169700        MOVE NEJ                         TO KNOTA-VIA-BILLIT-SW           
169800        PERFORM B-BEARBETA                                                
169900*FIX CO                                                                   
170000        IF A211-SAKNAS = JA                                               
170100           MOVE NEJ TO A211-SAKNAS                                        
170200           DISPLAY 'EJ-A211 B- ' ANM-IDDISTR ANM-IDKUNDNR                 
170300                                 ANM-IDRAPPNR                             
170400        ELSE                                                              
170500          MOVE ANM-IDDISTR            TO W-IDDISTR-WDB2                   
170600                                         TEST-IDDISTR                     
170700          MOVE ANM-IDKUNDNR           TO W-IDKUNDNR-WDB2                  
170800          PERFORM IMS-GET-WLGMTA01-UNIK                                   
170900          IF SEGMENT-FINNS AND                                            
171000             (GMT-FLLDCKND = JA OR GMT-FLRETUR = JA)                      
171100            IF ANM-IXDCCLEAR = 2                                          
171200              PERFORM J-KOLLA-OM-RETUR-DC-OK                              
171300              IF AENDRA-IDDC-RET                                          
171400                PERFORM K-UPPDATERA-IDDC-RET                              
171500              END-IF                                                      
171600              PERFORM L-KOLLA-RETUR-DC-WDA211                             
171700            END-IF                                                        
171800            IF ANM-IXDCCLEAR = 3                                          
171900              PERFORM L-KOLLA-RETUR-DC-WDA211                             
172000            END-IF                                                        
172100          END-IF                                                          
172200                                                                          
172300          IF RET-DDI-EXIT                                                 
172400            PERFORM O-UPPDATERA-STATUS-4                                  
172500          ELSE                                                            
172600            IF KNOTA-VIA-BILLIT                                           
172700              PERFORM I-UPPDATERA-STATUS-9                                
172800            ELSE                                                          
172900              PERFORM H-UPPDATERA-STATUS                                  
173000            END-IF                                                        
173100          END-IF                                                          
173200        END-IF                                                            
173300        PERFORM IMS-GN-WDA2A1-3                                           
173400     END-PERFORM                                                          
173500                                                                          
173600     MOVE '7'                            TO W-KDLEVANM-MIN                
173700                                            W-KDLEVANM-MAX                
173800     PERFORM IMS-GU-WDA2A1                                                
173900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
174000        MOVE NEJ                         TO                               
174100                                          TKOST-SKRIVNA-FOERUT-SW         
174200                                          TILLAEGGSKOSTNADER-SW           
174300        MOVE NEJ                         TO KNOTA-VIA-BILLIT-SW           
174400                                            AENDRA-IDDC-RET-SW            
174500                                            HANDLING-FEE-72-98-SW         
174600        PERFORM F-BEHANDLA-KRENOT-EFTER-RETUR                             
174700*FIX CO                                                                   
174800        IF A211-SAKNAS = JA                                               
174900           MOVE NEJ TO A211-SAKNAS                                        
175000           DISPLAY 'EJ-A211 F- ' ANM-IDDISTR ANM-IDKUNDNR                 
175100                                 ANM-IDRAPPNR                             
175200        ELSE                                                              
175300          IF KNOTA-VIA-BILLIT                                             
175400            PERFORM I-UPPDATERA-STATUS-9                                  
175500            IF HANDLING-FEE-72-98                                         
175600              PERFORM M-SKAPA-RADPOST-FAKT-FEE                            
175700            END-IF                                                        
175800          ELSE                                                            
175900            PERFORM H-UPPDATERA-STATUS                                    
176000          END-IF                                                          
176100        END-IF                                                            
176200        PERFORM IMS-GN-WDA2A1                                             
176300     END-PERFORM                                                          
176400                                                                          
176500     MOVE IN-AREA                         TO UT-AREA                      
176600     PERFORM S11-SKRIV-W41801                                             
176700                                                                          
176800     PERFORM Z-FINIT                                                      
176900                                                                          
177000     MOVE ZERO                            TO RETURN-CODE                  
177100     GOBACK                                                               
177200     .                                                                    
177300     EJECT                                                                
177400                                                                          
177500 A-INIT SECTION.                                                          
177600                                                                          
177700     OPEN INPUT  W41801-IN                                                
177800                                                                          
177900     OPEN OUTPUT W41801-UT                                                
178000                 W41833                                                   
178100                 W41835                                                   
178200                 W41836                                                   
178300                 W41837                                                   
178400                 W41839                                                   
178500                 W4183A                                                   
178600                 W4183B                                                   
178700                 W41834                                                   
178800                 W4183C                                                   
178900                 W4183D                                                   
179000                 W4183E                                                   
179100                 W4183F                                                   
179200                 W4183G                                                   
179300                 W4183H                                                   
179400                 W4183I                                                   
179500                 W4183J                                                   
179600                 W4183K                                                   
179700                 W4183L                                                   
179800                 W4183N                                                   
179900                 W4183O                                                   
180000                 W4183P                                                   
180100                 W4183Q                                                   
180200                 W4183R                                                   
180300                 W4183T                                                   
180400                 W4183U                                                   
180500                 W4183V                                                   
180600                 W4183W                                                   
180700                 W4183X                                                   
180800                 W4183Y                                                   
180900                 W4183Z                                                   
181000                 W418AA                                                   
181100                 W418AB                                                   
181200                 W418AC                                                   
181300                 W418AD                                                   
181400                 W418AE                                                   
181500                 W418AF                                                   
181600                 W418AG                                                   
181700                 W418AH                                                   
181800                 W418AI                                                   
181900                 W41830                                                   
182000                 W41832                                                   
182100                 W41843                                                   
182200                 W418AP                                                   
182300                 W418AQ                                                   
182400                 W418AS                                                   
182500                 W418AT                                                   
182600                 W418C1                                                   
182700                 W418AN                                                   
182800                 W41831                                                   
182900                 W418IN                                                   
183000                 W418CZ                                                   
183100                 W418HU                                                   
183200                 W418MQA                                                  
183300                                                                          
183400     MOVE LOW-VALUE                       TO W-IDLEVANM-X                 
183500                                             W-IDGMT-MIN-X                
183600                                             W-IDGMTREF-X                 
183700     MOVE HIGH-VALUE                      TO W-IDGMT-MAX-X                
183800                                                                          
183900     MOVE NEJ                             TO SKAPA-001-SW                 
184000                                                                          
184100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
184200     MOVE D-AAR                           TO DAGENS-DATUM-AAR             
184300     MOVE D-MAANAD                        TO DAGENS-DATUM-MAANAD          
184400     MOVE D-DAG                           TO DAGENS-DATUM-DAG             
184500     MOVE D-AAR                           TO W-DATE-AAMM(1:2)             
184600     MOVE D-MAANAD                        TO W-DATE-AAMM(3:2)             
184700                                                                          
184800     IF D-AAR < 50                                                        
184900        MOVE 20                           TO DAGENS-DATUM-SEK             
185000     ELSE                                                                 
185100        MOVE 19                           TO DAGENS-DATUM-SEK             
185200     END-IF                                                               
185300                                                                          
185400     MOVE D-AAR                           TO DAGENS-DATUM-AAR-SEK         
185500     MOVE D-MAANAD                        TO                              
185600                                          DAGENS-DATUM-MAANAD-SEK         
185700     MOVE D-DAG                           TO DAGENS-DATUM-DAG-SEK         
185800     MOVE D-VECKA                         TO DAGENS-DATUM-VV              
185900     MOVE D-DAGNR                         TO DAGENS-DATUM-D               
186000     MOVE IDPGM                           TO POSTSUM-PROGNAMN             
186100     MOVE W-DATE-AAMM                     TO CURR-TIAAMM                  
186200     MOVE WS-KDVALISO-HUV                 TO CURR-KDVALISO-HUV            
186300     MOVE 'M'                             TO CURR-KDVALTYP                
186400     .                                                                    
186500     EJECT                                                                
186600 B-BEARBETA SECTION.                                                      
186700                                                                          
186800     MOVE SPACE                           TO IDDC-WS                      
186900     MOVE SPACE                           TO KDANMORS-WS                  
187000     MOVE SPACE                           TO FLDIRLEV-WS                  
187100     MOVE SPACE                           TO FLLSBOK-11-21-WS             
187200     MOVE SPACE                           TO DR5-FLLSBOK-WS               
187300     MOVE ZERO                            TO WS-KVRADER-RT                
187400                                             WS-IDRADNR-VO                
187500                                             WS-KVRETINL                  
187600                                              W-IDKNOTNR-CDC              
187700                                              W-IDKNOTNR-SDC              
187800                                              W-IDKNOTNR-USA              
187900                                              W-IDKNOTNR-CAN              
188000                                              W-IDKNOTNR-JAP              
188100                                              W-IDKNOTNR-AUS              
188200                                              W-IDKNOTNR-ITL              
188300                                              W-IDKNOTNR-SE               
188400                                              W-IDKNOTNR-NO               
188500                                              W-IDKNOTNR-BE               
188600     MOVE JA                              TO                              
188700                                          SKRIV-HUV-RETILL-SW             
188800                                          SKRIV-HUV-KNOTA-CDC-SW          
188900                                          SKRIV-HUV-KNOTA-SDC-SW          
189000                                          SKRIV-HUV-KNOTA-SE-SW           
189100                                          SKRIV-HUV-KNOTA-NO-SW           
189200                                          SKRIV-HUV-KNOTA-BE-SW           
189300                                          SKRIV-HUV-EKOFIL-SW             
189400                                          SKRIV-MOMS-ITL                  
189500     MOVE NEJ                             TO                              
189600                                          RETILL-FINNS-SW                 
189700                                          RET-DDI-EXIT-SW                 
189800                                          FARLIGT-GODS-SW                 
189900                                          RETUR-ITL-SW                    
190000                                          TILLAEGGSKOSTNADER-SW           
190100                                          AENDRA-IDDC-RET-SW              
190200                                                                          
190300     MOVE SEQA-IDLEVANM                   TO W-IDLEVANM-X                 
190400     PERFORM IMS-GU-KREE-ANM                                              
190500     PERFORM IMS-GNP-KREE-LEV                                             
190600*FIX CO                                                                   
190700     IF SEGMENT-SAKNAS                                                    
190800       MOVE JA TO A211-SAKNAS                                             
190900     END-IF                                                               
191000                                                                          
191100     MOVE ANM-IDDISTR                     TO TEST-IDDISTR                 
191200                                                                          
191300     PERFORM UNTIL SEGMENT-SAKNAS                                         
191400        MOVE LEV-IDDC                     TO WS-IDDC                      
191500********FIX STOP OLD DDI                                                  
191600      IF ANM-IDDISTR = 1678       AND                                     
191700         LEV-TIFAKT-LOC > 0       AND                                     
191800         LEV-TIFAKT-LOC < 170507  AND                                     
191900         LEV-TIFAKT     = 0                                               
192000                                                                          
192100        IF (LEV-KDANMORS  = '12') OR                                      
192200           (LEV-KDANMORS  = '22') OR                                      
192300           (LEV-KDANMORS  = '27') OR                                      
192400           (LEV-KDANMORS  = '32') OR                                      
192500           (LEV-KDANMORS  = '42') OR                                      
192600           (LEV-KDANMORS  = '52') OR                                      
192700           (LEV-KDANMORS  = '54') OR                                      
192800           (LEV-KDANMORS  = '62') OR                                      
192900           (LEV-KDANMORS  = '72') OR                                      
193000           (LEV-KDANMORS  = '74') OR                                      
193100           (LEV-KDANMORS  = '75') OR                                      
193200           (LEV-KDANMORS  = '82') OR                                      
193300           (LEV-KDANMORS  = '92') OR                                      
193400           (LEV-KDANMORS  = '94') OR                                      
193500           (LEV-KDANMORS  = '98') OR                                      
193600           (LEV-KDANMORS  = '99')                                         
193700          MOVE JA TO RETILL-FINNS-SW                                      
193800          MOVE JA TO RET-DDI-EXIT-SW                                      
193900          ADD +1  TO WS-KVRADER-RT                                        
194000        ELSE                                                              
194100          MOVE JA TO A211-SAKNAS                                          
194200          PERFORM S90-SKAPA-DDI-LIST                                      
194300          PERFORM S91-UPPDATERA-STATUS-8                                  
194400        END-IF                                                            
194500********FIX STOP OLD DDI                                                  
194600                                                                          
194700      ELSE                                                                
194800        IF LEV-FLTEXT = JA                                                
194900           PERFORM S45-SKAPA-TXT-TILL-VIPS                                
195000        END-IF                                                            
195100        IF LEV-FLANNULL = NEJ                                             
195200           MOVE LEV-IDARTNR               TO W-IDARTNR                    
195300                                             W-IDARTNR-K7                 
195400                                          TEST-IDARTNR                    
195500           PERFORM IMS-GU-ARTC01                                          
195600           MOVE ART-KDSORT TO WS-KDSORT                                   
195700           PERFORM IMS-GNP-ARTC11                                         
195800           MOVE LEV-KDKREBEH              TO W-KDKREBEH                   
195900                                                                          
196000           IF W-BOKST = 'N' OR                                            
196100             (OKOD-FL-RETILL = JA AND                                     
196200                           ( LEV-KDKREBEH = 'ANN' OR 'DEL'))              
196300              PERFORM S80-SKAPA-RKD-POST                                  
196400           ELSE                                                           
196500              IF W-BOKST = 'C'                                            
196600                 IF LEV-KDKREBEH = 'C00'     AND                          
196700                   (LEV-KDANMORS = '52'      OR                           
196800                    LEV-KDANMORS = '53')                                  
196900                    CONTINUE                                              
197000                 ELSE                                                     
197100                    PERFORM S80-SKAPA-RKD-POST                            
197200                 END-IF                                                   
197300              END-IF                                                      
197400           END-IF                                                         
197500           MOVE LEV-KDANMORS              TO OKOD-KDANMORS                
197600           CALL W418OKOD USING OKOD-W418OKOD                              
197700                                                                          
197800           IF W-BOKST = 'N'                                               
197900              CONTINUE                                                    
198000           ELSE                                                           
198100              IF OKOD-FL-RETILL = JA OR                                   
198200              (OKOD-FL-INTERNUPPACKNING = JA  AND CDC-SE)                 
198300                 PERFORM C-BEHANDLA-RETILL                                
198400              ELSE                                                        
198500                 IF OKOD-FL-TF = JA                                       
198600                    PERFORM D-BEHANDLA-TILLAEGGSFAKTURA                   
198700                 ELSE                                                     
198800                    IF OKOD-FL-KRENOT-DIREKT = JA                         
198900                    OR (LEV-KDANMORS = '97'                               
199000                    AND (SDC OR NDC-PACIFIC OR LDC OR NDC-CN OR           
199100                         NDC-NS OR NDC-NX))                               
199200*-- 20120214 FRÅGETECKEN ANGÅENDE KINA OCH INTERNUPPACKNING.              
199300*-- KOMMER VIPS HA KOD 97?                                                
199400                      PERFORM E-BEHANDLA-KREDITNOTA-DIREKT                
199500                    END-IF                                                
199600                 END-IF                                                   
199700              END-IF                                                      
199800           END-IF                                                         
199900        END-IF                                                            
200000      END-IF                                                              
200100        PERFORM IMS-GNP-KREE-LEV                                          
200200     END-PERFORM                                                          
200300     .                                                                    
200400     EJECT                                                                
200500 C-BEHANDLA-RETILL SECTION.                                               
200600                                                                          
200700     PERFORM S100-NOLLA-720-AREA                                          
200800     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
200900     IF LEV-FLDIRLEV = JA               OR                                
201000        GOOD-DDC                        OR                                
201100        DIST35-REFILL-NA-JAP            OR                                
201200       (DIST07-USA-RETAILER AND CDC-SE) OR                                
201300       (DIST07-CAN-RETAILER AND CDC-SE) OR                                
201400       (DIST07-NON-VCC-OWNED AND CDC-SE)                                  
201500       CONTINUE                                                           
201600     ELSE                                                                 
201700        IF LEV-IDARTNR NOT = DUMMY-IDARTNR                                
201800          IF ART-KDERS-UTG = +0                                           
201900            IF LEV-KDANMORS = '12' OR '22'                                
202000*--ÄVEN CDC SKALL UPPDATERAS DIREKT ENL.BÅTH OCH BERRA 970110             
202100*--2006-03-07 FÖR LEV-IDDC NOT = 11/FTG=57 FÅR SALDOT EJ VARA < 0.        
202200               MOVE NEJ     TO MINUS-SALDO-SW                             
202300                                                                          
202400               IF LEV-IDDC NOT = DCS-IDDC                                 
202500                  MOVE LEV-IDDC TO W-IDDC-B6                              
202600                  PERFORM IMS-GU-WDB601                                   
202700               END-IF                                                     
202800                                                                          
202900               IF ( DCS-FTG-US OR DCS-FTG-CA   OR                         
203000                    DCS-LAND-NON-VCC-OWNED ) OR                           
203100                  ( LEV-IDDC = WC-CDC-SE )                                
203200                 PERFORM CA-SKRIV-EKONOMIFILER                            
203300                 IF SKRIV-HUV-EKOFIL                                      
203400                   MOVE LEV-IDDC TO WS-IDDC                               
203500                   IF XDC-NON-VCC-OWNED                                   
203600                     CONTINUE                                             
203700                   ELSE                                                   
203800                     PERFORM CB-FLYTTA-SKRIV-EKOFIL-HUVUD                 
203900                   END-IF                                                 
204000                   MOVE JA  TO EKHT-FLLSBOK                               
204100                   PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                   
204200                   MOVE NEJ            TO SKRIV-HUV-EKOFIL-SW             
204300                 ELSE                                                     
204400                   MOVE JA  TO EKHT-FLLSBOK                               
204500                   PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                   
204600                 END-IF                                                   
204700                 MOVE +2               TO UT34-KDAVVTYP                   
204800                 PERFORM S50-SKRIV-UPPDATPOST-KVLS                        
204900                 PERFORM CE-KOLLA-INVENTERING                             
205000               ELSE                                                       
205100                 MOVE LEV-IDARTNR      TO W-IDARTNR-K7                    
205200                 MOVE LEV-IDDC         TO W-IDDC                          
205300                 PERFORM IMS-GU-WDK711                                    
205400                 IF SEGMENT-FINNS                                         
205500                   COMPUTE SLAG-KVLS =                                    
205600                           SLAG-KVLS - LEV-KVLEVANM-BEKR                  
205700                   END-COMPUTE                                            
205800                   IF SLAG-KVLS < 0                                       
205900                     MOVE JA  TO MINUS-SALDO-SW                           
206000                     PERFORM CA-SKRIV-EKONOMIFILER                        
206100                     IF SKRIV-HUV-EKOFIL                                  
206200                       MOVE LEV-IDDC TO WS-IDDC                           
206300                       IF XDC-NON-VCC-OWNED                               
206400                         CONTINUE                                         
206500                       ELSE                                               
206600                         PERFORM CB-FLYTTA-SKRIV-EKOFIL-HUVUD             
206700                       END-IF                                             
206800                       MOVE NEJ TO EKHT-FLLSBOK                           
206900                       PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER               
207000                       MOVE NEJ            TO SKRIV-HUV-EKOFIL-SW         
207100                     ELSE                                                 
207200                       MOVE NEJ TO EKHT-FLLSBOK                           
207300                       PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER               
207400                     END-IF                                               
207500                     PERFORM CE-KOLLA-INVENTERING                         
207600                   ELSE                                                   
207700                     PERFORM CA-SKRIV-EKONOMIFILER                        
207800                     IF SKRIV-HUV-EKOFIL                                  
207900                       MOVE LEV-IDDC TO WS-IDDC                           
208000                       IF XDC-NON-VCC-OWNED                               
208100                         CONTINUE                                         
208200                       ELSE                                               
208300                         PERFORM CB-FLYTTA-SKRIV-EKOFIL-HUVUD             
208400                       END-IF                                             
208500                       MOVE JA  TO EKHT-FLLSBOK                           
208600                       PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER               
208700                       MOVE NEJ            TO SKRIV-HUV-EKOFIL-SW         
208800                     ELSE                                                 
208900                       MOVE JA  TO EKHT-FLLSBOK                           
209000                       PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER               
209100                     END-IF                                               
209200                     MOVE +2               TO UT34-KDAVVTYP               
209300                     PERFORM S50-SKRIV-UPPDATPOST-KVLS                    
209400                     PERFORM CE-KOLLA-INVENTERING                         
209500                   END-IF                                                 
209600                 ELSE                                                     
209700                   MOVE JA  TO MINUS-SALDO-SW                             
209800                   PERFORM CA-SKRIV-EKONOMIFILER                          
209900                   IF SKRIV-HUV-EKOFIL                                    
210000                     MOVE LEV-IDDC TO WS-IDDC                             
210100                     IF XDC-NON-VCC-OWNED                                 
210200                       CONTINUE                                           
210300                     ELSE                                                 
210400                       PERFORM CB-FLYTTA-SKRIV-EKOFIL-HUVUD               
210500                     END-IF                                               
210600                     MOVE NEJ TO EKHT-FLLSBOK                             
210700                     PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                 
210800                     MOVE NEJ            TO SKRIV-HUV-EKOFIL-SW           
210900                   ELSE                                                   
211000                     MOVE NEJ TO EKHT-FLLSBOK                             
211100                     PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                 
211200                   END-IF                                                 
211300                   PERFORM CE-KOLLA-INVENTERING                           
211400                 END-IF                                                   
211500               END-IF                                                     
211600            END-IF                                                        
211700            IF LEV-KDANMORS = '27'                                        
211800              IF SKRIV-HUV-EKOFIL                                         
211900                MOVE LEV-IDDC TO WS-IDDC                                  
212000                IF XDC-NON-VCC-OWNED                                      
212100                  CONTINUE                                                
212200                ELSE                                                      
212300                  PERFORM CB-FLYTTA-SKRIV-EKOFIL-HUVUD                    
212400                END-IF                                                    
212500                MOVE NEJ TO EKHT-FLLSBOK                                  
212600                PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                      
212700                MOVE NEJ            TO SKRIV-HUV-EKOFIL-SW                
212800              ELSE                                                        
212900                MOVE NEJ TO EKHT-FLLSBOK                                  
213000                PERFORM CF-FLYTTA-SKRIV-EKOFIL-RADER                      
213100              END-IF                                                      
213200                                                                          
213300              IF LEV-IDDC NOT = DCS-IDDC                                  
213400                 MOVE LEV-IDDC TO W-IDDC-B6                               
213500                 PERFORM IMS-GU-WDB601                                    
213600              END-IF                                                      
213700                                                                          
213800              PERFORM CE-KOLLA-INVENTERING                                
213900            END-IF                                                        
214000          END-IF                                                          
214100        END-IF                                                            
214200     END-IF                                                               
214300                                                                          
214400     PERFORM CC-SKRIV-UPPDAT-POST-KREE                                    
214500     IF LEV-KDANMORS = '54' OR '94' OR '97'                               
214600*-- KOD 97 INTERNUPPACKNING SKALL INTE GENERERA RETURTILLSTÅND            
214700        MOVE JA                     TO RETILL-FINNS-SW                    
214800     ELSE                                                                 
214900        PERFORM CD-SKAPA-SKRIV-RETILLPOSTER                               
215000     END-IF                                                               
215100     ADD +1                         TO WS-KVRADER-RT                      
215200     .                                                                    
215300     EJECT                                                                
215400 CA-SKRIV-EKONOMIFILER SECTION.                                           
215500                                                                          
215600     MOVE +2                        TO UT34-KDAVVTYP                      
215700*--  KONTERING                   EKONOMI-LAB                              
215800     MOVE '72C'                     TO 720-IDPTYP                         
215900                                       W-IDPTYP                           
216000                                                                          
216100     IF GOOD-DDC                                                          
216200       MOVE '11'                    TO 720-IDDC                           
216300     ELSE                                                                 
216400       MOVE LEV-IDDC                TO 720-IDDC                           
216500     END-IF                                                               
216600                                                                          
216700     MOVE ANM-IDDISTR               TO 720-IDDISTR                        
216800                                       TEST-IDDISTR                       
216900     MOVE ANM-IDKUNDNR              TO 720-IDKUNDNR                       
217000     MOVE +0                        TO 720-IDKNOTNR                       
217100     MOVE ANM-IDRAPPNR              TO 720-IDRAPPNR                       
217200     MOVE DAGENS-DATUM-SEKEL        TO 720-DAKRENOT                       
217300     MOVE LEV-IDARTNR               TO 720-IDARTNR                        
217400     MOVE LEV-KDANMORS              TO 720-KDANMORS                       
217500     COMPUTE 720-KVKREANT = LEV-KVLEVANM-BEKR * -1                        
217600     MOVE ART-KDPRODSL              TO 720-KDPRODSL                       
217700     MOVE JA                        TO 720-FLLSBOK                        
217800     MOVE CLAG-KDPSLLOC             TO 720-KDPSLLOC                       
217900                                                                          
218000     IF DIST79-DEALER-PRICE OR                                            
218100        DIST79-ECOM-PRICE                                                 
218200       MOVE LEV-PRARTBTO-LOC        TO 720-PRARTNTO                       
218300     ELSE                                                                 
218400       MOVE LEV-PRARTBTO            TO 720-PRARTNTO                       
218500     END-IF                                                               
218600                                                                          
218700     IF NDC-US OR NDC-CA                                                  
218800        MOVE LEV-IDDC               TO W-IDDC                             
218900        PERFORM IMS-GU-WDK711                                             
219000        IF SEGMENT-FINNS                                                  
219100           MOVE SLAG-PRAVCOST       TO 720-PRAVCOST                       
219200        ELSE                                                              
219300           MOVE +0                  TO 720-PRAVCOST                       
219400        END-IF                                                            
219500     ELSE                                                                 
219600        MOVE +0                     TO 720-PRAVCOST                       
219700     END-IF                                                               
219800                                                                          
219900     IF NDC-NA                                                            
220000       PERFORM S13-SKRIV-W41833                                           
220100     END-IF                                                               
220200     .                                                                    
220300     EJECT                                                                
220400 CB-FLYTTA-SKRIV-EKOFIL-HUVUD SECTION.                                    
220500                                                                          
220600     MOVE '303'                   TO EKHT-KDEKHHT                         
220700     MOVE '3XX'                   TO EKHT-KDEKSHT                         
220800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
220900     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
221000     MOVE 1                       TO EKHT-IDSEKVNR                        
221100     MOVE 'SUM'                   TO EKHT-KDEKNIVA                        
221200     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
221300     MOVE ' '                     TO EKHT-IDDC-REC                        
221400     MOVE ANM-IDDISTR          TO EKHT-IDDISTR                            
221500     MOVE ANM-IDKUNDNR         TO EKHT-IDKUNDNR                           
221600                                                                          
221700     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
221800     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
221900     CALL W009CIA USING           CIA-W009CIA                             
222000     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
222100                                                                          
222200     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
222300     MOVE 0                       TO EKHT-KDPRODSL                        
222400     MOVE 0                       TO EKHT-KDPSLLOC                        
222500     MOVE 0                       TO EKHT-IDARTNR                         
222600     MOVE ' '                     TO EKHT-FLLSBOK                         
222700     MOVE 'SEK'                   TO EKHT-KDVALISO                        
222800     MOVE 1.0                     TO EKHT-PRKURS                          
222900     MOVE 0                       TO EKHT-PRARTNTO                        
223000     MOVE 0                       TO EKHT-PRARTSJK                        
223100     MOVE 0                       TO EKHT-PRHEMTAG                        
223200     MOVE 0                       TO EKHT-PRARTSTD                        
223300     MOVE 0                       TO EKHT-PRLANDCO                        
223400     MOVE 0                       TO EKHT-PRINK                           
223500     MOVE 0                       TO EKHT-PRDIRLON                        
223600     MOVE 0                       TO EKHT-PRDMTRL                         
223700     MOVE 0                       TO EKHT-PROVRPAL                        
223800     MOVE 0                       TO EKHT-KVANTAL                         
223900     MOVE 'W4183000'              TO EKHT-IDPGM                           
224000     MOVE ' '                     TO EKHT-IDTRANS                         
224100                                                                          
224200     COMPUTE EKHT-SUBEL = LEV-KVLEVANM-BEKR                               
224300                       * CLAG-PRARTSTD                                    
224400                                                                          
224500     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
224600     MOVE SPACE                  TO EKHT-IDANALYS                         
224700     MOVE ZERO                   TO EKHT-BEVAT                            
224800                                    EKHT-IDKONTO                          
224900                                    EKHT-KDANMORS                         
225000                                    EKHT-KDFRAKT                          
225100                                    EKHT-SUVAT                            
225200     MOVE ZERO                   TO EKHT-DAAVIDAT                         
225300                                    EKHT-IDAVINR                          
225400                                    EKHT-KDAVVTYP                         
225500                                    EKHT-KDRT                             
225600                                    EKHT-KVANTMOT                         
225700                                    EKHT-KVAVIS                           
225800     MOVE WS-KDSORT              TO EKHT-KDSORT                           
225900     MOVE SPACE                  TO EKHT-KDTRADP                          
226000                                    EKHT-IDLEVNR                          
226100                                    EKHT-IDKST                            
226200     MOVE SPACE                  TO EKHT-FLOVRLEV                         
226300     MOVE ZERO                   TO EKHT-IDORDNR5                         
226400     MOVE SPACE                  TO EKHT-IDUSER                           
226500     MOVE NEJ                    TO EKHT-FLDCET                           
226600     MOVE SPACE                  TO EKHT-IDKUNDRF                         
226700     MOVE LEV-IDFAKT             TO EKHT-IDFAKT-EXP                       
226800                                                                          
226900     PERFORM S06-SKRIV-EKONOMIFIL                                         
227000     .                                                                    
227100     EJECT                                                                
227200 CC-SKRIV-UPPDAT-POST-KREE SECTION.                                       
227300*    DISPLAY '*** CC-SKRIV-UPPDAT-POST-KREE'                              
227400*                                                                         
227500*** UPPDATERAR WDA201 MED RETILLDATUM.                                    
227600*                                                                         
227700     MOVE '003'                     TO UT34-IDPTYP                        
227800     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
227900     MOVE ZERO                      TO UT34-IDARTNR                       
228000     MOVE ZERO                      TO UT34-IDRADNR                       
228100     MOVE SPACE                     TO UT34-IDDC                          
228200     MOVE SPACE                     TO UT34-FLFARLIG                      
228300     MOVE ZERO                      TO UT34-IDKNOTNR                      
228400                                       UT34-KDAVVTYP                      
228500                                       UT34-KDFAKTYP-KNOT                 
228600                                       UT34-KVRADER-RT                    
228700     MOVE SPACE                     TO UT34-KDLEVANM                      
228800     MOVE ZERO                      TO UT34-KVLEVANM                      
228900     MOVE ZERO                      TO UT34-TIKNOTA                       
229000     MOVE DAGENS-DATUM              TO UT34-TIRETILL                      
229100     MOVE ZERO                      TO UT34-PRARTBTO                      
229200     MOVE ZERO                      TO UT34-KDINVKAT                      
229300     MOVE ZERO                      TO UT34-KVJUSTKV                      
229400     MOVE ZERO                      TO UT34-TIM-INV                       
229500     MOVE SPACE                     TO UT34-TEINVANM                      
229600     MOVE SPACE                     TO UT34-KDARBTYP                      
229700     MOVE ZERO                      TO UT34-IDPERSON                      
229800     MOVE SPACE                     TO UT34-IDDC-RET                      
229900     MOVE ZERO                      TO UT34-IXDCCLEAR                     
230000                                                                          
230100     PERFORM S20-SKRIV-W41834                                             
230200     .                                                                    
230300     EJECT                                                                
230400 CD-SKAPA-SKRIV-RETILLPOSTER SECTION.                                     
230500*    DISPLAY '*** CD-SKAPA-SKRIV-RETILLPOSTER'                            
230600                                                                          
230700     IF SKRIV-HUV-RETILL                                                  
230800        PERFORM CDA-SKAPA-HUVUD-RETILL                                    
230900        PERFORM CDB-SKAPA-RADER-RETILL                                    
231000        MOVE NEJ                    TO SKRIV-HUV-RETILL-SW                
231100     ELSE                                                                 
231200        PERFORM CDB-SKAPA-RADER-RETILL                                    
231300     END-IF                                                               
231400     .                                                                    
231500     EJECT                                                                
231600 CDA-SKAPA-HUVUD-RETILL SECTION.                                          
231700*    DISPLAY '*** CDA-SKAPA-HUVUD-RETILL'                                 
231800                                                                          
231900     MOVE '717'                     TO 717-IDPTYP                         
232000     MOVE ANM-IDDISTR               TO 717-IDDISTR                        
232100     MOVE ANM-IDKUNDNR              TO 717-IDKUNDNR                       
232200     MOVE ANM-IDRAPPNR              TO 717-IDRAPPNR                       
232300     MOVE LEV-IDDC                  TO 717-IDDC                           
232400     MOVE ANM-PRFRAKT               TO 717-PRFRAKT                        
232500     MOVE ANM-PRFOERS               TO 717-PRFOERS                        
232600     MOVE ANM-PRLEGKST              TO 717-PRLEGKST                       
232700     MOVE ANM-RELANDCO              TO 717-RELANDCO                       
232800     MOVE ANM-REEMBHNT              TO 717-REEMBHNT                       
232900     MOVE LEV-DALEVANM (3:6)        TO 717-TILEVANM                       
233000                                                                          
233100     PERFORM S14-SKRIV-W41835                                             
233200     .                                                                    
233300     EJECT                                                                
233400 CDB-SKAPA-RADER-RETILL SECTION.                                          
233500*    DISPLAY '*** CDB-SKAPA-RADER-RETILL '                                
233600                                                                          
233700     MOVE '718'                     TO 718-IDPTYP                         
233800     MOVE ANM-IDDISTR               TO 718-IDDISTR                        
233900     MOVE ANM-IDKUNDNR              TO 718-IDKUNDNR                       
234000     MOVE ANM-IDRAPPNR              TO 718-IDRAPPNR                       
234100     MOVE LEV-IDDC                  TO 718-IDDC                           
234200     MOVE LEV-IDARTNR               TO 718-IDARTNR                        
234300     MOVE LEV-IDORDNR7              TO 718-IDORDNR                        
234400     MOVE LEV-IDRADNR               TO 718-IDRADNR                        
234500     MOVE LEV-KDANMORS              TO 718-KDANMORS                       
234600     MOVE LEV-KVLEVANM-BEKR         TO 718-KVLEVANM                       
234700     MOVE LEV-PRARTBTO              TO 718-PRARTBTO                       
234800     MOVE LEV-PRARTBTO-LOC          TO 718-PRARTBTO-LOC                   
234900                                                                          
235000     PERFORM S14-SKRIV-W41835                                             
235100     PERFORM CDC-SKAPA-RKE-POST                                           
235200                                                                          
235300     PERFORM  IMS-GU-ARTN-ARTN01                                          
235400                                                                          
235500     IF SEGMENT-FINNS                                                     
235600       IF CLAG-KDFARLIG = 4 OR 6 OR 7                                     
235700         MOVE JA                    TO FARLIGT-GODS-SW                    
235800       END-IF                                                             
235900     END-IF                                                               
236000                                                                          
236100     MOVE JA                        TO RETILL-FINNS-SW                    
236200     .                                                                    
236300     EJECT                                                                
236400 CDC-SKAPA-RKE-POST SECTION.                                              
236500*    DISPLAY '**** CDC-SKAPA-RKE-POST'                                    
236600*                            *** SKAPAR TRANS TILL NOAC MED               
236700*                            *** INFO OM RETURTILLSTÅNDSRADER             
236800     IF NOT LEV-KDANMORS = '74'                                           
236900                                                                          
237000       MOVE 'RKE'                    TO RKE-IDPTYP                        
237100       MOVE ANM-IDDISTR              TO RKE-IDDISTR                       
237200       MOVE ANM-IDKUNDNR             TO RKE-IDKUNDNR                      
237300       MOVE LEV-IDDC                 TO RKE-IDDC                          
237400       MOVE ANM-IDRAPPNR             TO RKE-IDRAPPNR                      
237500       MOVE LEV-IDARTNR              TO RKE-IDARTNR                       
237600       MOVE LEV-IDRADNR              TO RKE-IDRADNR                       
237700       MOVE ART-REKSIFFR             TO RKE-REKSIFFR                      
237800       MOVE DAGENS-DATUM             TO RKE-TIRETILL                      
237900       MOVE ANM-IDRAPPNR             TO RKE-IDRAPPNR-002                  
238000       MOVE SPACE                    TO RKE-FILLERX31                     
238100                                                                          
238200       MOVE ANM-IDDISTR               TO TEST-IDDISTR                     
238300       IF DIST35-NA-CDC-RETURN  OR DIST18-SCRAP-NDC-QUAL                  
238400       OR DIST35-CDC-RETURNS-NON-VCC                                      
238500         CONTINUE                                                         
238600       ELSE                                                               
238700          PERFORM S19-SKRIV-W4183B                                        
238800       END-IF                                                             
238900                                                                          
239000     END-IF                                                               
239100     .                                                                    
239200     EJECT                                                                
239300 CE-KOLLA-INVENTERING SECTION.                                            
239400*    DISPLAY '*** CE-KOLLA-INVENTERING'                                   
239500                                                                          
239600     IF NDC-US OR NDC-CA                                                  
239700     OR XDC-NON-VCC-OWNED                                                 
239800        MOVE LEV-IDDC               TO W-IDDC                             
239900        PERFORM IMS-GU-WDK711                                             
240000        IF SEGMENT-FINNS                                                  
240100           COMPUTE WS-INVVARDE ROUNDED =                                  
240200                   LEV-KVLEVANM-BEKR * SLAG-PRAVCOST                      
240300           END-COMPUTE                                                    
240400        ELSE                                                              
240500           MOVE +0                  TO WS-INVVARDE                        
240600        END-IF                                                            
240700     ELSE                                                                 
240800        IF SDC OR LDC                                                     
240900          MOVE LEV-IDDC               TO W-IDDC                           
241000          PERFORM IMS-GU-WDK711                                           
241100          IF SEGMENT-FINNS                                                
241200            COMPUTE WS-INVVARDE ROUNDED =                                 
241300                    LEV-KVLEVANM-BEKR * CLAG-PRARTSTD                     
241400            END-COMPUTE                                                   
241500          ELSE                                                            
241600            MOVE +0                 TO WS-INVVARDE                        
241700          END-IF                                                          
241800        ELSE                                                              
241900          COMPUTE WS-INVVARDE ROUNDED =                                   
242000                  LEV-KVLEVANM-BEKR * CLAG-PRARTSTD                       
242100          END-COMPUTE                                                     
242200        END-IF                                                            
242300     END-IF                                                               
242400                                                                          
242500     IF LEV-IDDC NOT = DCS-IDDC                                           
242600        MOVE LEV-IDDC TO W-IDDC-B6                                        
242700        PERFORM IMS-GU-WDB601                                             
242800     END-IF                                                               
242900                                                                          
243000     EVALUATE TRUE                                                        
243100        WHEN CDC-SE                                                       
243200           IF WS-INVVARDE > DCS-SUINVGRANS                                
243300              IF LEV-FLDIRLEV = JA                                        
243400                CONTINUE                                                  
243500              ELSE                                                        
243600                 IF ART-KDERS-UTG = +0                                    
243700                    IF CLAG-KDVVKL NOT = +4 AND +5                        
243800                       PERFORM S65-UPPD-INVENTERING                       
243900                    END-IF                                                
244000                 ELSE                                                     
244100                    PERFORM S65-UPPD-INVENTERING                          
244200                 END-IF                                                   
244300              END-IF                                                      
244400           END-IF                                                         
244500        WHEN SDC                                                          
244600           IF OKOD-FL-INVENT-SDC = JA                                     
244700              IF WS-INVVARDE > DCS-SUINVGRANS                             
244800                 IF LEV-FLDIRLEV = JA                                     
244900                   CONTINUE                                               
245000                 ELSE                                                     
245100                    PERFORM S65-UPPD-INVENTERING                          
245200                 END-IF                                                   
245300              END-IF                                                      
245400           END-IF                                                         
245500        WHEN NDC                                                          
245600           IF XDC-NON-VCC-OWNED                                           
245700             IF WS-INVVARDE > DCS-SUINVGRANS                              
245800               IF LEV-FLDIRLEV = JA                                       
245900                 CONTINUE                                                 
246000               ELSE                                                       
246100                 PERFORM S65-UPPD-INVENTERING                             
246200               END-IF                                                     
246300             END-IF                                                       
246400           ELSE                                                           
246500             IF WS-INVVARDE > DCS-SUINVGRANS                              
246600               IF LEV-FLDIRLEV = JA                                       
246700                 CONTINUE                                                 
246800               ELSE                                                       
246900                 PERFORM S65-UPPD-INVENTERING                             
247000               END-IF                                                     
247100             END-IF                                                       
247200           END-IF                                                         
247300        WHEN LDC                                                          
247400           IF OKOD-FL-INVENT-SDC = JA                                     
247500               IF WS-INVVARDE > DCS-SUINVGRANS                            
247600                  IF LEV-FLDIRLEV = JA                                    
247700                    CONTINUE                                              
247800                  ELSE                                                    
247900                     PERFORM S65-UPPD-INVENTERING                         
248000                  END-IF                                                  
248100               END-IF                                                     
248200           END-IF                                                         
248300     END-EVALUATE                                                         
248400     .                                                                    
248500     EJECT                                                                
248600 CF-FLYTTA-SKRIV-EKOFIL-RADER SECTION.                                    
248700                                                                          
248800     MOVE LEV-IDDC                TO WS-IDDC                              
248900     IF XDC-NON-VCC-OWNED                                                 
249000       IF LEV-KDANMORS = '12' OR '22'                                     
249100         IF NDC-IN                                                        
249200           PERFORM CFA-FLYTTA-SKRIV-EKOFIL-RAD-IN                         
249300         ELSE                                                             
249400           PERFORM CFA-FLYTTA-SKRIV-EKOFIL-RAD-SC                         
249500         END-IF                                                           
249600       END-IF                                                             
249700     ELSE                                                                 
249800       PERFORM CFB-FLYTTA-SKRIV-EKOFIL-RADER                              
249900     END-IF                                                               
250000     .                                                                    
250100     EJECT                                                                
250200 CFA-FLYTTA-SKRIV-EKOFIL-RAD-SC SECTION.                                  
250300                                                                          
250400     MOVE LEV-IDDC                TO SC-EKHT-IDDC-SEND                    
250500     MOVE ' '                     TO SC-EKHT-IDDC-REC                     
250600                                                                          
250700     MOVE '303'                   TO SC-EKHT-KDEKHHT                      
250800     MOVE '311'                   TO SC-EKHT-KDEKSHT                      
250900                                                                          
251000     IF MINUS-SALDO-SW = JA                                               
251100       MOVE '403'                 TO SC-EKHT-KDEKHHT                      
251200       MOVE '408'                 TO SC-EKHT-KDEKSHT                      
251300       MOVE LEV-IDDC-RET          TO SC-EKHT-IDDC-REC                     
251400     END-IF                                                               
251500                                                                          
251600     MOVE 'DET'                   TO SC-EKHT-KDEKNIVA                     
251700     MOVE FUNCTION CURRENT-DATE (1:8) TO SC-EKHT-TIREGDAT                 
251800     MOVE FUNCTION CURRENT-DATE (9:8) TO SC-EKHT-TIKLOCK                  
251900     MOVE 1                       TO SC-EKHT-IDSEKVNR                     
252000     MOVE ANM-IDDISTR          TO SC-EKHT-IDDISTR                         
252100     MOVE ANM-IDKUNDNR         TO SC-EKHT-IDKUNDNR                        
252200                                                                          
252300     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
252400     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
252500     CALL W009CIA USING           CIA-W009CIA                             
252600     MOVE CIA-IDARTBET-UT      TO SC-EKHT-IDVERGL                         
252700                                                                          
252800     MOVE DAGENS-DATUM-SEKEL      TO SC-EKHT-DAVERDAT                     
252900     MOVE 0                       TO SC-EKHT-KDPRODSL                     
253000     MOVE 0                       TO SC-EKHT-KDPSLLOC                     
253100     MOVE LEV-IDARTNR             TO SC-EKHT-IDARTNR                      
253200                                                                          
253300     MOVE JA                      TO SC-EKHT-FLLSBOK                      
253400                                                                          
253500     MOVE 1.0                     TO SC-EKHT-PRKURS                       
253600     MOVE 0                       TO SC-EKHT-PRARTNTO                     
253700     MOVE 0                       TO SC-EKHT-PRARTSJK                     
253800     MOVE 0                       TO SC-EKHT-PRHEMTAG                     
253900                                                                          
254000     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
254100     MOVE LEV-IDDC         TO W-IDDC                                      
254200     PERFORM IMS-GU-WDK711                                                
254300     IF SEGMENT-FINNS                                                     
254400       MOVE SLAG-PRAVCOST         TO SC-EKHT-PRARTSTD                     
254500     ELSE                                                                 
254600       MOVE 0                     TO SC-EKHT-PRARTSTD                     
254700     END-IF                                                               
254800                                                                          
254900     MOVE 0                       TO SC-EKHT-PRINK                        
255000     MOVE 0                       TO SC-EKHT-PRDIRLON                     
255100     MOVE 0                       TO SC-EKHT-PRDMTRL                      
255200     MOVE 0                       TO SC-EKHT-PROVRPAL                     
255300     MOVE 0                       TO SC-EKHT-PRLANDCO                     
255400     MOVE LEV-KVLEVANM-BEKR       TO SC-EKHT-KVANTAL                      
255500     MOVE 0                       TO SC-EKHT-SUBEL                        
255600     MOVE 'W4183000'              TO SC-EKHT-IDPGM                        
255700     MOVE ' '                     TO SC-EKHT-IDTRANS                      
255800     MOVE LEV-KDANMORS            TO SC-EKHT-KDANMORS                     
255900     MOVE LEV-IDANALYS            TO SC-EKHT-IDANALYS                     
256000     MOVE LEV-IDKONTO             TO SC-EKHT-IDKONTO                      
256100     MOVE LEV-IDKST               TO SC-EKHT-IDKST                        
256200     MOVE ZERO                   TO SC-EKHT-BEVAT                         
256300                                    SC-EKHT-KDFRAKT                       
256400                                    SC-EKHT-SUVAT                         
256500     MOVE ZERO                   TO SC-EKHT-DAAVIDAT                      
256600                                    SC-EKHT-IDAVINR                       
256700                                    SC-EKHT-KDAVVTYP                      
256800                                    SC-EKHT-KDRT                          
256900                                    SC-EKHT-KVANTMOT                      
257000                                    SC-EKHT-KVAVIS                        
257100     MOVE WS-KDSORT              TO SC-EKHT-KDSORT                        
257200     MOVE SPACE                  TO SC-EKHT-IDLEVNR                       
257300     MOVE SPACE                  TO SC-EKHT-FLOVRLEV                      
257400     MOVE ZERO                   TO SC-EKHT-IDORDNR5                      
257500     MOVE NEJ                    TO SC-EKHT-FLDCET                        
257600     MOVE SPACE                  TO SC-EKHT-IDKUNDRF                      
257700     MOVE LEV-IDFAKT             TO SC-EKHT-IDFAKT-EXP                    
257800     MOVE DCS-KDVALISO           TO SC-EKHT-KDVALISO                      
257900     MOVE DCS-KDTRADP            TO SC-EKHT-KDTRADP                       
258000     IF NDC-CN                                                            
258100       MOVE 'W570'               TO SC-EKHT-IDCPYTXT(1:4)                 
258200     ELSE                                                                 
258300       MOVE DCS-KDTRADP          TO SC-EKHT-IDCPYTXT(1:4)                 
258400     END-IF                                                               
258500     MOVE 'EKHA'                 TO SC-EKHT-IDCPYTXT(5:4)                 
258600                                                                          
258700     PERFORM S07-SKRIV-EKONOMIFIL                                         
258800     .                                                                    
258900     EJECT                                                                
259000 CFA-FLYTTA-SKRIV-EKOFIL-RAD-IN SECTION.                                  
259100                                                                          
259200     MOVE LEV-IDDC                TO IN-EKHT-IDDC-SEND                    
259300     MOVE ' '                     TO IN-EKHT-IDDC-REC                     
259400                                                                          
259500     MOVE '303'                   TO IN-EKHT-KDEKHHT                      
259600     MOVE '311'                   TO IN-EKHT-KDEKSHT                      
259700                                                                          
259800     IF MINUS-SALDO-SW = JA                                               
259900       MOVE '403'                 TO IN-EKHT-KDEKHHT                      
260000       MOVE '408'                 TO IN-EKHT-KDEKSHT                      
260100       MOVE LEV-IDDC-RET          TO IN-EKHT-IDDC-REC                     
260200     END-IF                                                               
260300                                                                          
260400     MOVE 'DET'                   TO IN-EKHT-KDEKNIVA                     
260500     MOVE FUNCTION CURRENT-DATE (1:8) TO IN-EKHT-TIREGDAT                 
260600     MOVE FUNCTION CURRENT-DATE (9:8) TO IN-EKHT-TIKLOCK                  
260700     MOVE 1                       TO IN-EKHT-IDSEKVNR                     
260800     MOVE ANM-IDDISTR          TO IN-EKHT-IDDISTR                         
260900     MOVE ANM-IDKUNDNR         TO IN-EKHT-IDKUNDNR                        
261000                                                                          
261100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
261200     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
261300     CALL W009CIA USING           CIA-W009CIA                             
261400     MOVE CIA-IDARTBET-UT      TO IN-EKHT-IDVERGL                         
261500                                                                          
261600     MOVE DAGENS-DATUM-SEKEL      TO IN-EKHT-DAVERDAT                     
261700     MOVE 0                       TO IN-EKHT-KDPRODSL                     
261800     MOVE 0                       TO IN-EKHT-KDPSLLOC                     
261900     MOVE LEV-IDARTNR             TO IN-EKHT-IDARTNR                      
262000                                                                          
262100     MOVE JA                      TO IN-EKHT-FLLSBOK                      
262200                                                                          
262300     MOVE 'INR'                   TO IN-EKHT-KDVALISO                     
262400     MOVE 1.0                     TO IN-EKHT-PRKURS                       
262500     MOVE 0                       TO IN-EKHT-PRARTNTO                     
262600     MOVE 0                       TO IN-EKHT-PRARTSJK                     
262700     MOVE 0                       TO IN-EKHT-PRHEMTAG                     
262800                                                                          
262900     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
263000     MOVE LEV-IDDC         TO W-IDDC                                      
263100     PERFORM IMS-GU-WDK711                                                
263200     IF SEGMENT-FINNS                                                     
263300       MOVE SLAG-PRAVCOST         TO IN-EKHT-PRARTSTD                     
263400     ELSE                                                                 
263500       MOVE 0                     TO IN-EKHT-PRARTSTD                     
263600     END-IF                                                               
263700                                                                          
263800     MOVE 0                       TO IN-EKHT-PRINK                        
263900     MOVE 0                       TO IN-EKHT-PRDIRLON                     
264000     MOVE 0                       TO IN-EKHT-PRDMTRL                      
264100     MOVE 0                       TO IN-EKHT-PROVRPAL                     
264200     MOVE 0                       TO IN-EKHT-PRLANDCO                     
264300     MOVE LEV-KVLEVANM-BEKR       TO IN-EKHT-KVANTAL                      
264400     MOVE 0                       TO IN-EKHT-SUBEL                        
264500     MOVE 'W4183000'              TO IN-EKHT-IDPGM                        
264600     MOVE ' '                     TO IN-EKHT-IDTRANS                      
264700     MOVE 'W515EKHA'              TO IN-EKHT-IDCPYTXT                     
264800     MOVE LEV-KDANMORS            TO IN-EKHT-KDANMORS                     
264900     MOVE LEV-IDANALYS            TO IN-EKHT-IDANALYS                     
265000     MOVE LEV-IDKONTO             TO IN-EKHT-IDKONTO                      
265100     MOVE LEV-IDKST               TO IN-EKHT-IDKST                        
265200     MOVE ZERO                   TO IN-EKHT-BEVAT                         
265300                                    IN-EKHT-KDFRAKT                       
265400                                    IN-EKHT-SUVAT                         
265500     MOVE ZERO                   TO IN-EKHT-DAAVIDAT                      
265600                                    IN-EKHT-IDAVINR                       
265700                                    IN-EKHT-KDAVVTYP                      
265800                                    IN-EKHT-KDRT                          
265900                                    IN-EKHT-KVANTMOT                      
266000                                    IN-EKHT-KVAVIS                        
266100     MOVE WS-KDSORT              TO IN-EKHT-KDSORT                        
266200     MOVE 'IN07'                 TO IN-EKHT-KDTRADP                       
266300     MOVE SPACE                  TO IN-EKHT-IDLEVNR                       
266400     MOVE SPACE                  TO IN-EKHT-FLOVRLEV                      
266500     MOVE ZERO                   TO IN-EKHT-IDORDNR5                      
266600     MOVE NEJ                    TO IN-EKHT-FLDCET                        
266700     MOVE SPACE                  TO IN-EKHT-IDKUNDRF                      
266800     MOVE LEV-IDFAKT             TO IN-EKHT-IDFAKT-EXP                    
266900                                                                          
267000     PERFORM S07-SKRIV-EKONOMIFIL-IN                                      
267100     .                                                                    
267200     EJECT                                                                
267300 CFB-FLYTTA-SKRIV-EKOFIL-RADER SECTION.                                   
267400                                                                          
267500     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
267600     MOVE ' '                     TO EKHT-IDDC-REC                        
267700                                                                          
267800     MOVE '303'                   TO EKHT-KDEKHHT                         
267900                                                                          
268000     IF LEV-KDANMORS = '12' OR '22'                                       
268100       MOVE '311'                 TO EKHT-KDEKSHT                         
268200       IF MINUS-SALDO-SW = JA                                             
268300         MOVE '403'               TO EKHT-KDEKHHT                         
268400         MOVE '408'               TO EKHT-KDEKSHT                         
268500         MOVE LEV-IDDC-RET        TO EKHT-IDDC-REC                        
268600       END-IF                                                             
268700     ELSE                                                                 
268800       IF LEV-KDANMORS = '27'                                             
268900         MOVE '316'               TO EKHT-KDEKSHT                         
269000       ELSE                                                               
269100         MOVE 0                   TO EKHT-KDEKSHT (1:1)                   
269200         MOVE LEV-KDANMORS        TO EKHT-KDEKSHT (2:2)                   
269300       END-IF                                                             
269400     END-IF                                                               
269500                                                                          
269600     MOVE 'DET'                   TO EKHT-KDEKNIVA                        
269700     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
269800     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
269900     MOVE 1                       TO EKHT-IDSEKVNR                        
270000     MOVE ANM-IDDISTR          TO EKHT-IDDISTR                            
270100     MOVE ANM-IDKUNDNR         TO EKHT-IDKUNDNR                           
270200                                                                          
270300     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
270400     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
270500     CALL W009CIA USING           CIA-W009CIA                             
270600     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
270700                                                                          
270800     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
270900     MOVE 0                       TO EKHT-KDPRODSL                        
271000     MOVE 0                       TO EKHT-KDPSLLOC                        
271100     MOVE LEV-IDARTNR             TO EKHT-IDARTNR                         
271200                                                                          
271300**- SE SEKTION C-BEHANDLA-RETILL  TO EKHT-FLLSBOK                         
271400                                                                          
271500     MOVE 'SEK'                   TO EKHT-KDVALISO                        
271600     MOVE 1.0                     TO EKHT-PRKURS                          
271700     MOVE 0                       TO EKHT-PRARTNTO                        
271800     MOVE 0                       TO EKHT-PRARTSJK                        
271900     MOVE 0                       TO EKHT-PRHEMTAG                        
272000     MOVE CLAG-PRARTSTD           TO EKHT-PRARTSTD                        
272100     MOVE 0                       TO EKHT-PRINK                           
272200     MOVE 0                       TO EKHT-PRDIRLON                        
272300     MOVE 0                       TO EKHT-PRDMTRL                         
272400     MOVE 0                       TO EKHT-PROVRPAL                        
272500     MOVE 0                       TO EKHT-PRLANDCO                        
272600     MOVE LEV-KVLEVANM-BEKR       TO EKHT-KVANTAL                         
272700     MOVE 0                       TO EKHT-SUBEL                           
272800     MOVE 'W4183000'              TO EKHT-IDPGM                           
272900     MOVE ' '                     TO EKHT-IDTRANS                         
273000     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
273100     MOVE LEV-KDANMORS            TO EKHT-KDANMORS                        
273200     MOVE LEV-IDANALYS            TO EKHT-IDANALYS                        
273300     MOVE LEV-IDKONTO             TO EKHT-IDKONTO                         
273400     MOVE LEV-IDKST               TO EKHT-IDKST                           
273500     MOVE ZERO                   TO EKHT-BEVAT                            
273600                                    EKHT-KDFRAKT                          
273700                                    EKHT-SUVAT                            
273800     MOVE ZERO                   TO EKHT-DAAVIDAT                         
273900                                    EKHT-IDAVINR                          
274000                                    EKHT-KDAVVTYP                         
274100                                    EKHT-KDRT                             
274200                                    EKHT-KVANTMOT                         
274300                                    EKHT-KVAVIS                           
274400     MOVE WS-KDSORT              TO EKHT-KDSORT                           
274500     MOVE 'SEPV'                 TO EKHT-KDTRADP                          
274600     MOVE SPACE                  TO EKHT-IDLEVNR                          
274700     MOVE SPACE                  TO EKHT-FLOVRLEV                         
274800     MOVE ZERO                   TO EKHT-IDORDNR5                         
274900     MOVE SPACE                  TO EKHT-IDUSER                           
275000     IF LEV-IDDC = WC-SDC-NL-ET                                           
275100       MOVE JA                   TO EKHT-FLDCET                           
275200     ELSE                                                                 
275300       MOVE NEJ                  TO EKHT-FLDCET                           
275400     END-IF                                                               
275500     MOVE SPACE                  TO EKHT-IDKUNDRF                         
275600     MOVE LEV-IDFAKT             TO EKHT-IDFAKT-EXP                       
275700                                                                          
275800     PERFORM S06-SKRIV-EKONOMIFIL                                         
275900     .                                                                    
276000     EJECT                                                                
276100 D-BEHANDLA-TILLAEGGSFAKTURA SECTION.                                     
276200*    DISPLAY '** D-BEHANDLA-TILLAEGGSFAKTURA '                            
276300*    TILLÄGGSFAKTURERING VIA ORDER                                        
276400                                                                          
276500***- FLLSBOK SÄTTS PÅ ORDERHUVUDET OCH GÄLLER FÖR ALLA ORDERRADER,        
276600***- DÄRFÖR KAN MAN INTE BLANDA KOD 11/21 MED 61 OCH DIRLEV=JA MED        
276700***- DIRLEV=NEJ.                                                          
276800***- 2006-03-07 KOD 26 SKALL EJ RÖRA SALDOT HELLER.                       
276900***-            KOD 11/21 RÖR EJ SALDOT OM SLAG-KVLS < 0 /IDDC-LEV        
277000***                                                                       
277100***- 2002-09-20 ENLIGT SUSSI, TUULA, BERIT JEBSEN... SKALL VI GÖRA        
277200***- ETT UNTANTAG FRÅN DENNA REGEL FÖR REFILL-DISTR. 8141-8151            
277300***- SAMT JAPAN-REFILLEN 8541-8551.                                       
277400***- 2018-08-29 GLOBAL EXPORT KOD 11,21 OCH 61 RÖR EJ SALDOT.             
277500                                                                          
277600     MOVE ANM-IDDISTR        TO TEST-IDDISTR                              
277700     MOVE NEJ                TO MINUS-SALDO-SW                            
277800                                                                          
277900     IF DIST35-REFILL-NA OR DIST35-REFILL-NA-JAP OR                       
278000        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
278100        DIST35-NONVCC-NONVCC-REFILL                                       
278200                                                                          
278300       IF LEV-IDDC = IDDC-WS                                              
278400          CONTINUE                                                        
278500       ELSE                                                               
278600          PERFORM S05-HAEMTA-ORDERNR                                      
278700       END-IF                                                             
278800     ELSE                                                                 
278900       IF LEV-KDANMORS = '11' OR '21'                                     
279000         PERFORM DC-KOLLA-FLLSBOK-KOD-11-21                               
279100       END-IF                                                             
279200                                                                          
279300       IF ( LEV-IDDC = IDDC-WS ) AND                                      
279400          ( LEV-KDANMORS = KDANMORS-WS ) AND                              
279500          ( LEV-FLDIRLEV = FLDIRLEV-WS )                                  
279600                                                                          
279700         IF LEV-KDANMORS = '11' OR '21'                                   
279800           PERFORM DD-KOLLA-OM-FLLSBOK-LIKA                               
279900         END-IF                                                           
280000       ELSE                                                               
280100          PERFORM S05-HAEMTA-ORDERNR                                      
280200       END-IF                                                             
280300     END-IF                                                               
280400                                                                          
280500     MOVE LEV-IDDC                  TO IDDC-WS                            
280600     MOVE LEV-KDANMORS              TO KDANMORS-WS                        
280700     MOVE LEV-FLDIRLEV              TO FLDIRLEV-WS                        
280800                                                                          
280900     MOVE '001'                   TO UT34-IDPTYP                          
281000     MOVE ANM-IDLEVANM            TO UT34-IDLEVANM                        
281100     MOVE LEV-IDARTNR             TO UT34-IDARTNR                         
281200     MOVE LEV-IDRADNR             TO UT34-IDRADNR                         
281300     MOVE SPACE                   TO UT34-IDDC                            
281400     MOVE SPACE                   TO UT34-FLFARLIG                        
281500     MOVE IN-IDORDNR              TO UT34-IDKNOTNR                        
281600                                      VIR-IDKNOTNR                        
281700     MOVE ZERO                    TO UT34-KDAVVTYP                        
281800     MOVE 'R'                     TO UT34-KDFAKTYP-KNOT                   
281900     MOVE SPACE                   TO UT34-KDLEVANM                        
282000     MOVE ZERO                    TO UT34-KVLEVANM                        
282100     MOVE DAGENS-DATUM            TO UT34-TIKNOTA                         
282200     MOVE ZERO                    TO UT34-TIRETILL                        
282300                                     UT34-KVRADER-RT                      
282400     MOVE ZERO                    TO UT34-PRARTBTO                        
282500     MOVE ZERO                    TO UT34-KDINVKAT                        
282600     MOVE ZERO                    TO UT34-KVJUSTKV                        
282700     MOVE ZERO                    TO UT34-TIM-INV                         
282800     MOVE SPACE                   TO UT34-TEINVANM                        
282900     MOVE SPACE                   TO UT34-KDARBTYP                        
283000     MOVE ZERO                    TO UT34-IDPERSON                        
283100     MOVE SPACE                   TO UT34-IDDC-RET                        
283200     MOVE ZERO                    TO UT34-IXDCCLEAR                       
283300     PERFORM S20-SKRIV-W41834                                             
283400                                                                          
283500     PERFORM DB-SKAPA-RADER-DR5                                           
283600                                                                          
283700     MOVE LEV-IDDC TO WS-IDDC                                             
283800     IF LEV-KDANMORS = '26'                                               
283900       IF XDC-NON-VCC-OWNED                                               
284000         CONTINUE                                                         
284100       ELSE                                                               
284200         PERFORM FAC-SKAPA-EKOFIL-RAD                                     
284300       END-IF                                                             
284400     END-IF                                                               
284500                                                                          
284600     IF LEV-KDANMORS = '11' OR '21'                                       
284700       IF XDC-NON-VCC-OWNED                                               
284800         CONTINUE                                                         
284900       ELSE                                                               
285000         IF MINUS-SALDO-SW = JA                                           
285100           PERFORM FAC-SKAPA-EKOFIL-RAD                                   
285200         END-IF                                                           
285300       END-IF                                                             
285400     END-IF                                                               
285500                                                                          
285600*-INVENTERING SKALL GÖRAS ÄVEN FÖR KOD 11/21 ,ALLA DC UTOM NDC-NA.        
285700*- FÖR KOD 26 GÄLLER SAMMA SOM KOD 21                                     
285800     IF NDC-NA OR ( LEV-KDANMORS = '31' )                                 
285900       CONTINUE                                                           
286000     ELSE                                                                 
286100       PERFORM CE-KOLLA-INVENTERING                                       
286200     END-IF                                                               
286300     .                                                                    
286400     EJECT                                                                
286500 DB-SKAPA-RADER-DR5 SECTION.                                              
286600*    DISPLAY '*** DB-SKAPA-RADER-DR5'                                     
286700                                                                          
286800     MOVE SPACE                     TO DR5-W418DR5                        
286900     MOVE 'DR5'                     TO DR5-IDPTYP                         
287000     MOVE ANM-IDDISTR               TO WS-IDDISTR                         
287100                                       TEST-IDDISTR                       
287200     MOVE WS-IDDISTR-ALFA           TO DR5-IDDISTR                        
287300     MOVE ANM-IDKUNDNR              TO WS-IDKUNDNR                        
287400     MOVE WS-IDKUNDNR-ALFA          TO DR5-IDKUNDNR                       
287500     MOVE ANM-IDRAPPNR              TO WS-IDRAPPNR                        
287600     MOVE WS-IDRAPPNR-ALFA          TO DR5-IDRAPPNR                       
287700                                                                          
287800     IF GOOD-DDC                                                          
287900       MOVE '11'                    TO DR5-IDDC                           
288000     ELSE                                                                 
288100       MOVE LEV-IDDC                TO DR5-IDDC                           
288200     END-IF                                                               
288300                                                                          
288400     MOVE IN-IDORDNR                TO DR5-IDORDNR5                       
288500     MOVE LEV-IDARTNR               TO DR5-IDARTNR                        
288600     MOVE LEV-KVLEVANM-BEKR         TO WS-KVLEVANM-BEKR                   
288700                                       VIR-KVLEVANM                       
288800     MOVE WS-KVLEVANM-BEKR-ALFA     TO DR5-KVBEART                        
288900                                                                          
289000     IF DIST79-DEALER-PRICE OR                                            
289100        DIST79-ECOM-PRICE                                                 
289200       IF LEV-PRARTBTO-LOC > +0                                           
289300          MOVE LEV-PRARTBTO-LOC       TO W-PRARTBTO-LOC                   
289400          MOVE W-PRARTBTO-HEL-LOC     TO W-PRARTBTO-X-HEL-LOC             
289500          MOVE W-PRARTBTO-DEC-LOC     TO W-PRARTBTO-X-DEC-LOC             
289600          MOVE W-PRARTBTO-X-LOC       TO DR5-PRARTNTO-LOC                 
289700       ELSE                                                               
289800          MOVE SPACE                  TO DR5-PRARTNTO-LOC                 
289900       END-IF                                                             
290000       MOVE SPACE                     TO DR5-PRARTNTO                     
290100     ELSE                                                                 
290200       IF LEV-PRARTBTO > +0  OR LEV-PRARTBTO > 0                          
290300          MOVE LEV-PRARTBTO           TO W-PRARTBTO                       
290400          MOVE W-PRARTBTO-HEL         TO W-PRARTBTO-X-HEL                 
290500          MOVE W-PRARTBTO-DEC         TO W-PRARTBTO-X-DEC                 
290600          MOVE W-PRARTBTO-X           TO DR5-PRARTNTO                     
290700       ELSE                                                               
290800          MOVE SPACE                  TO DR5-PRARTNTO                     
290900       END-IF                                                             
291000       MOVE SPACE                     TO DR5-PRARTNTO-LOC                 
291100     END-IF                                                               
291200                                                                          
291300     MOVE ANM-KDVALISO              TO DR5-KDVALISO                       
291400                                                                          
291500**** E'TRACKER 5984456 KOLLA FÖRST OM OK MED KLASS 1 FÖR LEV-IDDC.        
291600**** ANNARS FASTNAR ORDERN PÅ DISPATCHEN. 2007-12                         
291700**** MOVE 1                         TO DR5-KDORDKL                        
291800     MOVE ANM-IDDISTR               TO W-IDDISTR-WDB2                     
291900     MOVE ANM-IDKUNDNR              TO W-IDKUNDNR-WDB2                    
292000     PERFORM IMS-GET-WLGMTA01-UNIK                                        
292100     IF SEGMENT-FINNS                                                     
292200       PERFORM S66-KOLLA-OK-ORDERKLASS                                    
292300       IF KLASS-1-OK                                                      
292400         MOVE 1                     TO DR5-KDORDKL                        
292500       ELSE                                                               
292600         IF KLASS-0-OK                                                    
292700           MOVE 0                   TO DR5-KDORDKL                        
292800         ELSE                                                             
292900           MOVE 3                   TO DR5-KDORDKL                        
293000         END-IF                                                           
293100       END-IF                                                             
293200     END-IF                                                               
293300                                                                          
293400     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
293500     IF LEV-FLDIRLEV = JA               OR                                
293600        GOOD-DDC                        OR                                
293700        DIST35-REFILL-NA-JAP            OR                                
293800        DIST35-NONVCC-NONVCC-REFILL     OR                                
293900        DIST35-NONVCC-NONVCC-TRANSFER   OR                                
294000       (DIST07-USA-RETAILER AND CDC-SE) OR                                
294100       (DIST07-CAN-RETAILER AND CDC-SE) OR                                
294200       (DIST07-NON-VCC-OWNED AND CDC-SE) OR                               
294300       (LEV-KDANMORS = '61' OR '31' OR '26')                              
294400                                                                          
294500       MOVE NEJ                     TO DR5-FLLSBOK                        
294600     ELSE                                                                 
294700        IF DIST35-REFILL-NA                                               
294800          MOVE JA                   TO DR5-FLLSBOK                        
294900        ELSE                                                              
295000          IF LEV-IDDC NOT = DCS-IDDC                                      
295100            MOVE LEV-IDDC TO W-IDDC-B6                                    
295200            PERFORM IMS-GU-WDB601                                         
295300          END-IF                                                          
295400                                                                          
295500          IF (DCS-FTG-US OR DCS-FTG-CA OR                                 
295600              DCS-LAND-NON-VCC-OWNED)                                     
295700              OR                                                          
295800              CDC-SE                                                      
295900            MOVE JA                 TO DR5-FLLSBOK                        
296000          ELSE                                                            
296100            MOVE FLLSBOK-11-21-WS   TO DR5-FLLSBOK                        
296200                                       DR5-FLLSBOK-WS                     
296300          END-IF                                                          
296400        END-IF                                                            
296500     END-IF                                                               
296600                                                                          
296700*    1363345 CODE 61 SHALL NOT BOOK STOCK FOR USA                         
296800*    IF DIST35-REFILL-NA AND LEV-KDANMORS = '61'                          
296900*      IF LEV-FLDIRLEV = JA         OR                                    
297000*         GOOD-DDC                                                        
297100*        CONTINUE                                                         
297200*      ELSE                                                               
297300*        MOVE JA                    TO DR5-FLLSBOK                        
297400*      END-IF                                                             
297500*    END-IF                                                               
297600                                                                          
297700     MOVE LEV-IDRADNR               TO DR5-IDRADNR                        
297800                                                                          
297900     PERFORM S15-SKRIV-W41836                                             
298000                                                                          
298100     IF GOOD-DDC                                                          
298200       IF OKOD-FL-LEVERANTOER = JA                                        
298300         PERFORM S42-SKAPA-RADER-VIR                                      
298400       END-IF                                                             
298500     END-IF                                                               
298600     .                                                                    
298700     EJECT                                                                
298800 DC-KOLLA-FLLSBOK-KOD-11-21  SECTION.                                     
298900                                                                          
299000     IF LEV-IDDC NOT = DCS-IDDC                                           
299100        MOVE LEV-IDDC TO W-IDDC-B6                                        
299200        PERFORM IMS-GU-WDB601                                             
299300     END-IF                                                               
299400                                                                          
299500     IF LEV-FLDIRLEV = JA                           OR                    
299600        GOOD-DDC                                    OR                    
299700       (DIST07-USA-RETAILER AND CDC-SE)             OR                    
299800       (DIST07-CAN-RETAILER AND CDC-SE)             OR                    
299900       (DIST07-NON-VCC-OWNED AND CDC-SE)            OR                    
300000       (DCS-FTG-US OR DCS-FTG-CA                    OR                    
300100        DCS-LAND-NON-VCC-OWNED)                     OR                    
300200       (LEV-IDDC = WC-CDC-SE)                                             
300300                                                                          
300400       CONTINUE                                                           
300500     ELSE                                                                 
300600       MOVE LEV-IDARTNR      TO W-IDARTNR-K7                              
300700       MOVE LEV-IDDC         TO W-IDDC                                    
300800       PERFORM IMS-GU-WDK711                                              
300900       IF SEGMENT-FINNS                                                   
301000         COMPUTE SLAG-KVLS =                                              
301100                 SLAG-KVLS - LEV-KVLEVANM-BEKR                            
301200         END-COMPUTE                                                      
301300         IF SLAG-KVLS < 0                                                 
301400           MOVE NEJ          TO FLLSBOK-11-21-WS                          
301500           MOVE JA           TO MINUS-SALDO-SW                            
301600         ELSE                                                             
301700           MOVE JA           TO FLLSBOK-11-21-WS                          
301800         END-IF                                                           
301900       ELSE                                                               
302000         MOVE NEJ            TO FLLSBOK-11-21-WS                          
302100         MOVE JA             TO MINUS-SALDO-SW                            
302200       END-IF                                                             
302300     END-IF                                                               
302400     .                                                                    
302500     EJECT                                                                
302600 DD-KOLLA-OM-FLLSBOK-LIKA SECTION.                                        
302700                                                                          
302800     IF LEV-IDDC NOT = DCS-IDDC                                           
302900        MOVE LEV-IDDC TO W-IDDC-B6                                        
303000        PERFORM IMS-GU-WDB601                                             
303100     END-IF                                                               
303200                                                                          
303300     IF LEV-FLDIRLEV = JA               OR                                
303400        GOOD-DDC                        OR                                
303500       (DIST07-USA-RETAILER AND CDC-SE) OR                                
303600       (DIST07-CAN-RETAILER AND CDC-SE) OR                                
303700       (DIST07-NON-VCC-OWNED AND CDC-SE)                                  
303800                                                                          
303900       CONTINUE                                                           
304000     ELSE                                                                 
304100       IF (DCS-FTG-US OR DCS-FTG-CA                                       
304200       OR  DCS-LAND-NON-VCC-OWNED)                                        
304300       OR (LEV-IDDC = WC-CDC-SE)                                          
304400         CONTINUE                                                         
304500       ELSE                                                               
304600         IF DR5-FLLSBOK-WS  =  FLLSBOK-11-21-WS                           
304700           CONTINUE                                                       
304800         ELSE                                                             
304900           PERFORM S05-HAEMTA-ORDERNR                                     
305000         END-IF                                                           
305100       END-IF                                                             
305200     END-IF                                                               
305300     .                                                                    
305400     EJECT                                                                
305500 E-BEHANDLA-KREDITNOTA-DIREKT SECTION.                                    
305600*    DISPLAY '*** E-BEHANDLA-KREDITNOTA-DIREKT'                           
305700                                                                          
305800     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
305900                                                                          
306000     PERFORM S100-NOLLA-720-AREA                                          
306100     IF LEV-KDANMORS = '00' OR '13' OR '20' OR '23' OR '40' OR            
306200                       '60' OR '80' OR '90' OR '97' OR '70' OR            
306300                       '25' OR '28'                                       
306400        IF LEV-KDANMORS = ('13' OR '23' OR '28')                          
306500           PERFORM EB-FELLEVERANS-SKROT                                   
306600        ELSE                                                              
306700           PERFORM EA-KNOTA-DIREKT                                        
306800        END-IF                                                            
306900     ELSE                                                                 
307000        IF LEV-KDANMORS = '30' OR '96'                                    
307100           PERFORM  EC-KNOTA-PRISFEL                                      
307200        ELSE                                                              
307300           IF LEV-KDANMORS = '43' OR '53' OR '55' OR '63' OR              
307400                             '73' OR '83' OR '93'                         
307500              PERFORM ED-KNOTA-SKROT                                      
307600           END-IF                                                         
307700        END-IF                                                            
307800     END-IF                                                               
307900     .                                                                    
308000     EJECT                                                                
308100 EA-KNOTA-DIREKT SECTION.                                                 
308200*    DISPLAY '*** EA-KNOTA-DIREKT '                                       
308300**** KOD 00, 20, 40, 60, 80,90,70, 97(PÅ SDC,NDC-PACIFIC,LDC).            
308400**** UPPDATERAR LS OM EJ DIRLEV. OBS ! KOD 25 BOKAR INTE NER LS.          
308500**** OM INVENTERINGSGRÄNS UPPNÅDD GÖRS INVENTERING.                       
308600**** SKAPAR KONTERINGAR TILL EKONOMI-LAB                                  
308700****                                                                      
308800**** OM DISTR. 8141-8143 + 8151 OCH KOD 60 BOKAS SALDO ENLIGT             
308900**** SUSSI,TUULA ETC. 2002-09-20.KOD 61 RÖR OCKSÅ SALDO HÄR.              
309000**** FAKTURA 2 VID EXPORT FRÅN NDC-CN/NDC-NA SKALL EJ RÖRA LS.            
309100                                                                          
309200                                                                          
309300     MOVE ANM-IDFTG   TO WS-IDFTG                                         
309400                                                                          
309500*- NDC-NA MED IDFTG=53/54 (LUFTFAKTUROR) SKALL EJ GÅ VIA BILL-IT.         
309600     IF IDFTG-PV OR IDFTG-NON-VCC                                         
309700       CONTINUE                                                           
309800     ELSE                                                                 
309900       EVALUATE TRUE                                                      
310000          WHEN CDC-SE                                                     
310100             IF W-IDKNOTNR-CDC = +0                                       
310200                PERFORM S40-KNOTNR                                        
310300             END-IF                                                       
310400          WHEN SDC                                                        
310500             IF W-IDKNOTNR-SDC = +0                                       
310600                PERFORM S40-KNOTNR                                        
310700             END-IF                                                       
310800          WHEN NDC-US                                                     
310900             IF W-IDKNOTNR-USA = +0                                       
311000                PERFORM S40-KNOTNR                                        
311100             END-IF                                                       
311200          WHEN NDC-CA                                                     
311300             IF W-IDKNOTNR-CAN = +0                                       
311400                PERFORM S40-KNOTNR                                        
311500             END-IF                                                       
311600          WHEN NDC-JP                                                     
311700             IF W-IDKNOTNR-JAP = +0                                       
311800                PERFORM S40-KNOTNR                                        
311900             END-IF                                                       
312000          WHEN NDC-AU                                                     
312100             IF W-IDKNOTNR-AUS = +0                                       
312200                PERFORM S40-KNOTNR                                        
312300             END-IF                                                       
312400          WHEN DDC-SE                                                     
312500             IF W-IDKNOTNR-SE = +0                                        
312600                PERFORM S40-KNOTNR                                        
312700             END-IF                                                       
312800          WHEN DDC-NO                                                     
312900             IF W-IDKNOTNR-NO = +0                                        
313000                PERFORM S40-KNOTNR                                        
313100             END-IF                                                       
313200          WHEN DDC-BE                                                     
313300             IF W-IDKNOTNR-BE = +0                                        
313400                PERFORM S40-KNOTNR                                        
313500             END-IF                                                       
313600       END-EVALUATE                                                       
313700                                                                          
313800       PERFORM EAB-SKRIV-UPPDAT-POST-KREE                                 
313900     END-IF                                                               
314000                                                                          
314100     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
314200     IF LEV-FLDIRLEV = JA                               OR                
314300        GOOD-DDC                                        OR                
314400        DIST35-REFILL-NA-JAP                            OR                
314500       (DIST07-USA-RETAILER AND CDC-SE)                 OR                
314600       (DIST07-CAN-RETAILER AND CDC-SE)                 OR                
314700       (DIST07-NON-VCC-OWNED AND CDC-SE)                OR                
314800        DIST35-NONVCC-NONVCC-TRANSFER                   OR                
314900        DIST35-NONVCC-NONVCC-REFILL                                       
315000                                                                          
315100        MOVE ZERO                   TO  31B-KDAVVTYP                      
315200        MOVE NEJ                    TO  WS-FLLSBOK                        
315300     ELSE                                                                 
315400        MOVE ZERO                   TO 31B-KDAVVTYP                       
315500        IF LEV-FLDIRLEV = NEJ                                             
315600           IF ( LEV-KDANMORS = '00' OR '20' OR '90' OR '97' )             
315700*    1363345 CODE 60 SHALL NOT BOOK STOCK FOR USA                         
315800*             ( DIST35-REFILL-NA AND LEV-KDANMORS = '60' )                
315900              IF ART-KDERS-UTG = +0                                       
316000                 MOVE +1             TO UT34-KDAVVTYP                     
316100                                        31B-KDAVVTYP                      
316200*- SALDOBOKNING GÖRS I PGM W4183300 FÖR BILLIT-KREDITERINGAR              
316300                 IF IDFTG-PV OR IDFTG-NON-VCC                             
316400                   CONTINUE                                               
316500                 ELSE                                                     
316600                   PERFORM S50-SKRIV-UPPDATPOST-KVLS                      
316700                 END-IF                                                   
316800                 MOVE JA             TO 720-FLLSBOK                       
316900                                         WS-FLLSBOK                       
317000              END-IF                                                      
317100           END-IF                                                         
317200                                                                          
317300           IF LEV-KDANMORS = '25'                                         
317400             MOVE NEJ                TO WS-FLLSBOK                        
317500           END-IF                                                         
317600        END-IF                                                            
317700     END-IF                                                               
317800                                                                          
317900     IF IDFTG-PV OR IDFTG-NON-VCC                                         
318000       CONTINUE                                                           
318100     ELSE                                                                 
318200       IF CDC-SE                                                          
318300          IF SKRIV-HUV-KNOTA-CDC                                          
318400             PERFORM S35-SKAPA-HUVUD-KNOTA                                
318500          END-IF                                                          
318600       ELSE                                                               
318700         EVALUATE TRUE                                                    
318800           WHEN DDC-SE                                                    
318900             IF SKRIV-HUV-KNOTA-SE                                        
319000               PERFORM S35-SKAPA-HUVUD-KNOTA                              
319100             END-IF                                                       
319200           WHEN DDC-NO                                                    
319300             IF SKRIV-HUV-KNOTA-NO                                        
319400               PERFORM S35-SKAPA-HUVUD-KNOTA                              
319500             END-IF                                                       
319600           WHEN DDC-BE                                                    
319700             IF SKRIV-HUV-KNOTA-BE                                        
319800               PERFORM S35-SKAPA-HUVUD-KNOTA                              
319900             END-IF                                                       
320000           WHEN OTHER                                                     
320100            IF SKRIV-HUV-KNOTA-SDC                                        
320200              PERFORM S35-SKAPA-HUVUD-KNOTA                               
320300            END-IF                                                        
320400         END-EVALUATE                                                     
320500       END-IF                                                             
320600                                                                          
320700       PERFORM S36-SKAPA-RADPOST-KNOTA                                    
320800       PERFORM S60-RADPOST-EKO-LAB                                        
320900                                                                          
321000     END-IF                                                               
321100                                                                          
321200     IF NDC-US OR NDC-CA                                                  
321300     OR XDC-NON-VCC-OWNED                                                 
321400        MOVE LEV-IDDC               TO W-IDDC                             
321500        PERFORM IMS-GU-WDK711                                             
321600        IF SEGMENT-FINNS                                                  
321700           COMPUTE WS-INVVARDE ROUNDED =                                  
321800                   LEV-KVLEVANM-BEKR * SLAG-PRAVCOST                      
321900           END-COMPUTE                                                    
322000        ELSE                                                              
322100           MOVE +0                  TO WS-INVVARDE                        
322200        END-IF                                                            
322300     ELSE                                                                 
322400        COMPUTE WS-INVVARDE ROUNDED =                                     
322500                LEV-KVLEVANM-BEKR * CLAG-PRARTSTD                         
322600        END-COMPUTE                                                       
322700     END-IF                                                               
322800                                                                          
322900     MOVE 'N'   TO  31B-FLINVUPD                                          
323000                                                                          
323100     IF LEV-IDDC NOT = DCS-IDDC                                           
323200       MOVE LEV-IDDC TO W-IDDC-B6                                         
323300       PERFORM IMS-GU-WDB601                                              
323400     END-IF                                                               
323500                                                                          
323600     EVALUATE TRUE                                                        
323700        WHEN CDC-SE                                                       
323800           IF WS-INVVARDE > DCS-SUINVGRANS                                
323900              IF LEV-FLDIRLEV = JA                                        
324000                CONTINUE                                                  
324100              ELSE                                                        
324200                 IF ART-KDERS-UTG = +0                                    
324300                    IF CLAG-KDVVKL NOT = +4 AND +5                        
324400                       PERFORM S65-UPPD-INVENTERING                       
324500                       MOVE 'J'   TO  31B-FLINVUPD                        
324600                    END-IF                                                
324700                 ELSE                                                     
324800                    PERFORM S65-UPPD-INVENTERING                          
324900                    MOVE 'J'   TO  31B-FLINVUPD                           
325000                 END-IF                                                   
325100              END-IF                                                      
325200           END-IF                                                         
325300        WHEN SDC                                                          
325400           IF OKOD-FL-INVENT-SDC = JA                                     
325500              IF WS-INVVARDE > DCS-SUINVGRANS                             
325600                 IF LEV-FLDIRLEV = JA                                     
325700                   CONTINUE                                               
325800                 ELSE                                                     
325900                    PERFORM S65-UPPD-INVENTERING                          
326000                    MOVE 'J'   TO  31B-FLINVUPD                           
326100                 END-IF                                                   
326200              END-IF                                                      
326300           END-IF                                                         
326400        WHEN NDC                                                          
326500           IF XDC-NON-VCC-OWNED                                           
326600             IF WS-INVVARDE > DCS-SUINVGRANS                              
326700                IF LEV-FLDIRLEV = JA                                      
326800                  CONTINUE                                                
326900                ELSE                                                      
327000                   PERFORM S65-UPPD-INVENTERING                           
327100                   MOVE 'J'   TO  31B-FLINVUPD                            
327200                END-IF                                                    
327300             END-IF                                                       
327400           ELSE                                                           
327500             IF WS-INVVARDE > DCS-SUINVGRANS                              
327600                IF LEV-FLDIRLEV = JA                                      
327700                  CONTINUE                                                
327800                ELSE                                                      
327900                   PERFORM S65-UPPD-INVENTERING                           
328000                   MOVE 'J'   TO  31B-FLINVUPD                            
328100                END-IF                                                    
328200             END-IF                                                       
328300           END-IF                                                         
328400        WHEN LDC                                                          
328500           IF OKOD-FL-INVENT-SDC = JA                                     
328600               IF WS-INVVARDE > DCS-SUINVGRANS                            
328700                  IF LEV-FLDIRLEV = JA                                    
328800                    CONTINUE                                              
328900                  ELSE                                                    
329000                     PERFORM S65-UPPD-INVENTERING                         
329100                     MOVE 'J'   TO  31B-FLINVUPD                          
329200                  END-IF                                                  
329300               END-IF                                                     
329400           END-IF                                                         
329500     END-EVALUATE                                                         
329600                                                                          
329700     IF IDFTG-PV OR IDFTG-NON-VCC                                         
329800       MOVE JA     TO KNOTA-VIA-BILLIT-SW                                 
329900       PERFORM S46-SKAPA-KN-RAD-BILLIT                                    
330000     END-IF                                                               
330100     .                                                                    
330200     EJECT                                                                
330300 EAB-SKRIV-UPPDAT-POST-KREE SECTION.                                      
330400*    DISPLAY '*** EAB-SKRIV-UPPDAT-POST-KREE'                             
330500*                                                                         
330600*** UPPDATERAR WDA211 MED KNOTANR OCH                                     
330700*   PRARTBTO OM KOD=80 OCH PRIS IN VAR +0                                 
330800*                                                                         
330900     MOVE '004'                     TO UT34-IDPTYP                        
331000     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
331100     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
331200     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
331300     MOVE SPACE                     TO UT34-IDDC                          
331400     MOVE SPACE                     TO UT34-FLFARLIG                      
331500     EVALUATE TRUE                                                        
331600        WHEN CDC-SE                                                       
331700           MOVE W-IDKNOTNR-CDC      TO UT34-IDKNOTNR                      
331800        WHEN SDC                                                          
331900           MOVE W-IDKNOTNR-SDC      TO UT34-IDKNOTNR                      
332000        WHEN NDC-US                                                       
332100           MOVE W-IDKNOTNR-USA      TO UT34-IDKNOTNR                      
332200        WHEN NDC-CA                                                       
332300           MOVE W-IDKNOTNR-CAN      TO UT34-IDKNOTNR                      
332400        WHEN NDC-JP                                                       
332500           MOVE W-IDKNOTNR-JAP      TO UT34-IDKNOTNR                      
332600        WHEN NDC-AU                                                       
332700           MOVE W-IDKNOTNR-AUS      TO UT34-IDKNOTNR                      
332800        WHEN DDC-SE                                                       
332900           MOVE W-IDKNOTNR-SE       TO UT34-IDKNOTNR                      
333000        WHEN DDC-NO                                                       
333100           MOVE W-IDKNOTNR-NO       TO UT34-IDKNOTNR                      
333200        WHEN DDC-BE                                                       
333300           MOVE W-IDKNOTNR-BE       TO UT34-IDKNOTNR                      
333400     END-EVALUATE                                                         
333500     MOVE ZERO                      TO UT34-KDAVVTYP                      
333600     MOVE 'C'                       TO UT34-KDFAKTYP-KNOT                 
333700     MOVE SPACE                     TO UT34-KDLEVANM                      
333800     MOVE ZERO                      TO UT34-KVLEVANM                      
333900     MOVE DAGENS-DATUM              TO UT34-TIKNOTA                       
334000     MOVE ZERO                      TO UT34-TIRETILL                      
334100                                       UT34-KVRADER-RT                    
334200     MOVE LEV-PRARTBTO              TO UT34-PRARTBTO                      
334300     MOVE ZERO                      TO UT34-KDINVKAT                      
334400     MOVE ZERO                      TO UT34-KVJUSTKV                      
334500     MOVE ZERO                      TO UT34-TIM-INV                       
334600     MOVE SPACE                     TO UT34-TEINVANM                      
334700     MOVE SPACE                     TO UT34-KDARBTYP                      
334800     MOVE ZERO                      TO UT34-IDPERSON                      
334900     MOVE SPACE                     TO UT34-IDDC-RET                      
335000     MOVE ZERO                      TO UT34-IXDCCLEAR                     
335100                                                                          
335200     PERFORM S20-SKRIV-W41834                                             
335300     .                                                                    
335400     EJECT                                                                
335500 EB-FELLEVERANS-SKROT SECTION.                                            
335600*    DISPLAY '*** EB-FELLEVERANS-SKROT'                                   
335700                                                                          
335800                                                                          
335900     MOVE NEJ                       TO 720-FLLSBOK                        
336000     MOVE NEJ                       TO EKHT-FLLSBOK                       
336100     MOVE NEJ                       TO MINUS-SALDO-SW                     
336200     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
336300     IF LEV-FLDIRLEV = JA                OR                               
336400        LEV-IDARTNR = 100                OR                               
336500        GOOD-DDC                         OR                               
336600        DIST35-REFILL-NA-JAP             OR                               
336700       (DIST07-USA-RETAILER AND CDC-SE)  OR                               
336800       (DIST07-CAN-RETAILER AND CDC-SE)  OR                               
336900       (DIST07-NON-VCC-OWNED AND CDC-SE)                                  
337000                                                                          
337100        CONTINUE                                                          
337200     ELSE                                                                 
337300        IF LEV-FLDIRLEV = NEJ                                             
337400           IF ART-KDERS-UTG = +0                                          
337500             IF LEV-KDANMORS = '28'                                       
337600               CONTINUE                                                   
337700             ELSE                                                         
337800               IF LEV-IDDC NOT = DCS-IDDC                                 
337900                  MOVE LEV-IDDC TO W-IDDC-B6                              
338000                  PERFORM IMS-GU-WDB601                                   
338100               END-IF                                                     
338200                                                                          
338300               IF ( DCS-FTG-US OR DCS-FTG-CA OR                           
338400                    DCS-LAND-NON-VCC-OWNED )                              
338500               OR ( LEV-IDDC = WC-CDC-SE )                                
338600                 MOVE +2               TO UT34-KDAVVTYP                   
338700                 PERFORM S50-SKRIV-UPPDATPOST-KVLS                        
338800                 MOVE JA               TO 720-FLLSBOK                     
338900                 MOVE JA               TO EKHT-FLLSBOK                    
339000               ELSE                                                       
339100                 MOVE LEV-IDARTNR      TO W-IDARTNR-K7                    
339200                 MOVE LEV-IDDC         TO W-IDDC                          
339300                 PERFORM IMS-GU-WDK711                                    
339400                 IF SEGMENT-FINNS                                         
339500                   COMPUTE SLAG-KVLS =                                    
339600                           SLAG-KVLS - LEV-KVLEVANM-BEKR                  
339700                   END-COMPUTE                                            
339800                   IF SLAG-KVLS < 0                                       
339900                     MOVE JA TO MINUS-SALDO-SW                            
340000                   ELSE                                                   
340100                     MOVE +2               TO UT34-KDAVVTYP               
340200                     PERFORM S50-SKRIV-UPPDATPOST-KVLS                    
340300                     MOVE JA               TO 720-FLLSBOK                 
340400                     MOVE JA               TO EKHT-FLLSBOK                
340500                   END-IF                                                 
340600                 ELSE                                                     
340700                   MOVE JA                 TO MINUS-SALDO-SW              
340800                 END-IF                                                   
340900               END-IF                                                     
341000             END-IF                                                       
341100           END-IF                                                         
341200        END-IF                                                            
341300     END-IF                                                               
341400                                                                          
341500     IF NDC-NA                                                            
341600       PERFORM EBA-SKROTPOST-EKO-LAB                                      
341700     END-IF                                                               
341800                                                                          
341900     IF SKRIV-HUV-EKOFIL                                                  
342000       MOVE LEV-IDDC TO WS-IDDC                                           
342100       IF XDC-NON-VCC-OWNED                                               
342200         CONTINUE                                                         
342300       ELSE                                                               
342400         PERFORM EBB-FLYTTA-SKRIV-EKOFIL-HUVUD                            
342500       END-IF                                                             
342600       PERFORM EBC-FLYTTA-SKRIV-EKOFIL-RADER                              
342700       IF MINUS-SALDO-SW = JA                                             
342800         PERFORM EBD-FLYTTA-SKRIV-EKOFIL-403                              
342900       END-IF                                                             
343000       MOVE NEJ      TO SKRIV-HUV-EKOFIL-SW                               
343100     ELSE                                                                 
343200       PERFORM EBC-FLYTTA-SKRIV-EKOFIL-RADER                              
343300       IF MINUS-SALDO-SW = JA                                             
343400         PERFORM EBD-FLYTTA-SKRIV-EKOFIL-403                              
343500       END-IF                                                             
343600     END-IF                                                               
343700                                                                          
343800     PERFORM CE-KOLLA-INVENTERING                                         
343900     .                                                                    
344000     EJECT                                                                
344100 EBA-SKROTPOST-EKO-LAB  SECTION.                                          
344200                                                                          
344300     MOVE '72D'                     TO 720-IDPTYP                         
344400                                       W-IDPTYP                           
344500                                                                          
344600     IF GOOD-DDC                                                          
344700       MOVE '11'                    TO 720-IDDC                           
344800     ELSE                                                                 
344900       MOVE LEV-IDDC                TO 720-IDDC                           
345000     END-IF                                                               
345100                                                                          
345200     MOVE ANM-IDDISTR               TO 720-IDDISTR                        
345300                                       TEST-IDDISTR                       
345400     MOVE ANM-IDKUNDNR              TO 720-IDKUNDNR                       
345500*-KOD 13 OCH 23 GER INGEN KREDIT.                                         
345600     IF LEV-PRARTBTO = +0 OR LEV-PRARTBTO-LOC = +0                        
345700        MOVE +0                     TO 720-IDKNOTNR                       
345800     ELSE                                                                 
345900        EVALUATE TRUE                                                     
346000           WHEN CDC-SE                                                    
346100              MOVE W-IDKNOTNR-CDC   TO 720-IDKNOTNR                       
346200           WHEN SDC                                                       
346300              MOVE W-IDKNOTNR-SDC   TO 720-IDKNOTNR                       
346400           WHEN NDC-US                                                    
346500              MOVE W-IDKNOTNR-USA   TO 720-IDKNOTNR                       
346600           WHEN NDC-CA                                                    
346700              MOVE W-IDKNOTNR-CAN   TO 720-IDKNOTNR                       
346800           WHEN NDC-JP                                                    
346900              MOVE W-IDKNOTNR-JAP   TO 720-IDKNOTNR                       
347000           WHEN NDC-AU                                                    
347100              MOVE W-IDKNOTNR-AUS   TO 720-IDKNOTNR                       
347200           WHEN DDC-SE                                                    
347300              MOVE W-IDKNOTNR-SE    TO 720-IDKNOTNR                       
347400           WHEN DDC-NO                                                    
347500              MOVE W-IDKNOTNR-NO    TO 720-IDKNOTNR                       
347600           WHEN DDC-BE                                                    
347700              MOVE W-IDKNOTNR-BE    TO 720-IDKNOTNR                       
347800        END-EVALUATE                                                      
347900     END-IF                                                               
348000     MOVE ANM-IDRAPPNR              TO 720-IDRAPPNR                       
348100     MOVE DAGENS-DATUM-SEKEL        TO 720-DAKRENOT                       
348200     MOVE LEV-IDARTNR               TO 720-IDARTNR                        
348300     MOVE LEV-KDANMORS              TO 720-KDANMORS                       
348400     COMPUTE 720-KVKREANT = LEV-KVLEVANM-BEKR * -1                        
348500     MOVE ART-KDPRODSL              TO 720-KDPRODSL                       
348600     MOVE CLAG-KDPSLLOC             TO 720-KDPSLLOC                       
348700                                                                          
348800     IF DIST79-DEALER-PRICE OR                                            
348900        DIST79-ECOM-PRICE                                                 
349000       MOVE LEV-PRARTBTO-LOC        TO 720-PRARTNTO                       
349100     ELSE                                                                 
349200       MOVE LEV-PRARTBTO            TO 720-PRARTNTO                       
349300     END-IF                                                               
349400                                                                          
349500     IF NDC-US OR NDC-CA                                                  
349600        MOVE LEV-IDDC               TO W-IDDC                             
349700        PERFORM IMS-GU-WDK711                                             
349800        IF SEGMENT-FINNS                                                  
349900           MOVE SLAG-PRAVCOST       TO 720-PRAVCOST                       
350000        ELSE                                                              
350100           MOVE +0                  TO 720-PRAVCOST                       
350200        END-IF                                                            
350300     ELSE                                                                 
350400        MOVE +0                     TO 720-PRAVCOST                       
350500     END-IF                                                               
350600                                                                          
350700     PERFORM S13-SKRIV-W41833                                             
350800                                                                          
350900     .                                                                    
351000     EJECT                                                                
351100 EBB-FLYTTA-SKRIV-EKOFIL-HUVUD SECTION.                                   
351200                                                                          
351300     MOVE '303'                   TO EKHT-KDEKHHT                         
351400     MOVE '3XX'                   TO EKHT-KDEKSHT                         
351500     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
351600     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
351700     MOVE 1                       TO EKHT-IDSEKVNR                        
351800     MOVE 'SUM'                   TO EKHT-KDEKNIVA                        
351900     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
352000     MOVE ' '                     TO EKHT-IDDC-REC                        
352100     MOVE ANM-IDDISTR             TO EKHT-IDDISTR                         
352200     MOVE ANM-IDKUNDNR            TO EKHT-IDKUNDNR                        
352300                                                                          
352400     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
352500     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
352600     CALL W009CIA USING           CIA-W009CIA                             
352700     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
352800                                                                          
352900     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
353000     MOVE 0                       TO EKHT-KDPRODSL                        
353100     MOVE 0                       TO EKHT-KDPSLLOC                        
353200     MOVE 0                       TO EKHT-IDARTNR                         
353300**** SE SECTION EB-FELLEV...      TO EKHT-FLLSBOK                         
353400     MOVE 'SEK'                   TO EKHT-KDVALISO                        
353500     MOVE 1.0                     TO EKHT-PRKURS                          
353600     MOVE 0                       TO EKHT-PRARTNTO                        
353700     MOVE 0                       TO EKHT-PRARTSJK                        
353800     MOVE 0                       TO EKHT-PRHEMTAG                        
353900     MOVE 0                       TO EKHT-PRARTSTD                        
354000     MOVE 0                       TO EKHT-PRLANDCO                        
354100     MOVE 0                       TO EKHT-PRINK                           
354200     MOVE 0                       TO EKHT-PRDIRLON                        
354300     MOVE 0                       TO EKHT-PRDMTRL                         
354400     MOVE 0                       TO EKHT-PROVRPAL                        
354500     MOVE 0                       TO EKHT-KVANTAL                         
354600     MOVE 'W4183000'              TO EKHT-IDPGM                           
354700     MOVE ' '                     TO EKHT-IDTRANS                         
354800                                                                          
354900     COMPUTE EKHT-SUBEL = LEV-KVLEVANM-BEKR                               
355000                       * CLAG-PRARTSTD                                    
355100                                                                          
355200     MOVE 'W510EKHA'             TO EKHT-IDCPYTXT                         
355300     MOVE SPACE                  TO EKHT-IDANALYS                         
355400                                    EKHT-IDKST                            
355500     MOVE ZERO                   TO EKHT-BEVAT                            
355600                                    EKHT-IDKONTO                          
355700                                    EKHT-KDANMORS                         
355800                                    EKHT-KDFRAKT                          
355900                                    EKHT-SUVAT                            
356000     MOVE ZERO                   TO EKHT-DAAVIDAT                         
356100                                    EKHT-IDAVINR                          
356200                                    EKHT-KDAVVTYP                         
356300                                    EKHT-KDRT                             
356400                                    EKHT-KVANTMOT                         
356500                                    EKHT-KVAVIS                           
356600     MOVE WS-KDSORT              TO EKHT-KDSORT                           
356700     MOVE SPACE                  TO EKHT-KDTRADP                          
356800                                    EKHT-IDLEVNR                          
356900     MOVE SPACE                  TO EKHT-FLOVRLEV                         
357000     MOVE ZERO                   TO EKHT-IDORDNR5                         
357100     MOVE SPACE                  TO EKHT-IDUSER                           
357200     MOVE NEJ                    TO EKHT-FLDCET                           
357300     MOVE SPACE                  TO EKHT-IDKUNDRF                         
357400     MOVE LEV-IDFAKT             TO EKHT-IDFAKT-EXP                       
357500                                                                          
357600     PERFORM S06-SKRIV-EKONOMIFIL                                         
357700     .                                                                    
357800     EJECT                                                                
357900 EBC-FLYTTA-SKRIV-EKOFIL-RADER SECTION.                                   
358000                                                                          
358100     MOVE LEV-IDDC                TO WS-IDDC                              
358200     IF XDC-NON-VCC-OWNED                                                 
358300       IF LEV-KDANMORS = '13' OR '23'                                     
358400         IF NDC-IN                                                        
358500           PERFORM EBCA-FLYTT-SKRIV-EKOFIL-RAD-IN                         
358600         ELSE                                                             
358700           PERFORM EBCA-FLYTT-SKRIV-EKOFIL-RAD-SC                         
358800         END-IF                                                           
358900       END-IF                                                             
359000     ELSE                                                                 
359100       PERFORM EBCB-FLYTTA-SKRIV-EKOFIL-RADER                             
359200     END-IF                                                               
359300     .                                                                    
359400     EJECT                                                                
359500 EBCA-FLYTT-SKRIV-EKOFIL-RAD-SC SECTION.                                  
359600                                                                          
359700     MOVE '303'                   TO SC-EKHT-KDEKHHT                      
359800     MOVE '310'                   TO SC-EKHT-KDEKSHT                      
359900                                                                          
360000     MOVE 'DET'                   TO SC-EKHT-KDEKNIVA                     
360100     MOVE FUNCTION CURRENT-DATE (1:8) TO SC-EKHT-TIREGDAT                 
360200     MOVE FUNCTION CURRENT-DATE (9:8) TO SC-EKHT-TIKLOCK                  
360300     MOVE 1                       TO SC-EKHT-IDSEKVNR                     
360400     MOVE LEV-IDDC                TO SC-EKHT-IDDC-SEND                    
360500     MOVE ' '                     TO SC-EKHT-IDDC-REC                     
360600     MOVE ANM-IDDISTR             TO SC-EKHT-IDDISTR                      
360700     MOVE ANM-IDKUNDNR            TO SC-EKHT-IDKUNDNR                     
360800                                                                          
360900     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
361000     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
361100     CALL W009CIA USING           CIA-W009CIA                             
361200     MOVE CIA-IDARTBET-UT      TO SC-EKHT-IDVERGL                         
361300                                                                          
361400     MOVE DAGENS-DATUM-SEKEL      TO SC-EKHT-DAVERDAT                     
361500     MOVE 0                       TO SC-EKHT-KDPRODSL                     
361600     MOVE 0                       TO SC-EKHT-KDPSLLOC                     
361700     MOVE LEV-IDARTNR             TO SC-EKHT-IDARTNR                      
361800     MOVE JA                      TO SC-EKHT-FLLSBOK                      
361900     MOVE 1.0                     TO SC-EKHT-PRKURS                       
362000     MOVE 0                       TO SC-EKHT-PRARTNTO                     
362100     MOVE 0                       TO SC-EKHT-PRARTSJK                     
362200     MOVE 0                       TO SC-EKHT-PRHEMTAG                     
362300                                                                          
362400     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
362500     MOVE LEV-IDDC         TO W-IDDC                                      
362600     PERFORM IMS-GU-WDK711                                                
362700     IF SEGMENT-FINNS                                                     
362800       MOVE SLAG-PRAVCOST         TO SC-EKHT-PRARTSTD                     
362900     ELSE                                                                 
363000       MOVE 0                     TO SC-EKHT-PRARTSTD                     
363100     END-IF                                                               
363200                                                                          
363300     MOVE 0                       TO SC-EKHT-PRINK                        
363400     MOVE 0                       TO SC-EKHT-PRDIRLON                     
363500     MOVE 0                       TO SC-EKHT-PRDMTRL                      
363600     MOVE 0                       TO SC-EKHT-PROVRPAL                     
363700     MOVE 0                       TO SC-EKHT-PRLANDCO                     
363800     MOVE LEV-KVLEVANM-BEKR       TO SC-EKHT-KVANTAL                      
363900                                     VIR-KVLEVANM                         
364000     MOVE 0                       TO SC-EKHT-SUBEL                        
364100     MOVE 'W4183000'              TO SC-EKHT-IDPGM                        
364200     MOVE ' '                     TO SC-EKHT-IDTRANS                      
364300     MOVE LEV-KDANMORS            TO SC-EKHT-KDANMORS                     
364400     MOVE LEV-IDANALYS            TO SC-EKHT-IDANALYS                     
364500     MOVE LEV-IDKONTO             TO SC-EKHT-IDKONTO                      
364600     MOVE LEV-IDKST               TO SC-EKHT-IDKST                        
364700     MOVE ZERO                    TO SC-EKHT-BEVAT                        
364800                                     SC-EKHT-KDFRAKT                      
364900                                     SC-EKHT-SUVAT                        
365000     MOVE ZERO                    TO SC-EKHT-DAAVIDAT                     
365100                                     SC-EKHT-IDAVINR                      
365200                                     SC-EKHT-KDAVVTYP                     
365300                                     SC-EKHT-KDRT                         
365400                                     SC-EKHT-KVANTMOT                     
365500                                     SC-EKHT-KVAVIS                       
365600     MOVE WS-KDSORT               TO SC-EKHT-KDSORT                       
365700     MOVE SPACE                   TO SC-EKHT-IDLEVNR                      
365800     MOVE SPACE                   TO SC-EKHT-FLOVRLEV                     
365900     MOVE ZERO                    TO SC-EKHT-IDORDNR5                     
366000     MOVE SPACE                   TO SC-EKHT-IDUSER                       
366100     MOVE NEJ                     TO SC-EKHT-FLDCET                       
366200     MOVE SPACE                   TO SC-EKHT-IDKUNDRF                     
366300     MOVE LEV-IDFAKT              TO SC-EKHT-IDFAKT-EXP                   
366400     MOVE DCS-KDVALISO            TO SC-EKHT-KDVALISO                     
366500     MOVE DCS-KDTRADP             TO SC-EKHT-KDTRADP                      
366600     IF NDC-CN                                                            
366700       MOVE 'W570'                TO SC-EKHT-IDCPYTXT(1:4)                
366800     ELSE                                                                 
366900       MOVE DCS-KDTRADP           TO SC-EKHT-IDCPYTXT(1:4)                
367000     END-IF                                                               
367100     MOVE 'EKHA'                  TO SC-EKHT-IDCPYTXT(5:4)                
367200                                                                          
367300     IF XDC-NON-VCC-OWNED                                                 
367400       PERFORM S07-SKRIV-EKONOMIFIL                                       
367500     END-IF                                                               
367600                                                                          
367700     IF GOOD-DDC                                                          
367800       IF OKOD-FL-LEVERANTOER = JA                                        
367900          MOVE ZERO   TO VIR-IDKNOTNR                                     
368000          PERFORM S42-SKAPA-RADER-VIR                                     
368100       END-IF                                                             
368200     END-IF                                                               
368300     .                                                                    
368400     EJECT                                                                
368500                                                                          
368600 EBCA-FLYTT-SKRIV-EKOFIL-RAD-IN SECTION.                                  
368700     MOVE '303'                   TO IN-EKHT-KDEKHHT                      
368800     MOVE '310'                   TO IN-EKHT-KDEKSHT                      
368900                                                                          
369000     MOVE 'DET'                   TO IN-EKHT-KDEKNIVA                     
369100     MOVE FUNCTION CURRENT-DATE (1:8) TO IN-EKHT-TIREGDAT                 
369200     MOVE FUNCTION CURRENT-DATE (9:8) TO IN-EKHT-TIKLOCK                  
369300     MOVE 1                       TO IN-EKHT-IDSEKVNR                     
369400     MOVE LEV-IDDC                TO IN-EKHT-IDDC-SEND                    
369500     MOVE ' '                     TO IN-EKHT-IDDC-REC                     
369600     MOVE ANM-IDDISTR             TO IN-EKHT-IDDISTR                      
369700     MOVE ANM-IDKUNDNR            TO IN-EKHT-IDKUNDNR                     
369800                                                                          
369900     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
370000     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
370100     CALL W009CIA USING           CIA-W009CIA                             
370200     MOVE CIA-IDARTBET-UT      TO IN-EKHT-IDVERGL                         
370300                                                                          
370400     MOVE DAGENS-DATUM-SEKEL      TO IN-EKHT-DAVERDAT                     
370500     MOVE 0                       TO IN-EKHT-KDPRODSL                     
370600     MOVE 0                       TO IN-EKHT-KDPSLLOC                     
370700     MOVE LEV-IDARTNR             TO IN-EKHT-IDARTNR                      
370800     MOVE JA                      TO IN-EKHT-FLLSBOK                      
370900     MOVE 'INR'                   TO IN-EKHT-KDVALISO                     
371000     MOVE 1.0                     TO IN-EKHT-PRKURS                       
371100     MOVE 0                       TO IN-EKHT-PRARTNTO                     
371200     MOVE 0                       TO IN-EKHT-PRARTSJK                     
371300     MOVE 0                       TO IN-EKHT-PRHEMTAG                     
371400                                                                          
371500     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
371600     MOVE LEV-IDDC         TO W-IDDC                                      
371700     PERFORM IMS-GU-WDK711                                                
371800     IF SEGMENT-FINNS                                                     
371900       MOVE SLAG-PRAVCOST         TO IN-EKHT-PRARTSTD                     
372000     ELSE                                                                 
372100       MOVE 0                     TO IN-EKHT-PRARTSTD                     
372200     END-IF                                                               
372300                                                                          
372400     MOVE 0                       TO IN-EKHT-PRINK                        
372500     MOVE 0                       TO IN-EKHT-PRDIRLON                     
372600     MOVE 0                       TO IN-EKHT-PRDMTRL                      
372700     MOVE 0                       TO IN-EKHT-PROVRPAL                     
372800     MOVE 0                       TO IN-EKHT-PRLANDCO                     
372900     MOVE LEV-KVLEVANM-BEKR       TO IN-EKHT-KVANTAL                      
373000                                     VIR-KVLEVANM                         
373100     MOVE 0                       TO IN-EKHT-SUBEL                        
373200     MOVE 'W4183000'              TO IN-EKHT-IDPGM                        
373300     MOVE ' '                     TO IN-EKHT-IDTRANS                      
373400     MOVE 'W515EKHA'              TO IN-EKHT-IDCPYTXT                     
373500     MOVE LEV-KDANMORS            TO IN-EKHT-KDANMORS                     
373600     MOVE LEV-IDANALYS            TO IN-EKHT-IDANALYS                     
373700     MOVE LEV-IDKONTO             TO IN-EKHT-IDKONTO                      
373800     MOVE LEV-IDKST               TO IN-EKHT-IDKST                        
373900     MOVE ZERO                    TO IN-EKHT-BEVAT                        
374000                                     IN-EKHT-KDFRAKT                      
374100                                     IN-EKHT-SUVAT                        
374200     MOVE ZERO                    TO IN-EKHT-DAAVIDAT                     
374300                                     IN-EKHT-IDAVINR                      
374400                                     IN-EKHT-KDAVVTYP                     
374500                                     IN-EKHT-KDRT                         
374600                                     IN-EKHT-KVANTMOT                     
374700                                     IN-EKHT-KVAVIS                       
374800     MOVE WS-KDSORT               TO IN-EKHT-KDSORT                       
374900     MOVE 'IN07'                  TO IN-EKHT-KDTRADP                      
375000     MOVE SPACE                   TO IN-EKHT-IDLEVNR                      
375100     MOVE SPACE                   TO IN-EKHT-FLOVRLEV                     
375200     MOVE ZERO                    TO IN-EKHT-IDORDNR5                     
375300     MOVE SPACE                   TO IN-EKHT-IDUSER                       
375400     MOVE NEJ                     TO IN-EKHT-FLDCET                       
375500     MOVE SPACE                   TO IN-EKHT-IDKUNDRF                     
375600     MOVE LEV-IDFAKT              TO IN-EKHT-IDFAKT-EXP                   
375700                                                                          
375800     PERFORM S07-SKRIV-EKONOMIFIL-IN                                      
375900                                                                          
376000     IF GOOD-DDC                                                          
376100       IF OKOD-FL-LEVERANTOER = JA                                        
376200          MOVE ZERO   TO VIR-IDKNOTNR                                     
376300          PERFORM S42-SKAPA-RADER-VIR                                     
376400       END-IF                                                             
376500     END-IF                                                               
376600     .                                                                    
376700     EJECT                                                                
376800 EBCB-FLYTTA-SKRIV-EKOFIL-RADER SECTION.                                  
376900                                                                          
377000     MOVE '303'                   TO EKHT-KDEKHHT                         
377100                                                                          
377200     IF LEV-KDANMORS = '13' OR '23'                                       
377300       MOVE '310'                 TO EKHT-KDEKSHT                         
377400     ELSE                                                                 
377500       IF LEV-KDANMORS = '28'                                             
377600         MOVE '318'               TO EKHT-KDEKSHT                         
377700       ELSE                                                               
377800         MOVE 0                   TO EKHT-KDEKSHT (1:1)                   
377900         MOVE LEV-KDANMORS        TO EKHT-KDEKSHT (2:2)                   
378000       END-IF                                                             
378100     END-IF                                                               
378200                                                                          
378300     MOVE 'DET'                   TO EKHT-KDEKNIVA                        
378400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
378500     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
378600     MOVE 1                       TO EKHT-IDSEKVNR                        
378700     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
378800     MOVE ' '                     TO EKHT-IDDC-REC                        
378900     MOVE ANM-IDDISTR             TO EKHT-IDDISTR                         
379000     MOVE ANM-IDKUNDNR            TO EKHT-IDKUNDNR                        
379100                                                                          
379200     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
379300     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
379400     CALL W009CIA USING           CIA-W009CIA                             
379500     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
379600                                                                          
379700     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
379800     MOVE 0                       TO EKHT-KDPRODSL                        
379900     MOVE 0                       TO EKHT-KDPSLLOC                        
380000     MOVE LEV-IDARTNR             TO EKHT-IDARTNR                         
380100**** SE SECT. EB-FELLEV...        TO EKHT-FLLSBOK                         
380200     MOVE 'SEK'                   TO EKHT-KDVALISO                        
380300     MOVE 1.0                     TO EKHT-PRKURS                          
380400     MOVE 0                       TO EKHT-PRARTNTO                        
380500     MOVE 0                       TO EKHT-PRARTSJK                        
380600     MOVE 0                       TO EKHT-PRHEMTAG                        
380700     MOVE CLAG-PRARTSTD           TO EKHT-PRARTSTD                        
380800     MOVE 0                       TO EKHT-PRINK                           
380900     MOVE 0                       TO EKHT-PRDIRLON                        
381000     MOVE 0                       TO EKHT-PRDMTRL                         
381100     MOVE 0                       TO EKHT-PROVRPAL                        
381200     MOVE 0                       TO EKHT-PRLANDCO                        
381300     MOVE LEV-KVLEVANM-BEKR       TO EKHT-KVANTAL                         
381400                                     VIR-KVLEVANM                         
381500     MOVE 0                       TO EKHT-SUBEL                           
381600     MOVE 'W4183000'              TO EKHT-IDPGM                           
381700     MOVE ' '                     TO EKHT-IDTRANS                         
381800     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
381900     MOVE LEV-KDANMORS            TO EKHT-KDANMORS                        
382000     MOVE LEV-IDANALYS            TO EKHT-IDANALYS                        
382100     MOVE LEV-IDKONTO             TO EKHT-IDKONTO                         
382200     MOVE LEV-IDKST               TO EKHT-IDKST                           
382300     MOVE ZERO                    TO EKHT-BEVAT                           
382400                                     EKHT-KDFRAKT                         
382500                                     EKHT-SUVAT                           
382600     MOVE ZERO                    TO EKHT-DAAVIDAT                        
382700                                     EKHT-IDAVINR                         
382800                                     EKHT-KDAVVTYP                        
382900                                     EKHT-KDRT                            
383000                                     EKHT-KVANTMOT                        
383100                                     EKHT-KVAVIS                          
383200     MOVE WS-KDSORT               TO EKHT-KDSORT                          
383300     MOVE 'SEPV'                  TO EKHT-KDTRADP                         
383400     MOVE SPACE                   TO EKHT-IDLEVNR                         
383500     MOVE SPACE                   TO EKHT-FLOVRLEV                        
383600     MOVE ZERO                    TO EKHT-IDORDNR5                        
383700     MOVE SPACE                   TO EKHT-IDUSER                          
383800     MOVE NEJ                     TO EKHT-FLDCET                          
383900     MOVE SPACE                   TO EKHT-IDKUNDRF                        
384000     MOVE LEV-IDFAKT              TO EKHT-IDFAKT-EXP                      
384100                                                                          
384200     PERFORM S06-SKRIV-EKONOMIFIL                                         
384300                                                                          
384400     IF GOOD-DDC                                                          
384500       IF OKOD-FL-LEVERANTOER = JA                                        
384600          MOVE ZERO   TO VIR-IDKNOTNR                                     
384700          PERFORM S42-SKAPA-RADER-VIR                                     
384800       END-IF                                                             
384900     END-IF                                                               
385000     .                                                                    
385100     EJECT                                                                
385200 EBD-FLYTTA-SKRIV-EKOFIL-403   SECTION.                                   
385300                                                                          
385400     MOVE LEV-IDDC                TO WS-IDDC                              
385500     IF XDC-NON-VCC-OWNED                                                 
385600       IF LEV-KDANMORS = '13' OR '23'                                     
385700         IF NDC-IN                                                        
385800           PERFORM EBDA-FLYTT-SKRIV-EKOFIL-403-IN                         
385900         ELSE                                                             
386000           PERFORM EBDA-FLYTT-SKRIV-EKOFIL-403-SC                         
386100         END-IF                                                           
386200       END-IF                                                             
386300     ELSE                                                                 
386400       PERFORM EBDB-FLYTTA-SKRIV-EKOFIL-403                               
386500     END-IF                                                               
386600     .                                                                    
386700     EJECT                                                                
386800 EBDA-FLYTT-SKRIV-EKOFIL-403-SC SECTION.                                  
386900                                                                          
387000     MOVE '403'                   TO SC-EKHT-KDEKHHT                      
387100     MOVE '408'                   TO SC-EKHT-KDEKSHT                      
387200                                                                          
387300     MOVE 'DET'                   TO SC-EKHT-KDEKNIVA                     
387400     MOVE FUNCTION CURRENT-DATE (1:8) TO SC-EKHT-TIREGDAT                 
387500     MOVE FUNCTION CURRENT-DATE (9:8) TO SC-EKHT-TIKLOCK                  
387600     MOVE 1                       TO SC-EKHT-IDSEKVNR                     
387700     MOVE LEV-IDDC                TO SC-EKHT-IDDC-SEND                    
387800     MOVE LEV-IDDC-RET            TO SC-EKHT-IDDC-REC                     
387900     MOVE ANM-IDDISTR             TO SC-EKHT-IDDISTR                      
388000     MOVE ANM-IDKUNDNR            TO SC-EKHT-IDKUNDNR                     
388100                                                                          
388200     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
388300     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
388400     CALL W009CIA USING           CIA-W009CIA                             
388500     MOVE CIA-IDARTBET-UT      TO SC-EKHT-IDVERGL                         
388600                                                                          
388700     MOVE DAGENS-DATUM-SEKEL      TO SC-EKHT-DAVERDAT                     
388800     MOVE 0                       TO SC-EKHT-KDPRODSL                     
388900     MOVE 0                       TO SC-EKHT-KDPSLLOC                     
389000     MOVE LEV-IDARTNR             TO SC-EKHT-IDARTNR                      
389100     MOVE JA                      TO SC-EKHT-FLLSBOK                      
389200     MOVE 1.0                     TO SC-EKHT-PRKURS                       
389300     MOVE 0                       TO SC-EKHT-PRARTNTO                     
389400     MOVE 0                       TO SC-EKHT-PRARTSJK                     
389500     MOVE 0                       TO SC-EKHT-PRHEMTAG                     
389600                                                                          
389700     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
389800     MOVE LEV-IDDC         TO W-IDDC                                      
389900     PERFORM IMS-GU-WDK711                                                
390000     IF SEGMENT-FINNS                                                     
390100       MOVE SLAG-PRAVCOST         TO SC-EKHT-PRARTSTD                     
390200     ELSE                                                                 
390300       MOVE 0                     TO SC-EKHT-PRARTSTD                     
390400     END-IF                                                               
390500                                                                          
390600     MOVE 0                       TO SC-EKHT-PRINK                        
390700     MOVE 0                       TO SC-EKHT-PRDIRLON                     
390800     MOVE 0                       TO SC-EKHT-PRDMTRL                      
390900     MOVE 0                       TO SC-EKHT-PROVRPAL                     
391000     MOVE 0                       TO SC-EKHT-PRLANDCO                     
391100     MOVE LEV-KVLEVANM-BEKR       TO SC-EKHT-KVANTAL                      
391200                                     VIR-KVLEVANM                         
391300     MOVE 0                       TO SC-EKHT-SUBEL                        
391400     MOVE 'W4183000'              TO SC-EKHT-IDPGM                        
391500     MOVE ' '                     TO SC-EKHT-IDTRANS                      
391600     MOVE LEV-KDANMORS            TO SC-EKHT-KDANMORS                     
391700     MOVE LEV-IDANALYS            TO SC-EKHT-IDANALYS                     
391800     MOVE LEV-IDKONTO             TO SC-EKHT-IDKONTO                      
391900     MOVE LEV-IDKST               TO SC-EKHT-IDKST                        
392000     MOVE ZERO                    TO SC-EKHT-BEVAT                        
392100                                     SC-EKHT-KDFRAKT                      
392200                                     SC-EKHT-SUVAT                        
392300     MOVE ZERO                    TO SC-EKHT-DAAVIDAT                     
392400                                     SC-EKHT-IDAVINR                      
392500                                     SC-EKHT-KDAVVTYP                     
392600                                     SC-EKHT-KDRT                         
392700                                     SC-EKHT-KVANTMOT                     
392800                                     SC-EKHT-KVAVIS                       
392900     MOVE WS-KDSORT               TO SC-EKHT-KDSORT                       
393000     MOVE SPACE                   TO SC-EKHT-IDLEVNR                      
393100     MOVE SPACE                   TO SC-EKHT-FLOVRLEV                     
393200     MOVE ZERO                    TO SC-EKHT-IDORDNR5                     
393300     MOVE SPACE                   TO SC-EKHT-IDUSER                       
393400     MOVE NEJ                     TO SC-EKHT-FLDCET                       
393500     MOVE SPACE                   TO SC-EKHT-IDKUNDRF                     
393600     MOVE LEV-IDFAKT              TO SC-EKHT-IDFAKT-EXP                   
393700     MOVE DCS-KDVALISO            TO SC-EKHT-KDVALISO                     
393800     MOVE DCS-KDTRADP             TO SC-EKHT-KDTRADP                      
393900     IF NDC-CN                                                            
394000       MOVE 'W570'                TO SC-EKHT-IDCPYTXT(1:4)                
394100     ELSE                                                                 
394200       MOVE DCS-KDTRADP           TO SC-EKHT-IDCPYTXT(1:4)                
394300     END-IF                                                               
394400     MOVE 'EKHA'                  TO SC-EKHT-IDCPYTXT(5:4)                
394500                                                                          
394600     IF XDC-NON-VCC-OWNED                                                 
394700       PERFORM S07-SKRIV-EKONOMIFIL                                       
394800     END-IF                                                               
394900                                                                          
395000     IF GOOD-DDC                                                          
395100       IF OKOD-FL-LEVERANTOER = JA                                        
395200          MOVE ZERO   TO VIR-IDKNOTNR                                     
395300          PERFORM S42-SKAPA-RADER-VIR                                     
395400       END-IF                                                             
395500     END-IF                                                               
395600     .                                                                    
395700     EJECT                                                                
395800                                                                          
395900 EBDA-FLYTT-SKRIV-EKOFIL-403-IN SECTION.                                  
396000     MOVE '403'                   TO IN-EKHT-KDEKHHT                      
396100     MOVE '408'                   TO IN-EKHT-KDEKSHT                      
396200                                                                          
396300     MOVE 'DET'                   TO IN-EKHT-KDEKNIVA                     
396400     MOVE FUNCTION CURRENT-DATE (1:8) TO IN-EKHT-TIREGDAT                 
396500     MOVE FUNCTION CURRENT-DATE (9:8) TO IN-EKHT-TIKLOCK                  
396600     MOVE 1                       TO IN-EKHT-IDSEKVNR                     
396700     MOVE LEV-IDDC                TO IN-EKHT-IDDC-SEND                    
396800     MOVE LEV-IDDC-RET            TO IN-EKHT-IDDC-REC                     
396900     MOVE ANM-IDDISTR             TO IN-EKHT-IDDISTR                      
397000     MOVE ANM-IDKUNDNR            TO IN-EKHT-IDKUNDNR                     
397100                                                                          
397200     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
397300     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
397400     CALL W009CIA USING           CIA-W009CIA                             
397500     MOVE CIA-IDARTBET-UT      TO IN-EKHT-IDVERGL                         
397600                                                                          
397700     MOVE DAGENS-DATUM-SEKEL      TO IN-EKHT-DAVERDAT                     
397800     MOVE 0                       TO IN-EKHT-KDPRODSL                     
397900     MOVE 0                       TO IN-EKHT-KDPSLLOC                     
398000     MOVE LEV-IDARTNR             TO IN-EKHT-IDARTNR                      
398100     MOVE JA                      TO IN-EKHT-FLLSBOK                      
398200     MOVE 'INR'                   TO IN-EKHT-KDVALISO                     
398300     MOVE 1.0                     TO IN-EKHT-PRKURS                       
398400     MOVE 0                       TO IN-EKHT-PRARTNTO                     
398500     MOVE 0                       TO IN-EKHT-PRARTSJK                     
398600     MOVE 0                       TO IN-EKHT-PRHEMTAG                     
398700                                                                          
398800     MOVE LEV-IDARTNR      TO W-IDARTNR-K7                                
398900     MOVE LEV-IDDC         TO W-IDDC                                      
399000     PERFORM IMS-GU-WDK711                                                
399100     IF SEGMENT-FINNS                                                     
399200       MOVE SLAG-PRAVCOST         TO IN-EKHT-PRARTSTD                     
399300     ELSE                                                                 
399400       MOVE 0                     TO IN-EKHT-PRARTSTD                     
399500     END-IF                                                               
399600                                                                          
399700     MOVE 0                       TO IN-EKHT-PRINK                        
399800     MOVE 0                       TO IN-EKHT-PRDIRLON                     
399900     MOVE 0                       TO IN-EKHT-PRDMTRL                      
400000     MOVE 0                       TO IN-EKHT-PROVRPAL                     
400100     MOVE 0                       TO IN-EKHT-PRLANDCO                     
400200     MOVE LEV-KVLEVANM-BEKR       TO IN-EKHT-KVANTAL                      
400300                                     VIR-KVLEVANM                         
400400     MOVE 0                       TO IN-EKHT-SUBEL                        
400500     MOVE 'W4183000'              TO IN-EKHT-IDPGM                        
400600     MOVE ' '                     TO IN-EKHT-IDTRANS                      
400700     MOVE 'W515EKHA'              TO IN-EKHT-IDCPYTXT                     
400800     MOVE LEV-KDANMORS            TO IN-EKHT-KDANMORS                     
400900     MOVE LEV-IDANALYS            TO IN-EKHT-IDANALYS                     
401000     MOVE LEV-IDKONTO             TO IN-EKHT-IDKONTO                      
401100     MOVE LEV-IDKST               TO IN-EKHT-IDKST                        
401200     MOVE ZERO                    TO IN-EKHT-BEVAT                        
401300                                     IN-EKHT-KDFRAKT                      
401400                                     IN-EKHT-SUVAT                        
401500     MOVE ZERO                    TO IN-EKHT-DAAVIDAT                     
401600                                     IN-EKHT-IDAVINR                      
401700                                     IN-EKHT-KDAVVTYP                     
401800                                     IN-EKHT-KDRT                         
401900                                     IN-EKHT-KVANTMOT                     
402000                                     IN-EKHT-KVAVIS                       
402100     MOVE WS-KDSORT               TO IN-EKHT-KDSORT                       
402200     MOVE 'IN07'                  TO IN-EKHT-KDTRADP                      
402300     MOVE SPACE                   TO IN-EKHT-IDLEVNR                      
402400     MOVE SPACE                   TO IN-EKHT-FLOVRLEV                     
402500     MOVE ZERO                    TO IN-EKHT-IDORDNR5                     
402600     MOVE SPACE                   TO IN-EKHT-IDUSER                       
402700     MOVE NEJ                     TO IN-EKHT-FLDCET                       
402800     MOVE SPACE                   TO IN-EKHT-IDKUNDRF                     
402900     MOVE LEV-IDFAKT              TO IN-EKHT-IDFAKT-EXP                   
403000                                                                          
403100     PERFORM S07-SKRIV-EKONOMIFIL-IN                                      
403200                                                                          
403300     IF GOOD-DDC                                                          
403400       IF OKOD-FL-LEVERANTOER = JA                                        
403500          MOVE ZERO   TO VIR-IDKNOTNR                                     
403600          PERFORM S42-SKAPA-RADER-VIR                                     
403700       END-IF                                                             
403800     END-IF                                                               
403900     .                                                                    
404000     EJECT                                                                
404100 EBDB-FLYTTA-SKRIV-EKOFIL-403   SECTION.                                  
404200                                                                          
404300     IF LEV-KDANMORS = '13' OR '23'                                       
404400       MOVE '403'                 TO EKHT-KDEKHHT                         
404500       MOVE '408'                 TO EKHT-KDEKSHT                         
404600     ELSE                                                                 
404700       MOVE '303'                 TO EKHT-KDEKHHT                         
404800       MOVE 0                     TO EKHT-KDEKSHT (1:1)                   
404900       MOVE LEV-KDANMORS          TO EKHT-KDEKSHT (2:2)                   
405000     END-IF                                                               
405100                                                                          
405200     MOVE 'DET'                   TO EKHT-KDEKNIVA                        
405300     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
405400     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
405500     MOVE 1                       TO EKHT-IDSEKVNR                        
405600     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
405700     MOVE LEV-IDDC-RET            TO EKHT-IDDC-REC                        
405800     MOVE ANM-IDDISTR             TO EKHT-IDDISTR                         
405900     MOVE ANM-IDKUNDNR            TO EKHT-IDKUNDNR                        
406000                                                                          
406100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
406200     MOVE ANM-IDRAPPNR         TO CIA-IDARTBET-IN                         
406300     CALL W009CIA USING           CIA-W009CIA                             
406400     MOVE CIA-IDARTBET-UT      TO EKHT-IDVERGL                            
406500                                                                          
406600     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
406700     MOVE 0                       TO EKHT-KDPRODSL                        
406800     MOVE 0                       TO EKHT-KDPSLLOC                        
406900     MOVE LEV-IDARTNR             TO EKHT-IDARTNR                         
407000**** SE SECT. EB-FELLEV...        TO EKHT-FLLSBOK                         
407100     MOVE 'SEK'                   TO EKHT-KDVALISO                        
407200     MOVE 1.0                     TO EKHT-PRKURS                          
407300     MOVE 0                       TO EKHT-PRARTNTO                        
407400     MOVE 0                       TO EKHT-PRARTSJK                        
407500     MOVE 0                       TO EKHT-PRHEMTAG                        
407600     MOVE CLAG-PRARTSTD           TO EKHT-PRARTSTD                        
407700     MOVE 0                       TO EKHT-PRINK                           
407800     MOVE 0                       TO EKHT-PRDIRLON                        
407900     MOVE 0                       TO EKHT-PRDMTRL                         
408000     MOVE 0                       TO EKHT-PROVRPAL                        
408100     MOVE 0                       TO EKHT-PRLANDCO                        
408200     MOVE LEV-KVLEVANM-BEKR       TO EKHT-KVANTAL                         
408300                                     VIR-KVLEVANM                         
408400     MOVE 0                       TO EKHT-SUBEL                           
408500     MOVE 'W4183000'              TO EKHT-IDPGM                           
408600     MOVE ' '                     TO EKHT-IDTRANS                         
408700     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
408800     MOVE LEV-KDANMORS            TO EKHT-KDANMORS                        
408900     MOVE LEV-IDANALYS            TO EKHT-IDANALYS                        
409000     MOVE LEV-IDKONTO             TO EKHT-IDKONTO                         
409100     MOVE LEV-IDKST               TO EKHT-IDKST                           
409200     MOVE ZERO                    TO EKHT-BEVAT                           
409300                                     EKHT-KDFRAKT                         
409400                                     EKHT-SUVAT                           
409500     MOVE ZERO                    TO EKHT-DAAVIDAT                        
409600                                     EKHT-IDAVINR                         
409700                                     EKHT-KDAVVTYP                        
409800                                     EKHT-KDRT                            
409900                                     EKHT-KVANTMOT                        
410000                                     EKHT-KVAVIS                          
410100     MOVE WS-KDSORT               TO EKHT-KDSORT                          
410200     MOVE 'SEPV'                  TO EKHT-KDTRADP                         
410300     MOVE SPACE                   TO EKHT-IDLEVNR                         
410400     MOVE SPACE                   TO EKHT-FLOVRLEV                        
410500     MOVE ZERO                    TO EKHT-IDORDNR5                        
410600     MOVE SPACE                   TO EKHT-IDUSER                          
410700     IF LEV-IDDC = WC-SDC-NL-ET                                           
410800       MOVE JA                    TO EKHT-FLDCET                          
410900     ELSE                                                                 
411000       MOVE NEJ                   TO EKHT-FLDCET                          
411100     END-IF                                                               
411200     MOVE SPACE                   TO EKHT-IDKUNDRF                        
411300     MOVE LEV-IDFAKT              TO EKHT-IDFAKT-EXP                      
411400                                                                          
411500     PERFORM S06-SKRIV-EKONOMIFIL                                         
411600                                                                          
411700     IF GOOD-DDC                                                          
411800       IF OKOD-FL-LEVERANTOER = JA                                        
411900          MOVE ZERO   TO VIR-IDKNOTNR                                     
412000          PERFORM S42-SKAPA-RADER-VIR                                     
412100       END-IF                                                             
412200     END-IF                                                               
412300     .                                                                    
412400     EJECT                                                                
412500 EC-KNOTA-PRISFEL SECTION.                                                
412600*    DISPLAY '*** EC-KNOTA-PRISFEL '                                      
412700                                                                          
412800     MOVE ANM-IDFTG     TO WS-IDFTG                                       
412900                                                                          
413000     IF IDFTG-PV OR IDFTG-NON-VCC                                         
413100       MOVE JA       TO KNOTA-VIA-BILLIT-SW                               
413200       MOVE ZERO     TO 31B-KDAVVTYP                                      
413300       MOVE NEJ      TO 31B-FLINVUPD                                      
413400       PERFORM S46-SKAPA-KN-RAD-BILLIT                                    
413500     ELSE                                                                 
413600       EVALUATE TRUE                                                      
413700          WHEN CDC-SE                                                     
413800             IF W-IDKNOTNR-CDC = +0                                       
413900                PERFORM S40-KNOTNR                                        
414000             END-IF                                                       
414100          WHEN SDC                                                        
414200             IF W-IDKNOTNR-SDC = +0                                       
414300                PERFORM S40-KNOTNR                                        
414400             END-IF                                                       
414500          WHEN NDC-US                                                     
414600             IF W-IDKNOTNR-USA = +0                                       
414700                PERFORM S40-KNOTNR                                        
414800             END-IF                                                       
414900          WHEN NDC-CA                                                     
415000             IF W-IDKNOTNR-CAN = +0                                       
415100                PERFORM S40-KNOTNR                                        
415200             END-IF                                                       
415300          WHEN NDC-JP                                                     
415400             IF W-IDKNOTNR-JAP = +0                                       
415500                PERFORM S40-KNOTNR                                        
415600             END-IF                                                       
415700          WHEN NDC-AU                                                     
415800             IF W-IDKNOTNR-AUS = +0                                       
415900                PERFORM S40-KNOTNR                                        
416000             END-IF                                                       
416100          WHEN DDC-SE                                                     
416200             IF W-IDKNOTNR-SE = +0                                        
416300                PERFORM S40-KNOTNR                                        
416400             END-IF                                                       
416500          WHEN DDC-NO                                                     
416600             IF W-IDKNOTNR-NO = +0                                        
416700                PERFORM S40-KNOTNR                                        
416800             END-IF                                                       
416900          WHEN DDC-BE                                                     
417000             IF W-IDKNOTNR-BE = +0                                        
417100                PERFORM S40-KNOTNR                                        
417200             END-IF                                                       
417300       END-EVALUATE                                                       
417400                                                                          
417500       PERFORM S51-SKRIV-UPPDAT-POST-KREE                                 
417600                                                                          
417700       IF CDC-SE                                                          
417800          IF SKRIV-HUV-KNOTA-CDC                                          
417900             PERFORM S35-SKAPA-HUVUD-KNOTA                                
418000          END-IF                                                          
418100       ELSE                                                               
418200         EVALUATE TRUE                                                    
418300           WHEN DDC-SE                                                    
418400            IF SKRIV-HUV-KNOTA-SE                                         
418500               PERFORM S35-SKAPA-HUVUD-KNOTA                              
418600            END-IF                                                        
418700           WHEN DDC-NO                                                    
418800            IF SKRIV-HUV-KNOTA-NO                                         
418900               PERFORM S35-SKAPA-HUVUD-KNOTA                              
419000            END-IF                                                        
419100           WHEN DDC-BE                                                    
419200            IF SKRIV-HUV-KNOTA-BE                                         
419300               PERFORM S35-SKAPA-HUVUD-KNOTA                              
419400            END-IF                                                        
419500           WHEN OTHER                                                     
419600            IF SKRIV-HUV-KNOTA-SDC                                        
419700               PERFORM S35-SKAPA-HUVUD-KNOTA                              
419800            END-IF                                                        
419900         END-EVALUATE                                                     
420000       END-IF                                                             
420100                                                                          
420200       PERFORM S36-SKAPA-RADPOST-KNOTA                                    
420300       PERFORM S60-RADPOST-EKO-LAB                                        
420400     END-IF                                                               
420500     .                                                                    
420600     EJECT                                                                
420700 ED-KNOTA-SKROT SECTION.                                                  
420800*    DISPLAY '*** ED-KNOTA-SKROT '                                        
420900                                                                          
421000     MOVE ANM-IDFTG   TO WS-IDFTG                                         
421100                                                                          
421200     IF IDFTG-PV OR IDFTG-NON-VCC                                         
421300       MOVE JA       TO KNOTA-VIA-BILLIT-SW                               
421400       MOVE ZERO     TO 31B-KDAVVTYP                                      
421500       MOVE NEJ      TO 31B-FLINVUPD                                      
421600       PERFORM S46-SKAPA-KN-RAD-BILLIT                                    
421700     ELSE                                                                 
421800       EVALUATE TRUE                                                      
421900          WHEN CDC-SE                                                     
422000             IF W-IDKNOTNR-CDC = +0                                       
422100                PERFORM S40-KNOTNR                                        
422200             END-IF                                                       
422300          WHEN SDC                                                        
422400             IF W-IDKNOTNR-SDC = +0                                       
422500                PERFORM S40-KNOTNR                                        
422600             END-IF                                                       
422700          WHEN NDC-US                                                     
422800             IF W-IDKNOTNR-USA = +0                                       
422900                PERFORM S40-KNOTNR                                        
423000             END-IF                                                       
423100          WHEN NDC-CA                                                     
423200             IF W-IDKNOTNR-CAN = +0                                       
423300                PERFORM S40-KNOTNR                                        
423400             END-IF                                                       
423500          WHEN NDC-JP                                                     
423600             IF W-IDKNOTNR-JAP = +0                                       
423700                PERFORM S40-KNOTNR                                        
423800             END-IF                                                       
423900          WHEN NDC-AU                                                     
424000             IF W-IDKNOTNR-AUS = +0                                       
424100                PERFORM S40-KNOTNR                                        
424200             END-IF                                                       
424300          WHEN DDC-SE                                                     
424400             IF W-IDKNOTNR-SE = +0                                        
424500                PERFORM S40-KNOTNR                                        
424600             END-IF                                                       
424700          WHEN DDC-NO                                                     
424800             IF W-IDKNOTNR-NO = +0                                        
424900                PERFORM S40-KNOTNR                                        
425000             END-IF                                                       
425100          WHEN DDC-BE                                                     
425200             IF W-IDKNOTNR-BE = +0                                        
425300                PERFORM S40-KNOTNR                                        
425400             END-IF                                                       
425500       END-EVALUATE                                                       
425600                                                                          
425700       PERFORM S51-SKRIV-UPPDAT-POST-KREE                                 
425800                                                                          
425900       IF CDC-SE                                                          
426000          IF SKRIV-HUV-KNOTA-CDC                                          
426100             PERFORM S35-SKAPA-HUVUD-KNOTA                                
426200          END-IF                                                          
426300       ELSE                                                               
426400         EVALUATE TRUE                                                    
426500           WHEN DDC-SE                                                    
426600             IF SKRIV-HUV-KNOTA-SE                                        
426700                PERFORM S35-SKAPA-HUVUD-KNOTA                             
426800             END-IF                                                       
426900           WHEN DDC-NO                                                    
427000             IF SKRIV-HUV-KNOTA-NO                                        
427100                PERFORM S35-SKAPA-HUVUD-KNOTA                             
427200             END-IF                                                       
427300           WHEN DDC-BE                                                    
427400             IF SKRIV-HUV-KNOTA-BE                                        
427500                PERFORM S35-SKAPA-HUVUD-KNOTA                             
427600             END-IF                                                       
427700           WHEN OTHER                                                     
427800             IF SKRIV-HUV-KNOTA-SDC                                       
427900                PERFORM S35-SKAPA-HUVUD-KNOTA                             
428000             END-IF                                                       
428100         END-EVALUATE                                                     
428200       END-IF                                                             
428300                                                                          
428400       PERFORM S36-SKAPA-RADPOST-KNOTA                                    
428500       PERFORM S60-RADPOST-EKO-LAB                                        
428600     END-IF                                                               
428700     .                                                                    
428800     EJECT                                                                
428900 F-BEHANDLA-KRENOT-EFTER-RETUR SECTION.                                   
429000*    DISPLAY '*** F-BEHANDLA-KRENOT-EFTER-RETUR'                          
429100*                         *** KOD 12 22 42 52 62 72 82 92 97 98 99        
429200*                         *** KOD 27 SOM 22 MEN UTAN SALDOBOKNING         
429300*                         *** KODERNA 12 22 OCH 82 KRÄVER PRIS            
429400*                         *** RETUREN HAR STATUS 7                        
429500*                         *** KREDITNOTA SKAPAS EJ PÅ KOD 99              
429600*                         *** OM PRIS OCH INLAGT ANTAL > 0                
429700*                         *** KONTERA OCH SKRIV KREDITNOTA                
429800*                         *** OM BEGÄRT OCH INLAGT ANTAL DIFFAR           
429900*                         *** - JUSTERA EKONOMI-LAB                       
430000*                         *** INVENTERING OM KUNDEN EJ HAR                
430100*                         *** DEBITERATS DET RETURNERADE                  
430200                                                                          
430300     PERFORM S100-NOLLA-720-AREA                                          
430400     MOVE ZERO                            TO  WS-KVRETINL                 
430500                                              W-IDKNOTNR-CDC              
430600                                              W-IDKNOTNR-SDC              
430700                                              W-IDKNOTNR-USA              
430800                                              W-IDKNOTNR-CAN              
430900                                              W-IDKNOTNR-ITL              
431000                                              W-IDKNOTNR-JAP              
431100                                              W-IDKNOTNR-AUS              
431200                                              W-IDKNOTNR-SE               
431300                                              W-IDKNOTNR-NO               
431400                                              W-IDKNOTNR-BE               
431500                                              WS-KVRADER-72               
431600                                              WS-KVRADER-98               
431700     MOVE SPACE                           TO IDDC-WS                      
431800     MOVE JA                              TO                              
431900                                          SKRIV-HUV-KNOTA-CDC-SW          
432000                                          SKRIV-HUV-KNOTA-SDC-SW          
432100                                          SKRIV-HUV-KNOTA-SE-SW           
432200                                          SKRIV-HUV-KNOTA-NO-SW           
432300                                          SKRIV-HUV-KNOTA-BE-SW           
432400                                          SKRIV-HUV-EKOFIL-SW             
432500                                          SKRIV-MOMS-ITL                  
432600     MOVE NEJ                             TO                              
432700                                        RETILL-FINNS-SW                   
432800                                        RETUR-ITL-SW                      
432900                                                                          
433000     MOVE SEQA-IDLEVANM                   TO W-IDLEVANM-X                 
433100     PERFORM FC-KOLLA-OM-KNOTA-FINNS-TID                                  
433200     PERFORM IMS-GU-KREE-ANM                                              
433300     PERFORM IMS-GNP-KREE-LEV                                             
433400*FIX CO                                                                   
433500     IF SEGMENT-SAKNAS                                                    
433600       MOVE JA TO A211-SAKNAS                                             
433700     END-IF                                                               
433800                                                                          
433900     MOVE ANM-IDDISTR                     TO TEST-IDDISTR                 
434000                                                                          
434100     PERFORM UNTIL SEGMENT-SAKNAS                                         
434200                                                                          
434300********FIX DDI EXIT                                                      
434400      IF ANM-IDDISTR = 1678      AND                                      
434500         LEV-TIFAKT-LOC > 0      AND                                      
434600         LEV-TIFAKT-LOC < 170507 AND                                      
434700         LEV-TIFAKT     = 0                                               
434800        MOVE JA TO A211-SAKNAS                                            
434900        PERFORM S90-SKAPA-DDI-LIST                                        
435000        PERFORM S91-UPPDATERA-STATUS-8                                    
435100********FIX DDI EXIT                                                      
435200                                                                          
435300      ELSE                                                                
435400        MOVE ANM-IDFTG                    TO WS-IDFTG                     
435500        MOVE LEV-IDDC                     TO WS-IDDC                      
435600        MOVE LEV-KDANMORS                 TO OKOD-KDANMORS                
435700        CALL W418OKOD USING OKOD-W418OKOD                                 
435800                                                                          
435900        MOVE LEV-KDKREBEH                 TO W-KDKREBEH                   
436000        IF OKOD-FL-RETILL = JA            OR                              
436100           OKOD-FL-INTERNUPPACKNING = JA  OR                              
436200           W-BOKST = 'N'                  OR                              
436300           LEV-KDKREBEH = 'D01'           OR                              
436400           LEV-KDKREBEH = 'D02'           OR                              
436500           LEV-KDKREBEH = 'D03'                                           
436600                                                                          
436700           IF LEV-FLTEXT = JA                                             
436800              PERFORM S45-SKAPA-TXT-TILL-VIPS                             
436900           END-IF                                                         
437000           MOVE LEV-IDARTNR               TO W-IDARTNR                    
437100                                             W-IDARTNR-K7                 
437200                                             TEST-IDARTNR                 
437300           MOVE ANM-IDDISTR               TO TEST-IDDISTR                 
437400           IF (BYT16-BYTES OR BYT16-RADIO) AND                            
437500              (LEV-KVLEVANM-BEKR  >  LEV-KVAVV-KVANT)                     
437600              IF DIST16-BUYBACK-EXCL                                      
437700                 IF LEV-KDANMORS = '98'                                   
437800                    MOVE JA TO SKRIV-BYTES-SW                             
437900                    PERFORM S36C-SKAPA-RADPOST-BYTES                      
438000                 END-IF                                                   
438100              ELSE                                                        
438200                 IF DIST16-OVERSEAS-EXCL                                  
438300                    MOVE JA TO SKRIV-BYTES-SW                             
438400                    PERFORM S36C-SKAPA-RADPOST-BYTES                      
438500                 END-IF                                                   
438600              END-IF                                                      
438700           END-IF                                                         
438800           PERFORM IMS-GU-ARTC01                                          
438900           MOVE ART-KDSORT TO WS-KDSORT                                   
439000           PERFORM IMS-GNP-ARTC11                                         
439100           IF (W-BOKST = 'N' AND BARA-N7X-RADER ) OR                      
439200             (OKOD-FL-RETILL = JA AND                                     
439300                             ( LEV-KDKREBEH = 'ANN' OR 'DEL'))            
439400                                                                          
439500             PERFORM S80-SKAPA-RKD-POST                                   
439600                                                                          
439700             IF OKOD-FL-RETILL = JA AND                                   
439800                            ( LEV-KDKREBEH = 'ANN' OR 'DEL' )             
439900                IF LEV-KDANMORS = '12' OR '22' OR '27'                    
440000                  MOVE LEV-KVLEVANM-BEKR TO WS-KVLEVANM-BEKR              
440100                  PERFORM FA-BEHANDLA-KOD-12-22                           
440200                END-IF                                                    
440300             END-IF                                                       
440400                                                                          
440500           ELSE                                                           
440600             COMPUTE WS-KVRETINL = LEV-KVRETINL + LEV-KVRETINL-SKR        
440700                                                                          
440800              IF LEV-KDKREBEH = 'D01' OR 'D02' OR 'D03'                   
440900                                                                          
441000* OM ANTALSAVVIKELS/KVALITETSAVVIKELSE OCH RETURNERAT ANTAL               
441100* ÄR MINDRE ÄN LEVERANSANMÄRKNINGSANTAL SKA RKD-FELPOST SKAPAS            
441200                 IF LEV-KVRETINL < LEV-KVLEVANM-BEKR                      
441300                                                                          
441400                    PERFORM S80-SKAPA-RKD-POST                            
441500                                                                          
441600                    IF LEV-KDKREBEH = 'D01' OR 'D03'                      
441700                      IF LEV-KDANMORS = '12' OR '22' OR '27'              
441800                        MOVE LEV-KVAVV-KVANT TO WS-KVLEVANM-BEKR          
441900                        PERFORM FA-BEHANDLA-KOD-12-22                     
442000                        MOVE JA              TO SKAPA-001-SW              
442100                      END-IF                                              
442200                    END-IF                                                
442300                                                                          
442400                 END-IF                                                   
442500              END-IF                                                      
442600                                                                          
442700              MOVE ANM-IDDISTR  TO TEST-IDDISTR                           
442800              IF IDFTG-PV OR IDFTG-NON-VCC                                
442900                CONTINUE                                                  
443000              ELSE                                                        
443100                IF WS-KVRETINL > +0 AND                                   
443200                  (LEV-PRARTBTO > +0 OR LEV-PRARTBTO-LOC > 0)             
443300                  IF LEV-KDANMORS = '99'                                  
443400                     MOVE ZERO        TO UT34-IDKNOTNR                    
443500                  ELSE                                                    
443600                     EVALUATE TRUE                                        
443700                        WHEN CDC-SE                                       
443800                           IF W-IDKNOTNR-CDC = +0                         
443900                              PERFORM S40-KNOTNR                          
444000                           END-IF                                         
444100                        WHEN SDC                                          
444200                           IF W-IDKNOTNR-SDC = +0                         
444300                              PERFORM S40-KNOTNR                          
444400                           END-IF                                         
444500                        WHEN NDC-US                                       
444600                           IF W-IDKNOTNR-USA = +0                         
444700                              PERFORM S40-KNOTNR                          
444800                           END-IF                                         
444900                        WHEN NDC-CA                                       
445000                           IF W-IDKNOTNR-CAN = +0                         
445100                              PERFORM S40-KNOTNR                          
445200                           END-IF                                         
445300                        WHEN NDC-JP                                       
445400                           IF W-IDKNOTNR-JAP = +0                         
445500                              PERFORM S40-KNOTNR                          
445600                           END-IF                                         
445700                        WHEN NDC-AU                                       
445800                           IF W-IDKNOTNR-AUS = +0                         
445900                              PERFORM S40-KNOTNR                          
446000                           END-IF                                         
446100                        WHEN DDC-SE                                       
446200                           IF W-IDKNOTNR-SE = +0                          
446300                              PERFORM S40-KNOTNR                          
446400                           END-IF                                         
446500                        WHEN DDC-NO                                       
446600                           IF W-IDKNOTNR-NO = +0                          
446700                              PERFORM S40-KNOTNR                          
446800                           END-IF                                         
446900                        WHEN DDC-BE                                       
447000                           IF W-IDKNOTNR-BE = +0                          
447100                              PERFORM S40-KNOTNR                          
447200                           END-IF                                         
447300                     END-EVALUATE                                         
447400                  END-IF                                                  
447500                ELSE                                                      
447600                  MOVE ZERO           TO UT34-IDKNOTNR                    
447700                END-IF                                                    
447800              END-IF                                                      
447900                                                                          
448000              IF 001-POST-SKAPAD                                          
448100                CONTINUE                                                  
448200              ELSE                                                        
448300                IF IDFTG-PV OR IDFTG-NON-VCC                              
448400                  CONTINUE                                                
448500                ELSE                                                      
448600                  PERFORM S52-SKRIV-UPPDAT-POST-KREE                      
448700                END-IF                                                    
448800                                                                          
448900*---- INFO UT PÅ VIR-LISTA, NÄR RETUR HAR KOMMIT IN.                      
449000                IF ( LEV-KDANMORS = '12' OR '22' OR '27') AND             
449100                                                 GOOD-DDC                 
449200                  IF OKOD-FL-LEVERANTOER = JA                             
449300                    MOVE WS-KVRETINL TO VIR-KVLEVANM                      
449400                    MOVE ZERO        TO VIR-IDKNOTNR                      
449500                    PERFORM S42-SKAPA-RADER-VIR                           
449600                  END-IF                                                  
449700                END-IF                                                    
449800              END-IF                                                      
449900              MOVE NEJ              TO SKAPA-001-SW                       
450000                                                                          
450100              IF WS-KVRETINL > +0 AND                                     
450200                (LEV-PRARTBTO > +0 OR LEV-PRARTBTO-LOC > 0)               
450300                IF LEV-KDANMORS = '99'                                    
450400                   CONTINUE                                               
450500                ELSE                                                      
450600                  IF IDFTG-PV OR IDFTG-NON-VCC                            
450700                    CONTINUE                                              
450800                  ELSE                                                    
450900                    IF CDC-SE                                             
451000                       IF SKRIV-HUV-KNOTA-CDC                             
451100                          PERFORM S35-SKAPA-HUVUD-KNOTA                   
451200                       END-IF                                             
451300                    ELSE                                                  
451400                      EVALUATE TRUE                                       
451500                        WHEN DDC-SE                                       
451600                          IF SKRIV-HUV-KNOTA-SE                           
451700                            PERFORM S35-SKAPA-HUVUD-KNOTA                 
451800                          END-IF                                          
451900                        WHEN DDC-NO                                       
452000                          IF SKRIV-HUV-KNOTA-NO                           
452100                            PERFORM S35-SKAPA-HUVUD-KNOTA                 
452200                          END-IF                                          
452300                        WHEN DDC-BE                                       
452400                          IF SKRIV-HUV-KNOTA-BE                           
452500                            PERFORM S35-SKAPA-HUVUD-KNOTA                 
452600                          END-IF                                          
452700                        WHEN OTHER                                        
452800                          IF SKRIV-HUV-KNOTA-SDC                          
452900                            PERFORM S35-SKAPA-HUVUD-KNOTA                 
453000                          END-IF                                          
453100                      END-EVALUATE                                        
453200                    END-IF                                                
453300                  END-IF                                                  
453400                END-IF                                                    
453500                                                                          
453600*** ITALIEN SKALL INTE HA MOMS PÅ VISSA KODER                             
453700                IF LEV-KDANMORS =                                         
453800                   '42' OR '52' OR '62' OR '72' OR '75' OR                
453900                   '82' OR '92' OR '97' OR '98'                           
454000                   MOVE NEJ         TO SKRIV-MOMS-ITL                     
454100                END-IF                                                    
454200                                                                          
454300                IF LEV-IDDC NOT = DCS-IDDC                                
454400                   MOVE LEV-IDDC TO W-IDDC-B6                             
454500                   PERFORM IMS-GU-WDB601                                  
454600                END-IF                                                    
454700                IF LEV-IDDC-RET NOT = RET-DCS-IDDC                        
454800                   MOVE LEV-IDDC-RET TO W-IDDC-B6-RET                     
454900                   PERFORM IMS-GU-WDB601-RET                              
455000                END-IF                                                    
455100                                                                          
455200                IF RET-DCS-CDC OR RET-DCS-NDC-PF                          
455300           OR (RET-DCS-SDC AND NOT RET-DCS-LAND-NON-VCC-OWNED)            
455400                                                                          
455500                  IF LEV-KDANMORS = '72' OR '98'                          
455600                    MOVE JA           TO HANDLING-FEE-72-98-SW            
455700                    IF LEV-KDANMORS = '72'                                
455800                      ADD +1          TO WS-KVRADER-72                    
455900                    END-IF                                                
456000                                                                          
456100                    IF LEV-KDANMORS = '98'                                
456200                      ADD +1          TO WS-KVRADER-98                    
456300                    END-IF                                                
456400                  END-IF                                                  
456500                END-IF                                                    
456600                                                                          
456700                IF LEV-KDANMORS = '99'                                    
456800                   CONTINUE                                               
456900                ELSE                                                      
457000*- KOD 74 SKAPAR INTERN DOKUMENT VIA BILL-IT, EJ KREDITNOTA.              
457100                  IF IDFTG-PV OR IDFTG-NON-VCC                            
457200                    MOVE JA       TO KNOTA-VIA-BILLIT-SW                  
457300                    MOVE ZERO     TO 31B-KDAVVTYP                         
457400                    MOVE NEJ      TO 31B-FLINVUPD                         
457500                    PERFORM S46-SKAPA-KN-RAD-BILLIT                       
457600                  ELSE                                                    
457700                    PERFORM S36-SKAPA-RADPOST-KNOTA                       
457800                    PERFORM S60-RADPOST-EKO-LAB                           
457900                  END-IF                                                  
458000                END-IF                                                    
458100                                                                          
458200***** ÄNDRING 990201 ENLIGT BOSSE H , INGEN EKO-TRANS FÖR KOD 99          
458300*     TILL EKONOMI. ENBART DEN FRÅN RETUR-PGM W40792 OCH W40797,          
458400*     DÄR SALDOT BERÖRS VID INLÄGGNING AV RETURAVD. (EKHT-)               
458500*****************************************************************         
458600              END-IF                                                      
458700           END-IF                                                         
458800        END-IF                                                            
458900        MOVE NEJ TO SKRIV-BYTES-SW                                        
459000      END-IF                                                              
459100        PERFORM IMS-GNP-KREE-LEV                                          
459200     END-PERFORM                                                          
459300     .                                                                    
459400     EJECT                                                                
459500 FA-BEHANDLA-KOD-12-22 SECTION.                                           
459600                                                                          
459700*- MAN SKALL ENBART SKAPA TF OM KUNDEN ÄR GODKÄND FÖR R-FAKTURA.          
459800     MOVE ANM-IDDISTR                     TO W-IDDISTR-WDB2               
459900     MOVE ANM-IDKUNDNR                    TO W-IDKUNDNR-WDB2              
460000     PERFORM IMS-GET-WLGMTA01-UNIK                                        
460100     IF SEGMENT-FINNS AND (GMT-FLOKFAK-R = 'J' OR 'Y')                    
460200                                                                          
460300       IF LEV-IDARTNR = 100                                               
460400         PERFORM FAA-SKAPA-LISTFIL                                        
460500                                                                          
460600       ELSE                                                               
460700         IF LEV-IDDC = IDDC-WS                                            
460800            CONTINUE                                                      
460900         ELSE                                                             
461000            PERFORM S05-HAEMTA-ORDERNR                                    
461100         END-IF                                                           
461200                                                                          
461300         MOVE LEV-IDDC                TO IDDC-WS                          
461400                                                                          
461500         MOVE '001'                 TO UT34-IDPTYP                        
461600         MOVE ANM-IDLEVANM          TO UT34-IDLEVANM                      
461700         MOVE LEV-IDARTNR           TO UT34-IDARTNR                       
461800         MOVE LEV-IDRADNR           TO UT34-IDRADNR                       
461900         MOVE SPACE                 TO UT34-IDDC                          
462000         MOVE SPACE                 TO UT34-FLFARLIG                      
462100         MOVE IN-IDORDNR            TO UT34-IDKNOTNR                      
462200                                       VIR-IDKNOTNR                       
462300         MOVE ZERO                  TO UT34-KDAVVTYP                      
462400         MOVE 'R'                   TO UT34-KDFAKTYP-KNOT                 
462500         MOVE SPACE                 TO UT34-KDLEVANM                      
462600         MOVE ZERO                  TO UT34-KVLEVANM                      
462700         MOVE DAGENS-DATUM          TO UT34-TIKNOTA                       
462800         MOVE ZERO                  TO UT34-TIRETILL                      
462900                                       UT34-KVRADER-RT                    
463000         MOVE ZERO                  TO UT34-PRARTBTO                      
463100         MOVE ZERO                  TO UT34-KDINVKAT                      
463200         MOVE ZERO                  TO UT34-KVJUSTKV                      
463300         MOVE ZERO                  TO UT34-TIM-INV                       
463400         MOVE SPACE                 TO UT34-TEINVANM                      
463500         MOVE SPACE                 TO UT34-KDARBTYP                      
463600         MOVE ZERO                  TO UT34-IDPERSON                      
463700         MOVE SPACE                 TO UT34-IDDC-RET                      
463800         MOVE ZERO                  TO UT34-IXDCCLEAR                     
463900                                                                          
464000         PERFORM S20-SKRIV-W41834                                         
464100                                                                          
464200         PERFORM FAB-SKAPA-TF-DR5                                         
464300                                                                          
464400         IF ANM-IDDISTR = 778                                             
464500           PERFORM FAC-SKAPA-EKOFIL-RAD                                   
464600         END-IF                                                           
464700       END-IF                                                             
464800     END-IF                                                               
464900                                                                          
465000     .                                                                    
465100     EJECT                                                                
465200 FAA-SKAPA-LISTFIL SECTION.                                               
465300                                                                          
465400     MOVE ANM-IDDISTR               TO UT3H-IDDISTR                       
465500     MOVE ANM-IDKUNDNR              TO UT3H-IDKUNDNR                      
465600     MOVE ANM-IDRAPPNR              TO UT3H-IDRAPPNR                      
465700     MOVE WS-KVLEVANM-BEKR          TO UT3H-KVBEART                       
465800     MOVE DAGENS-DATUM              TO UT3H-TIREGDAT                      
465900                                                                          
466000     MOVE '3H '                     TO W-IDPTYP                           
466100     PERFORM S26-SKRIV-W4183H                                             
466200     .                                                                    
466300     EJECT                                                                
466400 FAB-SKAPA-TF-DR5 SECTION.                                                
466500                                                                          
466600     MOVE SPACE                     TO DR5-W418DR5                        
466700     MOVE 'DR5'                     TO DR5-IDPTYP                         
466800     MOVE ANM-IDDISTR               TO WS-IDDISTR                         
466900     MOVE WS-IDDISTR-ALFA           TO DR5-IDDISTR                        
467000     MOVE ANM-IDKUNDNR              TO WS-IDKUNDNR                        
467100     MOVE WS-IDKUNDNR-ALFA          TO DR5-IDKUNDNR                       
467200     MOVE ANM-IDRAPPNR              TO WS-IDRAPPNR                        
467300     MOVE WS-IDRAPPNR-ALFA          TO DR5-IDRAPPNR                       
467400                                                                          
467500     IF GOOD-DDC                                                          
467600       MOVE '11'                    TO DR5-IDDC                           
467700     ELSE                                                                 
467800       MOVE LEV-IDDC                TO DR5-IDDC                           
467900     END-IF                                                               
468000                                                                          
468100     MOVE IN-IDORDNR                TO DR5-IDORDNR5                       
468200     MOVE LEV-IDARTNR               TO DR5-IDARTNR                        
468300     MOVE WS-KVLEVANM-BEKR-ALFA     TO DR5-KVBEART                        
468400     MOVE WS-KVLEVANM-BEKR          TO VIR-KVLEVANM                       
468500     MOVE SPACE                     TO DR5-PRARTNTO                       
468600                                                                          
468700     PERFORM S66-KOLLA-OK-ORDERKLASS                                      
468800     IF KLASS-1-OK                                                        
468900       MOVE 1                       TO DR5-KDORDKL                        
469000     ELSE                                                                 
469100       IF KLASS-0-OK                                                      
469200         MOVE 0                     TO DR5-KDORDKL                        
469300       ELSE                                                               
469400         MOVE 3                     TO DR5-KDORDKL                        
469500       END-IF                                                             
469600     END-IF                                                               
469700                                                                          
469800     MOVE ANM-IDDISTR               TO TEST-IDDISTR                       
469900     MOVE NEJ                       TO DR5-FLLSBOK                        
470000     MOVE LEV-IDRADNR               TO DR5-IDRADNR                        
470100                                                                          
470200     PERFORM S15-SKRIV-W41836                                             
470300                                                                          
470400     IF GOOD-DDC                                                          
470500       IF OKOD-FL-LEVERANTOER = JA                                        
470600         PERFORM S42-SKAPA-RADER-VIR                                      
470700       END-IF                                                             
470800     END-IF                                                               
470900     .                                                                    
471000     EJECT                                                                
471100 FAC-SKAPA-EKOFIL-RAD SECTION.                                            
471200                                                                          
471300     MOVE LEV-IDDC                TO EKHT-IDDC-SEND                       
471400     MOVE ' '                     TO EKHT-IDDC-REC                        
471500                                                                          
471600     IF LEV-KDANMORS = '26' OR '11' OR '21'                               
471700       IF LEV-KDANMORS = '26'                                             
471800         MOVE '204'                   TO EKHT-KDEKHHT                     
471900         MOVE '203'                   TO EKHT-KDEKSHT                     
472000       ELSE                                                               
472100         MOVE '403'                   TO EKHT-KDEKHHT                     
472200         MOVE '408'                   TO EKHT-KDEKSHT                     
472300         MOVE LEV-IDDC-RET            TO EKHT-IDDC-REC                    
472400       END-IF                                                             
472500     ELSE                                                                 
472600       MOVE '303'                   TO EKHT-KDEKHHT                       
472700                                                                          
472800       IF LEV-KDANMORS = '12' OR '22'                                     
472900         MOVE '311'                 TO EKHT-KDEKSHT                       
473000       ELSE                                                               
473100         IF LEV-KDANMORS = '27'                                           
473200           MOVE '316'               TO EKHT-KDEKSHT                       
473300         ELSE                                                             
473400           MOVE 0                   TO EKHT-KDEKSHT (1:1)                 
473500           MOVE LEV-KDANMORS        TO EKHT-KDEKSHT (2:2)                 
473600         END-IF                                                           
473700       END-IF                                                             
473800     END-IF                                                               
473900                                                                          
474000     MOVE 'DET'                   TO EKHT-KDEKNIVA                        
474100     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
474200     MOVE FUNCTION CURRENT-DATE (9:8) TO EKHT-TIKLOCK                     
474300     MOVE 1                       TO EKHT-IDSEKVNR                        
474400     MOVE ANM-IDDISTR             TO EKHT-IDDISTR                         
474500     MOVE ANM-IDKUNDNR            TO EKHT-IDKUNDNR                        
474600                                                                          
474700     MOVE 'VO'                    TO CIA-IDARTPRE-IN                      
474800     MOVE ANM-IDRAPPNR            TO CIA-IDARTBET-IN                      
474900     CALL W009CIA USING              CIA-W009CIA                          
475000     MOVE CIA-IDARTBET-UT         TO EKHT-IDVERGL                         
475100                                                                          
475200     MOVE DAGENS-DATUM-SEKEL      TO EKHT-DAVERDAT                        
475300     MOVE 0                       TO EKHT-KDPRODSL                        
475400                                                                          
475500     IF LEV-KDANMORS = '11' OR '21'                                       
475600       MOVE ART-KDPRODSL          TO EKHT-KDPRODSL                        
475700     END-IF                                                               
475800                                                                          
475900     MOVE 0                       TO EKHT-KDPSLLOC                        
476000     MOVE LEV-IDARTNR             TO EKHT-IDARTNR                         
476100     MOVE NEJ                     TO EKHT-FLLSBOK                         
476200     MOVE 'SEK'                   TO EKHT-KDVALISO                        
476300     MOVE 1.0                     TO EKHT-PRKURS                          
476400     MOVE 0                       TO EKHT-PRARTNTO                        
476500     MOVE 0                       TO EKHT-PRARTSJK                        
476600     MOVE 0                       TO EKHT-PRHEMTAG                        
476700     MOVE CLAG-PRARTSTD           TO EKHT-PRARTSTD                        
476800     MOVE 0                       TO EKHT-PRINK                           
476900     MOVE 0                       TO EKHT-PRDIRLON                        
477000     MOVE 0                       TO EKHT-PRDMTRL                         
477100     MOVE 0                       TO EKHT-PROVRPAL                        
477200     MOVE 0                       TO EKHT-PRLANDCO                        
477300     MOVE WS-KVLEVANM-BEKR        TO EKHT-KVANTAL                         
477400     MOVE 0                       TO EKHT-SUBEL                           
477500     MOVE 'W4183000'              TO EKHT-IDPGM                           
477600     MOVE ' '                     TO EKHT-IDTRANS                         
477700     MOVE 'W510EKHA'              TO EKHT-IDCPYTXT                        
477800     MOVE LEV-KDANMORS            TO EKHT-KDANMORS                        
477900     MOVE LEV-IDANALYS            TO EKHT-IDANALYS                        
478000     MOVE LEV-IDKONTO             TO EKHT-IDKONTO                         
478100     MOVE LEV-IDKST               TO EKHT-IDKST                           
478200     MOVE ZERO                    TO EKHT-BEVAT                           
478300                                     EKHT-KDFRAKT                         
478400     MOVE ZERO                    TO EKHT-DAAVIDAT                        
478500                                     EKHT-IDAVINR                         
478600                                     EKHT-KDAVVTYP                        
478700                                     EKHT-KDRT                            
478800                                     EKHT-KVANTMOT                        
478900                                     EKHT-KVAVIS                          
479000                                     EKHT-SUVAT                           
479100     MOVE WS-KDSORT               TO EKHT-KDSORT                          
479200     MOVE SPACE                   TO EKHT-KDTRADP                         
479300                                     EKHT-IDLEVNR                         
479400     MOVE SPACE                   TO EKHT-FLOVRLEV                        
479500     MOVE ZERO                    TO EKHT-IDORDNR5                        
479600     MOVE SPACE                   TO EKHT-IDUSER                          
479700     IF LEV-IDDC = WC-SDC-NL-ET                                           
479800       MOVE JA                    TO EKHT-FLDCET                          
479900     ELSE                                                                 
480000       MOVE NEJ                   TO EKHT-FLDCET                          
480100     END-IF                                                               
480200     MOVE SPACE                   TO EKHT-IDKUNDRF                        
480300     MOVE LEV-IDFAKT              TO EKHT-IDFAKT-EXP                      
480400                                                                          
480500     PERFORM S06-SKRIV-EKONOMIFIL                                         
480600     .                                                                    
480700     EJECT                                                                
480800 FC-KOLLA-OM-KNOTA-FINNS-TID SECTION.                                     
480900*    DISPLAY '***** FC-KOLLA-OM-KNOTA-FINNS'                              
481000                                                                          
481100     MOVE JA TO  BARA-N7X-SW                                              
481200                                                                          
481300     PERFORM IMS-GU-KREE-ANM                                              
481400     PERFORM IMS-GNP-KREE-LEV                                             
481500     PERFORM UNTIL SEGMENT-SAKNAS                                         
481600        IF LEV-IDKNOTNR > +0                                              
481700           MOVE JA TO TKOST-SKRIVNA-FOERUT-SW                             
481800        END-IF                                                            
481900                                                                          
482000        MOVE LEV-KDKREBEH                 TO W-KDKREBEH                   
482100        IF W-BOKST = 'N'                                                  
482200          CONTINUE                                                        
482300        ELSE                                                              
482400           MOVE NEJ TO BARA-N7X-SW                                        
482500        END-IF                                                            
482600                                                                          
482700        PERFORM IMS-GNP-KREE-LEV                                          
482800     END-PERFORM                                                          
482900     .                                                                    
483000     EJECT                                                                
483100 H-UPPDATERA-STATUS SECTION.                                              
483200*    DISPLAY '*** H-UPPDATERA-STATUS '                                    
483300                                                                          
483400     MOVE '010'                     TO UT34-IDPTYP                        
483500     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
483600     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
483700     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
483800     MOVE SPACE                     TO UT34-IDDC                          
483900     MOVE ZERO                      TO UT34-IDKNOTNR                      
484000                                       UT34-KDAVVTYP                      
484100                                       UT34-KDFAKTYP-KNOT                 
484200                                       UT34-KVLEVANM                      
484300                                       UT34-TIKNOTA                       
484400                                       UT34-TIRETILL                      
484500     MOVE WS-KVRADER-RT             TO UT34-KVRADER-RT                    
484600     IF RETILL-FINNS                                                      
484700        MOVE '4'                    TO UT34-KDLEVANM                      
484800        IF FARLIGT-GODS-FINNS                                             
484900           MOVE JA                  TO UT34-FLFARLIG                      
485000        ELSE                                                              
485100           MOVE NEJ                 TO UT34-FLFARLIG                      
485200        END-IF                                                            
485300        PERFORM HA-HAEMTA-ANSVARIG                                        
485400     ELSE                                                                 
485500        MOVE '8'                    TO UT34-KDLEVANM                      
485600        MOVE NEJ                    TO UT34-FLFARLIG                      
485700     END-IF                                                               
485800     MOVE ZERO                      TO UT34-PRARTBTO                      
485900     MOVE ZERO                      TO UT34-KDINVKAT                      
486000     MOVE ZERO                      TO UT34-KVJUSTKV                      
486100     MOVE ZERO                      TO UT34-TIM-INV                       
486200     MOVE SPACE                     TO UT34-TEINVANM                      
486300     MOVE SPACE                     TO UT34-IDDC-RET                      
486400     MOVE ZERO                      TO UT34-IXDCCLEAR                     
486500                                                                          
486600     PERFORM S20-SKRIV-W41834                                             
486700     .                                                                    
486800     EJECT                                                                
486900 HA-HAEMTA-ANSVARIG SECTION.                                              
487000*    DISPLAY '*** HA-HAEMTA-ANSVARIG '                                    
487100                                                                          
487200     MOVE +3                             TO ANSV-KDCALL                   
487300     MOVE ANM-IDDISTR                    TO ANSV-IDDISTR                  
487400                                                                          
487500     IF AENDRA-IDDC-RET                                                   
487600       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
487700          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
487800          DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                     
487900          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
488000          DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                    
488100         MOVE GMT-IDDC-RET               TO ANSV-IDDC                     
488200       ELSE                                                               
488300         MOVE WC-CDC-SE                  TO ANSV-IDDC                     
488400       END-IF                                                             
488500     ELSE                                                                 
488600       MOVE ANM-IDDC-RET                 TO ANSV-IDDC                     
488700     END-IF                                                               
488800                                                                          
488900     MOVE ANM-IDKUNDNR                   TO ANSV-IDKUNDNR                 
489000     MOVE ANM-IDFTG                      TO ANSV-IDFTG                    
489100     MOVE LEV-KDANMORS                   TO ANSV-KDANMORS                 
489200     MOVE ZERO                           TO ANSV-KDORDKL                  
489300                                            ANSV-ADLAGOMR                 
489400                                                                          
489500     CALL W418ANSV USING ANSV-W418ANSV 4113-PCB 4115-PCB 4117-PCB         
489600                                                                          
489700     IF ANSV-OK                                                           
489800        MOVE ANSV-KDARBTYP               TO UT34-KDARBTYP                 
489900                                            31A-KDARBTYP                  
490000        MOVE ANSV-IDPERSON               TO UT34-IDPERSON                 
490100                                            31A-IDPERSON                  
490200     ELSE                                                                 
490300        MOVE 'RET'                       TO UT34-KDARBTYP                 
490400                                            31A-KDARBTYP                  
490500        MOVE 9                           TO UT34-IDPERSON                 
490600                                            31A-IDPERSON                  
490700     END-IF                                                               
490800     .                                                                    
490900     EJECT                                                                
491000 O-UPPDATERA-STATUS-4 SECTION.                                            
491100*    DISPLAY '*** O-UPPDATERA-STATUS-4 '                                  
491200                                                                          
491300* FÖR ATT MÖJLIGGÖRA UPPDAT AV TIRETILL PÅ A2 I W41835                    
491400     MOVE '003'                     TO UT34-IDPTYP                        
491500     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
491600     MOVE ZERO                      TO UT34-IDARTNR                       
491700     MOVE ZERO                      TO UT34-IDRADNR                       
491800     MOVE SPACE                     TO UT34-IDDC                          
491900     MOVE SPACE                     TO UT34-FLFARLIG                      
492000     MOVE ZERO                      TO UT34-IDKNOTNR                      
492100                                       UT34-KDAVVTYP                      
492200                                       UT34-KDFAKTYP-KNOT                 
492300                                       UT34-KVRADER-RT                    
492400     MOVE SPACE                     TO UT34-KDLEVANM                      
492500     MOVE ZERO                      TO UT34-KVLEVANM                      
492600     MOVE ZERO                      TO UT34-TIKNOTA                       
492700     MOVE DAGENS-DATUM              TO UT34-TIRETILL                      
492800     MOVE ZERO                      TO UT34-PRARTBTO                      
492900     MOVE ZERO                      TO UT34-KDINVKAT                      
493000     MOVE ZERO                      TO UT34-KVJUSTKV                      
493100     MOVE ZERO                      TO UT34-TIM-INV                       
493200     MOVE SPACE                     TO UT34-TEINVANM                      
493300     MOVE SPACE                     TO UT34-KDARBTYP                      
493400     MOVE ZERO                      TO UT34-IDPERSON                      
493500     MOVE SPACE                     TO UT34-IDDC-RET                      
493600     MOVE ZERO                      TO UT34-IXDCCLEAR                     
493700                                                                          
493800     PERFORM S20-SKRIV-W41834                                             
493900                                                                          
494000* FÖR ATT UPPDATERA STATUS PÅ A2 I W41835                                 
494100     MOVE '010'                     TO UT34-IDPTYP                        
494200     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
494300     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
494400     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
494500     MOVE SPACE                     TO UT34-IDDC                          
494600     MOVE ZERO                      TO UT34-IDKNOTNR                      
494700                                       UT34-KDAVVTYP                      
494800                                       UT34-KDFAKTYP-KNOT                 
494900                                       UT34-KVLEVANM                      
495000                                       UT34-TIKNOTA                       
495100     MOVE DAGENS-DATUM              TO UT34-TIRETILL                      
495200     MOVE WS-KVRADER-RT             TO UT34-KVRADER-RT                    
495300     MOVE '4'                       TO UT34-KDLEVANM                      
495400     IF FARLIGT-GODS-FINNS                                                
495500        MOVE JA                     TO UT34-FLFARLIG                      
495600     ELSE                                                                 
495700        MOVE NEJ                    TO UT34-FLFARLIG                      
495800     END-IF                                                               
495900     PERFORM HA-HAEMTA-ANSVARIG                                           
496000                                                                          
496100     PERFORM S20-SKRIV-W41834                                             
496200     .                                                                    
496300     EJECT                                                                
496400 I-UPPDATERA-STATUS-9 SECTION.                                            
496500*    DISPLAY '*** I-UPPDATERA-STATUS-9 '                                  
496600                                                                          
496700     MOVE '31A'                     TO 31A-IDPTYP                         
496800     MOVE ANM-IDDISTR               TO 31A-IDDISTR                        
496900     MOVE ANM-IDKUNDNR              TO 31A-IDKUNDNR                       
497000     MOVE ANM-IDRAPPNR              TO 31A-IDRAPPNR                       
497100     MOVE ZERO                      TO 31A-IDDC                           
497200     MOVE WS-KVRADER-RT             TO 31A-KVRADER-RT                     
497300     IF RETILL-FINNS                                                      
497400        MOVE '4'                    TO 31A-KDLEVANM-UPD                   
497500        IF FARLIGT-GODS-FINNS                                             
497600           MOVE JA                  TO 31A-FLFARLIG                       
497700        ELSE                                                              
497800           MOVE NEJ                 TO 31A-FLFARLIG                       
497900        END-IF                                                            
498000        PERFORM HA-HAEMTA-ANSVARIG                                        
498100     ELSE                                                                 
498200        MOVE '8'                    TO 31A-KDLEVANM-UPD                   
498300        MOVE NEJ                    TO 31A-FLFARLIG                       
498400        MOVE ZERO                   TO 31A-IDPERSON                       
498500        MOVE SPACE                  TO 31A-KDARBTYP                       
498600     END-IF                                                               
498700                                                                          
498800     PERFORM S32-SKRIV-W418AI                                             
498900     .                                                                    
499000     EJECT                                                                
499100 J-KOLLA-OM-RETUR-DC-OK     SECTION.                                      
499200                                                                          
499300     MOVE NEJ TO  AENDRA-IDDC-RET-SW                                      
499400                                                                          
499500     PERFORM IMS-GU-KREE-ANM                                              
499600                                                                          
499700     MOVE ANM-IDDC-RET        TO W-IDDC-B6                                
499800                                 W-IDDC                                   
499900     PERFORM IMS-GU-WDB601                                                
500000     IF SEGMENT-SAKNAS                                                    
500100       MOVE JA TO AENDRA-IDDC-RET-SW                                      
500200     ELSE                                                                 
500300       PERFORM IMS-GNP-KREE-LEV                                           
500400       PERFORM UNTIL SEGMENT-SAKNAS  OR AENDRA-IDDC-RET-SW = JA           
500500         IF LEV-KDKREBEH(1:1) = 'N'                                       
500600           CONTINUE                                                       
500700         ELSE                                                             
500800           MOVE LEV-KDANMORS  TO OKOD-KDANMORS                            
500900           CALL W418OKOD USING OKOD-W418OKOD                              
501000                                                                          
501100           IF OKOD-FL-RETILL = JA                                         
501200             PERFORM JA-KOLLA-RETUR-DC-2                                  
501300           END-IF                                                         
501400         END-IF                                                           
501500                                                                          
501600          PERFORM IMS-GNP-KREE-LEV                                        
501700       END-PERFORM                                                        
501800     END-IF                                                               
501900     .                                                                    
502000     EJECT                                                                
502100 JA-KOLLA-RETUR-DC-2  SECTION.                                            
502200                                                                          
502300     MOVE NEJ                       TO  GODK-KOD-SW                       
502400     MOVE JA                        TO  GODK-ARTIKEL-SW                   
502500                                        GODK-IDFKNGRP-SW                  
502600                                        GODK-DC-ARTIKEL-SW                
502700                                        GODK-DC-LEV-SW                    
502800                                                                          
502900     IF DCS-FLARTDC = JA                                                  
503000*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
503100       MOVE LEV-IDARTNR TO W-IDARTNR-K7                                   
503200       PERFORM IMS-GU-WDK711                                              
503300       IF SEGMENT-SAKNAS                                                  
503400         MOVE NEJ                   TO GODK-DC-ARTIKEL-SW                 
503500       END-IF                                                             
503600     END-IF                                                               
503700                                                                          
503800     IF EJ-GODK-DC-ARTIKEL                                                
503900       CONTINUE                                                           
504000     ELSE                                                                 
504100       MOVE SPACE           TO W-URV-TEELMT                               
504200       MOVE WC-IDARTNR      TO W-URV-TEELMT                               
504300       MOVE SPACE           TO W-URV-FILLER                               
504400       MOVE LEV-IDARTNR     TO W-URV-IDARTNR-EXCP                         
504500                                                                          
504600       PERFORM IMS-GNP-WDB611-FIRST                                       
504700       IF SEGMENT-FINNS                                                   
504800         MOVE NEJ                TO GODK-ARTIKEL-SW                       
504900       ELSE                                                               
505000         MOVE LEV-IDARTNR        TO W-IDARTNR                             
505100         PERFORM IMS-GU-ARTC01                                            
505200         IF SEGMENT-SAKNAS                                                
505300           MOVE NEJ  TO GODK-DC-ARTIKEL-SW                                
505400         ELSE                                                             
505500           MOVE SPACE            TO W-URV-TEELMT                          
505600           MOVE WC-IDFKNGRP      TO W-URV-TEELMT                          
505700           MOVE SPACE            TO W-URV-FILLER                          
505800           MOVE ART-IDFKNGRP     TO W-URV-IDFKNGRP-EXCP                   
505900                                                                          
506000           PERFORM IMS-GNP-WDB611-FIRST                                   
506100           IF SEGMENT-FINNS                                               
506200             MOVE NEJ            TO GODK-IDFKNGRP-SW                      
506300           ELSE                                                           
506400             MOVE SPACE            TO W-URV-TEELMT                        
506500             MOVE WC-IDDC-EXCP     TO W-URV-TEELMT                        
506600             MOVE SPACE            TO W-URV-FILLER                        
506700             MOVE LEV-IDDC         TO W-URV-IDDC-EXCP                     
506800                                                                          
506900             PERFORM IMS-GNP-WDB611-FIRST                                 
507000             IF SEGMENT-FINNS                                             
507100               MOVE NEJ            TO GODK-DC-LEV-SW                      
507200             ELSE                                                         
507300               MOVE SPACE          TO W-URV-TEELMT                        
507400               MOVE WC-KDANMORS    TO W-URV-TEELMT                        
507500               MOVE SPACE          TO W-URV-FILLER                        
507600               MOVE LEV-KDANMORS   TO W-URV-KDANMORS-RET                  
507700                                                                          
507800               PERFORM IMS-GNP-WDB611-FIRST                               
507900               IF SEGMENT-FINNS                                           
508000                 MOVE JA           TO GODK-KOD-SW                         
508100               END-IF                                                     
508200             END-IF                                                       
508300           END-IF                                                         
508400         END-IF                                                           
508500       END-IF                                                             
508600     END-IF                                                               
508700                                                                          
508800     IF EJ-GODK-DC-ARTIKEL OR                                             
508900        EJ-GODK-ARTIKEL OR                                                
509000        EJ-GODK-IDFKNGRP OR                                               
509100        EJ-GODK-DC-LEV OR                                                 
509200        EJ-GODK-KOD                                                       
509300                                                                          
509400        MOVE JA TO AENDRA-IDDC-RET-SW                                     
509500     END-IF                                                               
509600     .                                                                    
509700     EJECT                                                                
509800 K-UPPDATERA-IDDC-RET SECTION.                                            
509900                                                                          
510000     MOVE '009'                     TO UT34-IDPTYP                        
510100     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
510200     MOVE ZERO                      TO UT34-IDARTNR                       
510300                                       UT34-IDRADNR                       
510400     MOVE SPACE                     TO UT34-IDDC                          
510500                                       UT34-FLFARLIG                      
510600     MOVE ZERO                      TO UT34-IDKNOTNR                      
510700                                       UT34-IDPERSON                      
510800     MOVE SPACE                     TO UT34-KDARBTYP                      
510900     MOVE ZERO                      TO UT34-KDAVVTYP                      
511000     MOVE SPACE                     TO UT34-KDFAKTYP-KNOT                 
511100                                       UT34-KDLEVANM                      
511200     MOVE ZERO                      TO UT34-KVLEVANM                      
511300                                       UT34-KVRADER-RT                    
511400                                       UT34-PRARTBTO                      
511500                                       UT34-TIKNOTA                       
511600                                       UT34-TIRETILL                      
511700                                       UT34-KDINVKAT                      
511800                                       UT34-KVJUSTKV                      
511900                                       UT34-TIM-INV                       
512000     MOVE SPACE                     TO UT34-TEINVANM                      
512100                                                                          
512200     IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                           
512300        DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                          
512400        DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                       
512500        DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                       
512600        DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                      
512700       MOVE GMT-IDDC-RET            TO UT34-IDDC-RET                      
512800     ELSE                                                                 
512900       MOVE WC-CDC-SE               TO UT34-IDDC-RET                      
513000     END-IF                                                               
513100                                                                          
513200     MOVE 3                         TO UT34-IXDCCLEAR                     
513300                                                                          
513400     PERFORM S20-SKRIV-W41834                                             
513500     .                                                                    
513600     EJECT                                                                
513700 L-KOLLA-RETUR-DC-WDA211     SECTION.                                     
513800                                                                          
513900     PERFORM IMS-GU-KREE-ANM                                              
514000                                                                          
514100     PERFORM IMS-GNP-KREE-LEV                                             
514200     PERFORM UNTIL SEGMENT-SAKNAS                                         
514300         IF LEV-KDKREBEH(1:1) = 'N'                                       
514400           CONTINUE                                                       
514500         ELSE                                                             
514600           MOVE LEV-KDANMORS  TO OKOD-KDANMORS                            
514700           CALL W418OKOD USING OKOD-W418OKOD                              
514800                                                                          
514900           IF OKOD-FL-RETILL = JA                                         
515000             IF AENDRA-IDDC-RET                                           
515100               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
515200                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
515300                  DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR             
515400                  DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR             
515500                  DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC            
515600                 IF LEV-IDDC-RET = GMT-IDDC-RET                           
515700                   CONTINUE                                               
515800                 ELSE                                                     
515900                   PERFORM LA-UPPDATERA-IDDC-RET-LEV                      
516000                 END-IF                                                   
516100               ELSE                                                       
516200                 IF LEV-IDDC-RET = WC-CDC-SE                              
516300                   CONTINUE                                               
516400                 ELSE                                                     
516500                   PERFORM LA-UPPDATERA-IDDC-RET-LEV                      
516600                 END-IF                                                   
516700               END-IF                                                     
516800             ELSE                                                         
516900               IF ANM-IDDC-RET = LEV-IDDC-RET                             
517000                 CONTINUE                                                 
517100               ELSE                                                       
517200                 PERFORM LA-UPPDATERA-IDDC-RET-LEV                        
517300               END-IF                                                     
517400             END-IF                                                       
517500           END-IF                                                         
517600         END-IF                                                           
517700                                                                          
517800       PERFORM IMS-GNP-KREE-LEV                                           
517900     END-PERFORM                                                          
518000     .                                                                    
518100     EJECT                                                                
518200 LA-UPPDATERA-IDDC-RET-LEV SECTION.                                       
518300                                                                          
518400     MOVE '011'                     TO UT34-IDPTYP                        
518500     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
518600     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
518700     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
518800     MOVE SPACE                     TO UT34-IDDC                          
518900                                       UT34-FLFARLIG                      
519000     MOVE ZERO                      TO UT34-IDKNOTNR                      
519100                                       UT34-IDPERSON                      
519200     MOVE SPACE                     TO UT34-KDARBTYP                      
519300     MOVE ZERO                      TO UT34-KDAVVTYP                      
519400     MOVE SPACE                     TO UT34-KDFAKTYP-KNOT                 
519500                                       UT34-KDLEVANM                      
519600     MOVE ZERO                      TO UT34-KVLEVANM                      
519700                                       UT34-KVRADER-RT                    
519800                                       UT34-PRARTBTO                      
519900                                       UT34-TIKNOTA                       
520000                                       UT34-TIRETILL                      
520100                                       UT34-KDINVKAT                      
520200                                       UT34-KVJUSTKV                      
520300                                       UT34-TIM-INV                       
520400     MOVE SPACE                     TO UT34-TEINVANM                      
520500                                                                          
520600     IF AENDRA-IDDC-RET                                                   
520700       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
520800          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
520900          DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                     
521000          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
521100          DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                    
521200         MOVE GMT-IDDC-RET          TO UT34-IDDC-RET                      
521300       ELSE                                                               
521400         MOVE WC-CDC-SE             TO UT34-IDDC-RET                      
521500       END-IF                                                             
521600     ELSE                                                                 
521700       MOVE ANM-IDDC-RET            TO UT34-IDDC-RET                      
521800     END-IF                                                               
521900                                                                          
522000     MOVE ZERO                      TO UT34-IXDCCLEAR                     
522100                                                                          
522200     PERFORM S20-SKRIV-W41834                                             
522300     .                                                                    
522400     EJECT                                                                
522500 M-SKAPA-RADPOST-FAKT-FEE SECTION.                                        
522600*    DISPLAY '*** M-SKAPA-RADPOST-FAKT-FEE'                               
522700                                                                          
522800     MOVE 'RET'                          TO UTAT-RP-IDPTYP                
522900     MOVE ANM-IDDC-RET                   TO UTAT-RP-IDDC                  
523000     MOVE ANM-IDDISTR                    TO UTAT-RP-IDDISTR               
523100     MOVE ANM-IDKUNDNR                   TO UTAT-RP-IDKUNDNR              
523200     MOVE ANM-IDRAPPNR                   TO UTAT-RP-IDRAPPNR              
523300     MOVE SPACE                          TO UTAT-RP-IDRAPP                
523400     MOVE WS-KVRADER-72                  TO UTAT-RP-KVRADER-72            
523500     MOVE WS-KVRADER-98                  TO UTAT-RP-KVRADER-98            
523600     MOVE ZERO                           TO UTAT-RP-KVRADER-RET           
523700     MOVE ANM-DARETANK                   TO UTAT-RP-DARETANK              
523800     MOVE ZERO                           TO UTAT-RP-DARFSDAT              
523900                                                                          
524000     PERFORM S410-SKRIV-W418AT                                            
524100     .                                                                    
524200     EJECT                                                                
524300 Z-FINIT SECTION.                                                         
524400*    DISPLAY '*** Z-FINIT '                                               
524500                                                                          
524600     CLOSE W41801-IN                                                      
524700           W41801-UT                                                      
524800           W41833                                                         
524900           W41835                                                         
525000           W41836                                                         
525100           W41837                                                         
525200           W41839                                                         
525300           W4183A                                                         
525400           W4183B                                                         
525500           W41834                                                         
525600           W4183C                                                         
525700           W4183D                                                         
525800           W4183E                                                         
525900           W4183F                                                         
526000           W4183G                                                         
526100           W4183H                                                         
526200           W4183I                                                         
526300           W4183J                                                         
526400           W4183K                                                         
526500           W4183L                                                         
526600           W4183N                                                         
526700           W4183O                                                         
526800           W4183P                                                         
526900           W4183Q                                                         
527000           W4183R                                                         
527100           W4183T                                                         
527200           W4183U                                                         
527300           W4183V                                                         
527400           W4183W                                                         
527500           W4183X                                                         
527600           W4183Y                                                         
527700           W4183Z                                                         
527800           W418AA                                                         
527900           W418AB                                                         
528000           W418AC                                                         
528100           W418AD                                                         
528200           W418AE                                                         
528300           W418AF                                                         
528400           W418AG                                                         
528500           W418AH                                                         
528600           W418AI                                                         
528700           W41830                                                         
528800           W41832                                                         
528900           W41843                                                         
529000           W418AP                                                         
529100           W418AQ                                                         
529200           W418AS                                                         
529300           W418AT                                                         
529400           W418C1                                                         
529500           W418AN                                                         
529600           W41831                                                         
529700           W418IN                                                         
529800           W418CZ                                                         
529900           W418HU                                                         
530000           W418MQA                                                        
530100     SKIP2                                                                
530200     MOVE 'S' TO POSTSUM-OPKOD                                            
530300     CALL POSTSUM USING POSTSUM-PARM                                      
530400     .                                                                    
530500     EJECT                                                                
530600 S01-LAES-W41801  SECTION.                                                
530700*    DISPLAY '*** S01-LAES-W41801'                                        
530800                                                                          
530900     READ W41801-IN INTO IN-AREA                                          
531000                                                                          
531100     NOT AT END                                                           
531200        MOVE 'W41801'                     TO POSTSUM-FDNAMN               
531300        MOVE 'W41830D1'                   TO POSTSUM-DDNAMN2              
531400        MOVE 'NRIN'                       TO POSTSUM-TRANSTYP             
531500        CALL POSTSUM USING POSTSUM-PARM                                   
531600     END-READ                                                             
531700     .                                                                    
531800     EJECT                                                                
531900 S05-HAEMTA-ORDERNR SECTION.                                              
532000*    DISPLAY '*** S05-HAEMTA-ORDERNR '                                    
532100                                                                          
532200     IF IN-IDORDNR = IN-IDORDNR-MAX                                       
532300       MOVE IN-IDORDNR-MIN          TO IN-IDORDNR                         
532400     ELSE                                                                 
532500       ADD +1                       TO IN-IDORDNR                         
532600     END-IF                                                               
532700                                                                          
532800*---KONTROLLERA OM ORDER FINNS REDAN.                                     
532900                                                                          
533000     MOVE ANM-IDDISTR               TO W-SEQC-IDDISTR                     
533100     MOVE ANM-IDKUNDNR              TO W-SEQC-IDKUNDNR                    
533200     MOVE IN-IDORDNR                TO W-SEQC-IDORDNR7                    
533300                                                                          
533400     PERFORM IMS-01-GU-WDQ2C1                                             
533500                                                                          
533600     IF SEGMENT-SAKNAS                                                    
533700       CONTINUE                                                           
533800     ELSE                                                                 
533900       PERFORM UNTIL SEGMENT-SAKNAS                                       
534000         IF IN-IDORDNR = IN-IDORDNR-MAX                                   
534100           MOVE IN-IDORDNR-MIN      TO IN-IDORDNR                         
534200         ELSE                                                             
534300           ADD +1                   TO IN-IDORDNR                         
534400         END-IF                                                           
534500         MOVE IN-IDORDNR            TO W-SEQC-IDORDNR7                    
534600                                                                          
534700         PERFORM IMS-01-GU-WDQ2C1                                         
534800       END-PERFORM                                                        
534900     END-IF                                                               
535000     .                                                                    
535100     EJECT                                                                
535200 S06-SKRIV-EKONOMIFIL SECTION.                                            
535300*    DISPLAY '*** S06-SKRIV-W41830'                                       
535400                                                                          
535500     MOVE ANM-IDFTG    TO WS-IDFTG                                        
535600                                                                          
535700     IF IDFTG-PV                                                          
535800       WRITE UT30-W41830           FROM UT30-AREA                         
535900                                                                          
536000       MOVE SPACE                           TO POSTSUM-TRANSTYP           
536100       MOVE 'W41830'                        TO POSTSUM-FDNAMN             
536200       MOVE 'W41830DS'                      TO POSTSUM-DDNAMN2            
536300       CALL POSTSUM USING POSTSUM-PARM                                    
536400     END-IF                                                               
536500     .                                                                    
536600     EJECT                                                                
536700 S07-SKRIV-EKONOMIFIL SECTION.                                            
536800*    DISPLAY '*** S07-SKRIV-W41832'                                       
536900                                                                          
537000     WRITE UT32-W41832           FROM UT32-AREA                           
537100                                                                          
537200     MOVE SPACE                           TO POSTSUM-TRANSTYP             
537300     MOVE 'W41832'                        TO POSTSUM-FDNAMN               
537400     MOVE 'W41830DM'                      TO POSTSUM-DDNAMN2              
537500     CALL POSTSUM USING POSTSUM-PARM                                      
537600     .                                                                    
537700     EJECT                                                                
537800 S07-SKRIV-EKONOMIFIL-IN SECTION.                                         
537900*    DISPLAY '*** S07-SKRIV-W41831'                                       
538000                                                                          
538100     WRITE UT32A-W41831           FROM UT32A-AREA                         
538200                                                                          
538300     MOVE SPACE                           TO POSTSUM-TRANSTYP             
538400     MOVE 'W41831'                        TO POSTSUM-FDNAMN               
538500     MOVE 'W41830EH'                      TO POSTSUM-DDNAMN2              
538600     CALL POSTSUM USING POSTSUM-PARM                                      
538700     .                                                                    
538800     EJECT                                                                
538900 S11-SKRIV-W41801 SECTION.                                                
539000*    DISPLAY '*** S11-SKRIV-W41801'                                       
539100                                                                          
539200     WRITE UT-POST FROM UT-AREA                                           
539300                                                                          
539400     MOVE 'NRUT'                          TO POSTSUM-TRANSTYP             
539500     MOVE 'W41801'                        TO POSTSUM-FDNAMN               
539600     MOVE 'W41830D2'                      TO POSTSUM-DDNAMN2              
539700     CALL POSTSUM USING POSTSUM-PARM                                      
539800     .                                                                    
539900     EJECT                                                                
540000 S13-SKRIV-W41833 SECTION.                                                
540100*    DISPLAY '*** S13-SKRIV-W41833'                                       
540200                                                                          
540300     WRITE 720-POST FROM UT33-AREA                                        
540400     MOVE W-IDPTYP                        TO POSTSUM-TRANSTYP             
540500     MOVE 'W41833'                        TO POSTSUM-FDNAMN               
540600     MOVE 'W41830D3'                      TO POSTSUM-DDNAMN2              
540700     CALL POSTSUM USING POSTSUM-PARM                                      
540800     .                                                                    
540900     EJECT                                                                
541000 S14-SKRIV-W41835 SECTION.                                                
541100*    DISPLAY '*** S14-SKRIV-W41835 '                                      
541200                                                                          
541300     MOVE ANM-IDFTG   TO WS-IDFTG                                         
541400                                                                          
541500     IF IDFTG-PV                                                          
541600        EVALUATE UT35-IDPTYP                                              
541700           WHEN '712'                                                     
541800              WRITE 712-POST FROM UT35-AREA                               
541900           WHEN '713'                                                     
542000              WRITE 713-POST FROM UT35-AREA                               
542100           WHEN '717'                                                     
542200              WRITE 717-POST FROM UT35-AREA                               
542300           WHEN '718'                                                     
542400              WRITE 718-POST FROM UT35-AREA                               
542500        END-EVALUATE                                                      
542600                                                                          
542700        MOVE UT35-IDPTYP                  TO POSTSUM-TRANSTYP             
542800        MOVE 'W41835'                     TO POSTSUM-FDNAMN               
542900        MOVE 'W41830D4'                   TO POSTSUM-DDNAMN2              
543000        CALL POSTSUM USING POSTSUM-PARM                                   
543100     END-IF                                                               
543200     .                                                                    
543300     EJECT                                                                
543400 S15-SKRIV-W41836 SECTION.                                                
543500*    DISPLAY '*** S15-SKRIV-W41836 '                                      
543600                                                                          
543700     WRITE DR5-POST FROM UT36-AREA                                        
543800                                                                          
543900     MOVE UT36-IDPTYP                     TO POSTSUM-TRANSTYP             
544000     MOVE 'W41836'                        TO POSTSUM-FDNAMN               
544100     MOVE 'W41830D5'                      TO POSTSUM-DDNAMN2              
544200     CALL POSTSUM USING POSTSUM-PARM                                      
544300     .                                                                    
544400     EJECT                                                                
544500 S16-SKRIV-W41837 SECTION.                                                
544600*    DISPLAY '*** S16-SKRIV-W41837 '                                      
544700                                                                          
544800     WRITE UT37-POST FROM UT37-AREA                                       
544900                                                                          
545000     MOVE UT37-KRED-IDPTYP                TO POSTSUM-TRANSTYP             
545100     MOVE 'W41837'                        TO POSTSUM-FDNAMN               
545200     MOVE 'W41830D6'                      TO POSTSUM-DDNAMN2              
545300     CALL POSTSUM USING POSTSUM-PARM                                      
545400     .                                                                    
545500     EJECT                                                                
545600 S17-SKRIV-W41839 SECTION.                                                
545700*    DISPLAY '*** S17-SKRIV-W41839'                                       
545800                                                                          
545900     MOVE ANM-IDFTG   TO WS-IDFTG                                         
546000                                                                          
546100     IF IDFTG-PV                                                          
546200        WRITE UT39-POST FROM UT39-AREA                                    
546300                                                                          
546400        MOVE UT39-IDPTYP                  TO POSTSUM-TRANSTYP             
546500        MOVE 'W41839'                     TO POSTSUM-FDNAMN               
546600        MOVE 'W41830D7'                   TO POSTSUM-DDNAMN2              
546700        CALL POSTSUM USING POSTSUM-PARM                                   
546800     END-IF                                                               
546900     .                                                                    
547000     EJECT                                                                
547100 S18-SKRIV-W4183A SECTION.                                                
547200*    DISPLAY '*** S18-SKRIV-W4183A'                                       
547300                                                                          
547400     WRITE UT3A-POST FROM RKD-AREA                                        
547500                                                                          
547600     MOVE RKD-IDPTYP                      TO POSTSUM-TRANSTYP             
547700     MOVE 'W4183A'                        TO POSTSUM-FDNAMN               
547800     MOVE 'W41830D8'                      TO POSTSUM-DDNAMN2              
547900     CALL POSTSUM USING POSTSUM-PARM                                      
548000     .                                                                    
548100     EJECT                                                                
548200 S19-SKRIV-W4183B SECTION.                                                
548300*    DISPLAY '*** S19-SKRIV-W4183B '                                      
548400                                                                          
548500     WRITE UT3B-POST FROM RKE-AREA                                        
548600                                                                          
548700     MOVE RKE-IDPTYP                      TO POSTSUM-TRANSTYP             
548800     MOVE 'W4183B'                        TO POSTSUM-FDNAMN               
548900     MOVE 'W41830D9'                      TO POSTSUM-DDNAMN2              
549000     CALL POSTSUM USING POSTSUM-PARM                                      
549100     .                                                                    
549200     EJECT                                                                
549300 S20-SKRIV-W41834 SECTION.                                                
549400*    DISPLAY '*** S20-SKRIV-W41834 '                                      
549500                                                                          
549600     WRITE UT34-POST FROM UT34-AREA                                       
549700                                                                          
549800     MOVE UT34-AREA-IDPTYP                TO POSTSUM-TRANSTYP             
549900     MOVE 'W41834'                        TO POSTSUM-FDNAMN               
550000     MOVE 'W41830DA'                      TO POSTSUM-DDNAMN2              
550100     CALL POSTSUM USING POSTSUM-PARM                                      
550200     .                                                                    
550300     EJECT                                                                
550400 S21-SKRIV-W4183C SECTION.                                                
550500*    DISPLAY '*** S21-SKRIV-W4183C '                                      
550600                                                                          
550700     WRITE UT3C-POST FROM UT3C-AREA                                       
550800                                                                          
550900     MOVE UT3C-IDPTYP                     TO POSTSUM-TRANSTYP             
551000     MOVE 'W4183C'                        TO POSTSUM-FDNAMN               
551100     MOVE 'W41830DB'                      TO POSTSUM-DDNAMN2              
551200     CALL POSTSUM USING POSTSUM-PARM                                      
551300     .                                                                    
551400     EJECT                                                                
551500 S22-SKRIV-W4183D SECTION.                                                
551600*    DISPLAY '*** S22-SKRIV-W4183D '                                      
551700                                                                          
551800     WRITE UT3D-POST FROM UT3D-AREA                                       
551900                                                                          
552000     MOVE UT3D-IDPTYP                     TO POSTSUM-TRANSTYP             
552100     MOVE 'W4183D'                        TO POSTSUM-FDNAMN               
552200     MOVE 'W41830DC'                      TO POSTSUM-DDNAMN2              
552300     CALL POSTSUM USING POSTSUM-PARM                                      
552400     .                                                                    
552500     EJECT                                                                
552600 S23-SKRIV-W4183E SECTION.                                                
552700*    DISPLAY '*** S23-SKRIV-W4183E '                                      
552800                                                                          
552900     WRITE UT3E-POST FROM UT3E-AREA                                       
553000                                                                          
553100     MOVE UT3E-IDPTYP                     TO POSTSUM-TRANSTYP             
553200     MOVE 'W4183E'                        TO POSTSUM-FDNAMN               
553300     MOVE 'W41830DE'                      TO POSTSUM-DDNAMN2              
553400     CALL POSTSUM USING POSTSUM-PARM                                      
553500     .                                                                    
553600     EJECT                                                                
553700 S24-SKRIV-W4183F SECTION.                                                
553800*    DISPLAY '*** S24-SKRIV-W4183F '                                      
553900                                                                          
554000     WRITE UT3F-POST FROM UT3F-AREA                                       
554100                                                                          
554200     MOVE UT3F-IDPTYP                     TO POSTSUM-TRANSTYP             
554300     MOVE 'W4183F'                        TO POSTSUM-FDNAMN               
554400     MOVE 'W41830DF'                      TO POSTSUM-DDNAMN2              
554500     CALL POSTSUM USING POSTSUM-PARM                                      
554600     .                                                                    
554700     EJECT                                                                
554800 S25-SKRIV-W4183G SECTION.                                                
554900*    DISPLAY '*** S25-SKRIV-W4183G '                                      
555000                                                                          
555100     WRITE UT3G-POST FROM UT3G-AREA                                       
555200                                                                          
555300     MOVE UT3G-IDPTYP                     TO POSTSUM-TRANSTYP             
555400     MOVE 'W4183G'                        TO POSTSUM-FDNAMN               
555500     MOVE 'W41830DG'                      TO POSTSUM-DDNAMN2              
555600     CALL POSTSUM USING POSTSUM-PARM                                      
555700     .                                                                    
555800     EJECT                                                                
555900 S26-SKRIV-W4183H SECTION.                                                
556000*    DISPLAY '*** S26-SKRIV-W4183H '                                      
556100                                                                          
556200     WRITE UT3H-POST FROM UT3H-AREA                                       
556300                                                                          
556400     MOVE W-IDPTYP                        TO POSTSUM-TRANSTYP             
556500     MOVE 'W4183H'                        TO POSTSUM-FDNAMN               
556600     MOVE 'W41830DH'                      TO POSTSUM-DDNAMN2              
556700     CALL POSTSUM USING POSTSUM-PARM                                      
556800     .                                                                    
556900     EJECT                                                                
557000 S27-SKRIV-W4183I SECTION.                                                
557100*    DISPLAY '*** S27-SKRIV-W4183I '                                      
557200                                                                          
557300     WRITE UT3I-POST FROM UT3I-AREA                                       
557400                                                                          
557500     MOVE UT3I-IDPTYP                     TO POSTSUM-TRANSTYP             
557600     MOVE 'W4183I'                        TO POSTSUM-FDNAMN               
557700     MOVE 'W41830DI'                      TO POSTSUM-DDNAMN2              
557800     CALL POSTSUM USING POSTSUM-PARM                                      
557900     .                                                                    
558000     EJECT                                                                
558100 S28-SKRIV-W4183J SECTION.                                                
558200*    DISPLAY '*** S28-SKRIV-W4183J '                                      
558300                                                                          
558400     WRITE UT3J-POST FROM UT3J-AREA                                       
558500                                                                          
558600     MOVE UT3J-IDPTYP                     TO POSTSUM-TRANSTYP             
558700     MOVE 'W4183J'                        TO POSTSUM-FDNAMN               
558800     MOVE 'W41830DJ'                      TO POSTSUM-DDNAMN2              
558900     CALL POSTSUM USING POSTSUM-PARM                                      
559000     .                                                                    
559100     EJECT                                                                
559200 S29-SKRIV-W4183K SECTION.                                                
559300*    DISPLAY '*** S29-SKRIV-W4183K '                                      
559400                                                                          
559500     WRITE UT3K-POST FROM UT3K-AREA                                       
559600                                                                          
559700     MOVE UT3K-IDPTYP                     TO POSTSUM-TRANSTYP             
559800     MOVE 'W4183K'                        TO POSTSUM-FDNAMN               
559900     MOVE 'W41830DK'                      TO POSTSUM-DDNAMN2              
560000     CALL POSTSUM USING POSTSUM-PARM                                      
560100     .                                                                    
560200     EJECT                                                                
560300 S31-SKRIV-W418AG SECTION.                                                
560400*    DISPLAY '*** S31-SKRIV-W418AG '                                      
560500                                                                          
560600     WRITE UTAG-POST FROM UTAG-AREA                                       
560700                                                                          
560800     MOVE UTAG-IDPTYP                     TO POSTSUM-TRANSTYP             
560900     MOVE 'W418AG'                        TO POSTSUM-FDNAMN               
561000     MOVE 'W41830DD'                      TO POSTSUM-DDNAMN2              
561100     CALL POSTSUM USING POSTSUM-PARM                                      
561200     .                                                                    
561300     EJECT                                                                
561400 S32-SKRIV-W418AI SECTION.                                                
561500*    DISPLAY '*** S32-SKRIV-W418AI '                                      
561600                                                                          
561700     EVALUATE UTAI-IDPTYP                                                 
561800       WHEN '31A'                                                         
561900         WRITE 31A-POST FROM UTAI-AREA                                    
562000       WHEN '31B'                                                         
562100         WRITE 31B-POST FROM UTAI-AREA                                    
562200      END-EVALUATE                                                        
562300                                                                          
562400     MOVE UTAI-IDPTYP                     TO POSTSUM-TRANSTYP             
562500     MOVE 'W418AI'                        TO POSTSUM-FDNAMN               
562600     MOVE 'W41830DL'                      TO POSTSUM-DDNAMN2              
562700     CALL POSTSUM USING POSTSUM-PARM                                      
562800     .                                                                    
562900     EJECT                                                                
563000 S33-SKRIV-W4183N SECTION.                                                
563100*    DISPLAY '*** S33-SKRIV-W4183N '                                      
563200                                                                          
563300     WRITE UT3N-POST FROM UT3N-AREA                                       
563400                                                                          
563500     MOVE UT3N-IDPTYP                     TO POSTSUM-TRANSTYP             
563600     MOVE 'W4183N'                        TO POSTSUM-FDNAMN               
563700     MOVE 'W41830DN'                      TO POSTSUM-DDNAMN2              
563800     CALL POSTSUM USING POSTSUM-PARM                                      
563900     .                                                                    
564000     EJECT                                                                
564100 S34-SKRIV-W4183O SECTION.                                                
564200*    DISPLAY '*** S34-SKRIV-W4183O '                                      
564300                                                                          
564400     WRITE UT3O-POST FROM UT3O-AREA                                       
564500                                                                          
564600     MOVE UT3O-IDPTYP                     TO POSTSUM-TRANSTYP             
564700     MOVE 'W4183O'                        TO POSTSUM-FDNAMN               
564800     MOVE 'W41830DO'                      TO POSTSUM-DDNAMN2              
564900     CALL POSTSUM USING POSTSUM-PARM                                      
565000     .                                                                    
565100     EJECT                                                                
565200 S35-SKAPA-HUVUD-KNOTA SECTION.                                           
565300*    DISPLAY '**** S35-SKAPA-KNOTNR'                                      
565400                                                                          
565500     IF TKOST-SKRIVNA-FOERUT                                              
565600        MOVE JA                           TO TILLAEGGSKOSTNADER-SW        
565700     END-IF                                                               
565800                                                                          
565900     MOVE '712'                           TO 712-IDPTYP                   
566000     MOVE ANM-IDDISTR                     TO 712-IDDISTR                  
566100     MOVE ANM-IDKUNDNR                    TO 712-IDKUNDNR                 
566200     MOVE LEV-IDDC                        TO 712-IDDC                     
566300     EVALUATE TRUE                                                        
566400        WHEN CDC-SE                                                       
566500           MOVE W-IDKNOTNR-CDC            TO 712-IDKNOTNR                 
566600        WHEN SDC-NL                                                       
566700        WHEN SDC-NL-ET                                                    
566800        WHEN SDC-ES                                                       
566900        WHEN SDC-AT                                                       
567000           MOVE W-IDKNOTNR-SDC            TO 712-IDKNOTNR                 
567100        WHEN SDC-IT                                                       
567200           IF W-IDKNOTNR-ITL > +0                                         
567300              MOVE W-IDKNOTNR-ITL         TO 712-IDKNOTNR                 
567400           ELSE                                                           
567500              MOVE W-IDKNOTNR-SDC         TO 712-IDKNOTNR                 
567600           END-IF                                                         
567700        WHEN NDC-US                                                       
567800           MOVE W-IDKNOTNR-USA            TO 712-IDKNOTNR                 
567900        WHEN NDC-CA                                                       
568000           MOVE W-IDKNOTNR-CAN            TO 712-IDKNOTNR                 
568100        WHEN NDC-JP                                                       
568200           MOVE W-IDKNOTNR-JAP            TO 712-IDKNOTNR                 
568300        WHEN NDC-AU                                                       
568400           MOVE W-IDKNOTNR-AUS            TO 712-IDKNOTNR                 
568500        WHEN DDC-SE                                                       
568600           MOVE W-IDKNOTNR-SE             TO 712-IDKNOTNR                 
568700        WHEN DDC-NO                                                       
568800           MOVE W-IDKNOTNR-NO             TO 712-IDKNOTNR                 
568900        WHEN DDC-BE                                                       
569000           MOVE W-IDKNOTNR-BE             TO 712-IDKNOTNR                 
569100     END-EVALUATE                                                         
569200     MOVE ANM-IDRAPPNR                    TO 712-IDRAPPNR                 
569300     MOVE ANM-DALEVANM (3:6)              TO 712-TILEVANM                 
569400     MOVE ANM-DARETILL (3:6)              TO 712-TIRETILL                 
569500     MOVE DAGENS-DATUM                    TO 712-TIKNOTA                  
569600     MOVE ANM-IDUSER-ADM                  TO 712-IDUSER-ADM               
569700     MOVE ANM-BEANST                      TO 712-BEANST                   
569800     MOVE ANM-RELANDCO                    TO 712-RELANDCO                 
569900     MOVE +0                              TO 712-REEMBHNT                 
570000     IF TILLAEGGSK-SKRIVNA                                                
570100        MOVE +0                           TO 712-PRFOERS                  
570200                                             712-PRFRAKT                  
570300                                             712-PRLEGKST                 
570400     ELSE                                                                 
570500        MOVE ANM-PRFOERS                  TO 712-PRFOERS                  
570600        MOVE ANM-PRFRAKT                  TO 712-PRFRAKT                  
570700        MOVE ANM-PRLEGKST                 TO 712-PRLEGKST                 
570800        MOVE JA                           TO TILLAEGGSKOSTNADER-SW        
570900     END-IF                                                               
571000                                                                          
571100*-- ATTESTANSVARIGA SKALL SKRIVAS UT PÅ KNOTAN.SOX-KRAV 050830            
571200     MOVE +1           TO GODK-IX                                         
571300     PERFORM UNTIL GODK-IX > MAX-GODK-IX                                  
571400       MOVE SPACE                 TO 712-ATTESTANSVARIGA(GODK-IX)         
571500       MOVE ZERO                  TO 712-TIUPPDAT(GODK-IX)                
571600       ADD +1          TO GODK-IX                                         
571700     END-PERFORM                                                          
571800                                                                          
571900     MOVE ANM-IDDISTR      TO W-IDDISTR-4103                              
572000     MOVE ANM-IDKUNDNR     TO W-IDKUNDNR-4103                             
572100     MOVE ANM-IDRAPPNR     TO W-IDRAPPNR-4103                             
572200                                                                          
572300     PERFORM IMS-GU-WDGX4103                                              
572400     IF SEGMENT-FINNS                                                     
572500       MOVE LEV-IDDC      TO W-IDDC-4104                                  
572600                                                                          
572700       IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                              
572800            LEV-KDANMORS = '97'                                           
572900         MOVE 'RP'     TO W-KDKRENOT-4104                                 
573000       ELSE                                                               
573100         MOVE 'CN'     TO W-KDKRENOT-4104                                 
573200       END-IF                                                             
573300                                                                          
573400       PERFORM IMS-GNP-WDGX4104                                           
573500       IF SEGMENT-FINNS                                                   
573600         MOVE +1           TO GODK-IX                                     
573700         PERFORM IMS-GNP-WDGX4106                                         
573800         PERFORM UNTIL SEGMENT-SAKNAS OR GODK-IX > MAX-GODK-IX            
573900           MOVE 4106-IDUSER-GODK   TO 712-IDUSER-GODK(GODK-IX)            
574000           MOVE 4106-BEANST-GODK   TO 712-BEANST-GODK(GODK-IX)            
574100           MOVE 4106-TIUPPDAT      TO 712-TIUPPDAT   (GODK-IX)            
574200           ADD +1            TO GODK-IX                                   
574300           PERFORM IMS-GNP-WDGX4106                                       
574400         END-PERFORM                                                      
574500       END-IF                                                             
574600     END-IF                                                               
574700                                                                          
574800     IF CDC-SE                                                            
574900        MOVE NEJ                        TO SKRIV-HUV-KNOTA-CDC-SW         
575000     ELSE                                                                 
575100       EVALUATE TRUE                                                      
575200         WHEN DDC-SE                                                      
575300           MOVE NEJ                     TO SKRIV-HUV-KNOTA-SE-SW          
575400         WHEN DDC-NO                                                      
575500           MOVE NEJ                     TO SKRIV-HUV-KNOTA-NO-SW          
575600         WHEN DDC-BE                                                      
575700           MOVE NEJ                     TO SKRIV-HUV-KNOTA-BE-SW          
575800         WHEN OTHER                                                       
575900           MOVE NEJ                     TO SKRIV-HUV-KNOTA-SDC-SW         
576000       END-EVALUATE                                                       
576100     END-IF                                                               
576200                                                                          
576300     PERFORM S14-SKRIV-W41835                                             
576400     .                                                                    
576500     EJECT                                                                
576600 S36-SKAPA-RADPOST-KNOTA SECTION.                                         
576700*    DISPLAY '*** S36-SKAPA-RADPOST-KNOTA '                               
576800                                                                          
576900     MOVE '713'                           TO 713-IDPTYP                   
577000     MOVE ANM-IDDISTR                     TO 713-IDDISTR                  
577100                                             TEST-IDDISTR                 
577200     MOVE ANM-IDKUNDNR                    TO 713-IDKUNDNR                 
577300     MOVE LEV-IDDC                        TO 713-IDDC                     
577400     EVALUATE TRUE                                                        
577500        WHEN CDC-SE                                                       
577600           MOVE W-IDKNOTNR-CDC            TO 713-IDKNOTNR                 
577700        WHEN SDC-NL                                                       
577800        WHEN SDC-NL-ET                                                    
577900        WHEN SDC-ES                                                       
578000        WHEN SDC-AT                                                       
578100           MOVE W-IDKNOTNR-SDC            TO 713-IDKNOTNR                 
578200        WHEN SDC-IT                                                       
578300           IF W-IDKNOTNR-ITL > +0                                         
578400              MOVE W-IDKNOTNR-ITL         TO 713-IDKNOTNR                 
578500           ELSE                                                           
578600              MOVE W-IDKNOTNR-SDC         TO 713-IDKNOTNR                 
578700           END-IF                                                         
578800        WHEN NDC-US                                                       
578900           MOVE W-IDKNOTNR-USA            TO 713-IDKNOTNR                 
579000        WHEN NDC-CA                                                       
579100           MOVE W-IDKNOTNR-CAN            TO 713-IDKNOTNR                 
579200        WHEN NDC-JP                                                       
579300           MOVE W-IDKNOTNR-JAP            TO 713-IDKNOTNR                 
579400        WHEN NDC-AU                                                       
579500           MOVE W-IDKNOTNR-AUS            TO 713-IDKNOTNR                 
579600        WHEN DDC-SE                                                       
579700           MOVE W-IDKNOTNR-SE             TO 713-IDKNOTNR                 
579800                                             VIR-IDKNOTNR                 
579900        WHEN DDC-NO                                                       
580000           MOVE W-IDKNOTNR-NO             TO 713-IDKNOTNR                 
580100                                             VIR-IDKNOTNR                 
580200        WHEN DDC-BE                                                       
580300           MOVE W-IDKNOTNR-BE             TO 713-IDKNOTNR                 
580400                                             VIR-IDKNOTNR                 
580500     END-EVALUATE                                                         
580600     MOVE LEV-IDARTNR                     TO 713-IDARTNR                  
580700     MOVE LEV-KDFAKTYP                    TO 713-KDFAKTYP                 
580800     MOVE LEV-IDFAKT                      TO 713-IDFAKT                   
580900     MOVE LEV-TIFAKT                      TO 713-TIFAKT                   
581000     MOVE LEV-KVLEVANM-BEKR               TO 713-KVLEVANM                 
581100     MOVE LEV-KDANMORS                    TO 713-KDANMORS                 
581200     COMPUTE 713-KVKREANT = LEV-KVLEVANM-BEKR -                           
581300                            LEV-KVAVV-KVANT   -                           
581400                            LEV-KVAVV-KVAL                                
581500     END-COMPUTE                                                          
581600     MOVE 713-KVKREANT                    TO VIR-KVLEVANM                 
581700     MOVE LEV-PRARTBTO                    TO 713-PRARTBTO                 
581800     MOVE WS-FLLSBOK                      TO 713-FLLSBOK                  
581900     MOVE LEV-IDANALYS                    TO 713-IDANALYS                 
582000     MOVE LEV-IDKONTO                     TO 713-IDKONTO                  
582100     MOVE LEV-IDKST                       TO 713-IDKST                    
582200                                                                          
582300     PERFORM S14-SKRIV-W41835                                             
582400                                                                          
582500     PERFORM S36A-SKAPA-RADPOST-ARTSTAT                                   
582600                                                                          
582700     IF LEV-KDANMORS = '40' OR '74'                                       
582800        CONTINUE                                                          
582900     ELSE                                                                 
583000        PERFORM S36B-SKAPA-RADPOST-VIPS                                   
583100     END-IF                                                               
583200                                                                          
583300     IF GOOD-DDC                                                          
583400       IF OKOD-FL-LEVERANTOER = JA                                        
583500          PERFORM S42-SKAPA-RADER-VIR                                     
583600       END-IF                                                             
583700     END-IF                                                               
583800                                                                          
583900     IF BYT16-BYTES OR BYT16-RADIO                                        
584000       IF LEV-KDANMORS = '30' OR '40'                                     
584100          CONTINUE                                                        
584200       ELSE                                                               
584300          IF SKRIV-BYTES-OK                                               
584400             CONTINUE                                                     
584500          ELSE                                                            
584600             PERFORM S36C-SKAPA-RADPOST-BYTES                             
584700          END-IF                                                          
584800       END-IF                                                             
584900     END-IF                                                               
585000     .                                                                    
585100     EJECT                                                                
585200 S36A-SKAPA-RADPOST-ARTSTAT SECTION.                                      
585300*    DISPLAY '*** S36A-SKAPA-RADPOST-ARTSTAT'                             
585400                                                                          
585500     IF DIST79-DEALER-PRICE OR                                            
585600        DIST79-ECOM-PRICE                                                 
585700       IF LEV-PRARTBTO-LOC > +0                                           
585800         MOVE ' 99'                         TO UT39-IDPTYP                
585900         MOVE LEV-IDARTNR                   TO UT39-IDARTNR               
586000         MOVE ANM-IDDISTR                   TO UT39-IDDISTR               
586100                                                                          
586200         COMPUTE WS-KVLEVART = LEV-KVLEVANM-BEKR -                        
586300                               LEV-KVAVV-KVANT   -                        
586400                               LEV-KVAVV-KVAL                             
586500         END-COMPUTE                                                      
586600         IF LEV-KDANMORS = '30'                                           
586700           MOVE ZERO                        TO UT39-KVLEVART              
586800         ELSE                                                             
586900           MOVE WS-KVLEVART                 TO UT39-KVLEVART              
587000         END-IF                                                           
587100                                                                          
587200         MOVE DAGENS-DATUM                  TO UT39-TIFAKT                
587300                                                                          
587400*- PRISET SKALL VARA I SEK.MAN HÄMTAR KURSEN OCH RÄKNAR OM.               
587500         MOVE ANM-KDVALISO                  TO CURR-KDVALISO-ROW          
587600**** LÄS PRKURS HTYP 9305               *****                             
587700         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
587800         IF CURR-KDSVAR = ' '                                             
587900            CONTINUE                                                      
588000         ELSE                                                             
588100            MOVE 1                          TO CURR-PRKURS-NEW            
588200         END-IF                                                           
588300                                                                          
588400         COMPUTE UT39-PRARTNTO ROUNDED =                                  
588500            (LEV-PRARTBTO-LOC  * CURR-PRKURS-NEW) * WS-KVLEVART           
588600         END-COMPUTE                                                      
588700                                                                          
588800         MOVE 'K'                           TO UT39-KDPRTYP               
588900         MOVE  4                            TO UT39-KDORDKL               
589000                                                                          
589100         PERFORM S17-SKRIV-W41839                                         
589200       END-IF                                                             
589300     ELSE                                                                 
589400       IF LEV-PRARTBTO > +0                                               
589500         MOVE ' 99'                         TO UT39-IDPTYP                
589600         MOVE LEV-IDARTNR                   TO UT39-IDARTNR               
589700         MOVE ANM-IDDISTR                   TO UT39-IDDISTR               
589800                                                                          
589900*- ÄNDRING BEGÄRD AV JANNE P 990512, ART.STAT VILL EJ HA KVANT            
590000*  VID PRISFEL.                                                           
590100                                                                          
590200         COMPUTE WS-KVLEVART = LEV-KVLEVANM-BEKR -                        
590300                               LEV-KVAVV-KVANT   -                        
590400                               LEV-KVAVV-KVAL                             
590500         END-COMPUTE                                                      
590600         IF LEV-KDANMORS = '30'                                           
590700           MOVE ZERO                        TO UT39-KVLEVART              
590800         ELSE                                                             
590900           MOVE WS-KVLEVART                 TO UT39-KVLEVART              
591000         END-IF                                                           
591100                                                                          
591200         MOVE DAGENS-DATUM                  TO UT39-TIFAKT                
591300         COMPUTE UT39-PRARTNTO ROUNDED =                                  
591400                               LEV-PRARTBTO * WS-KVLEVART                 
591500         MOVE 'K'                           TO UT39-KDPRTYP               
591600         MOVE  4                            TO UT39-KDORDKL               
591700                                                                          
591800         PERFORM S17-SKRIV-W41839                                         
591900       END-IF                                                             
592000     END-IF                                                               
592100     .                                                                    
592200     EJECT                                                                
592300 S36B-SKAPA-RADPOST-VIPS SECTION.                                         
592400*    DISPLAY '*** S36B-SKAPA-RADPOST-VIPS'                                
592500                                                                          
592600     IF NOT LEV-KDANMORS = '74'                                           
592700                                                                          
592800     MOVE '021'                           TO UT37-KRED-IDPTYP             
592900     MOVE ANM-IDDISTR                     TO UT37-KRED-IDDISTR            
593000                                             TEST-IDDISTR                 
593100     MOVE ANM-IDKUNDNR                    TO UT37-KRED-IDKUNDNR           
593200     MOVE LEV-IDDC                        TO UT37-KRED-IDDC               
593300                                             MOMS-WS-IDDC                 
593400     MOVE ANM-IDRAPPNR                    TO UT37-KRED-IDRAPPNR           
593500     EVALUATE TRUE                                                        
593600        WHEN CDC-SE                                                       
593700           MOVE W-IDKNOTNR-CDC            TO UT37-KRED-IDKNOTNR           
593800        WHEN SDC-NL                                                       
593900        WHEN SDC-NL-ET                                                    
594000        WHEN SDC-ES                                                       
594100        WHEN SDC-AT                                                       
594200           MOVE W-IDKNOTNR-SDC            TO UT37-KRED-IDKNOTNR           
594300        WHEN SDC-IT                                                       
594400           IF W-IDKNOTNR-ITL > +0                                         
594500              MOVE W-IDKNOTNR-ITL         TO UT37-KRED-IDKNOTNR           
594600           ELSE                                                           
594700              MOVE W-IDKNOTNR-SDC         TO UT37-KRED-IDKNOTNR           
594800           END-IF                                                         
594900        WHEN NDC-US                                                       
595000           MOVE W-IDKNOTNR-USA            TO UT37-KRED-IDKNOTNR           
595100        WHEN NDC-CA                                                       
595200           MOVE W-IDKNOTNR-CAN            TO UT37-KRED-IDKNOTNR           
595300        WHEN NDC-JP                                                       
595400           MOVE W-IDKNOTNR-JAP            TO UT37-KRED-IDKNOTNR           
595500        WHEN NDC-AU                                                       
595600           MOVE W-IDKNOTNR-AUS            TO UT37-KRED-IDKNOTNR           
595700        WHEN DDC-SE                                                       
595800           MOVE W-IDKNOTNR-SE             TO UT37-KRED-IDKNOTNR           
595900        WHEN DDC-NO                                                       
596000           MOVE W-IDKNOTNR-NO             TO UT37-KRED-IDKNOTNR           
596100        WHEN DDC-BE                                                       
596200           MOVE W-IDKNOTNR-BE             TO UT37-KRED-IDKNOTNR           
596300     END-EVALUATE                                                         
596400     MOVE DAGENS-DATUM                    TO UT37-KRED-TIM-KN             
596500     MOVE LEV-IDFAKT                      TO UT37-KRED-IDFAKT             
596600     MOVE LEV-IDORDNR7                    TO UT37-KRED-IDORDNR            
596700     MOVE LEV-IDARTNR                     TO UT37-KRED-IDARTNR            
596800     MOVE ZERO                            TO UT37-KRED-REKSIFFR           
596900     MOVE LEV-IDRADNR                     TO UT37-KRED-IDRADNR            
597000     MOVE LEV-KDANMORS                    TO UT37-KRED-KDANMORS           
597100                                                                          
597200*- KOD 25 GÄLLER BARA INOM PULS, BYTS TILLBAKA I VIPS.                    
597300     IF UT37-KRED-KDANMORS = 25                                           
597400       MOVE 20                            TO UT37-KRED-KDANMORS           
597500     END-IF                                                               
597600                                                                          
597700     MOVE CLAG-KDPSLLOC                   TO UT37-KRED-KDPSLLOC           
597800     COMPUTE UT37-KRED-KVKREANT = LEV-KVLEVANM-BEKR -                     
597900                                  LEV-KVAVV-KVANT   -                     
598000                                  LEV-KVAVV-KVAL                          
598100     END-COMPUTE                                                          
598200                                                                          
598300     IF DIST79-DEALER-PRICE OR                                            
598400        DIST79-ECOM-PRICE                                                 
598500       MOVE ZERO                        TO UT37-KRED-PRARTBTO             
598600       MOVE LEV-PRARTBTO-LOC            TO UT37-KRED-PRARTBTO-LOC         
598700     ELSE                                                                 
598800       MOVE LEV-PRARTBTO                TO UT37-KRED-PRARTBTO             
598900       MOVE ZERO                        TO UT37-KRED-PRARTBTO-LOC         
599000     END-IF                                                               
599100                                                                          
599200     IF OKOD-FL-KOD-SOM-BAER-TK  = JA OR                                  
599300        OKOD-FL-INTERNUPPACKNING = JA                                     
599400                                                                          
599500       IF DIST79-DEALER-PRICE OR                                          
599600          DIST79-ECOM-PRICE                                               
599700         COMPUTE UT37-KRED-PREMBHNT ROUNDED =                             
599800         (UT37-KRED-KVKREANT * LEV-PRARTBTO-LOC *                         
599900                                     ANM-RELANDCO) / 100                  
600000       ELSE                                                               
600100         COMPUTE UT37-KRED-PREMBHNT ROUNDED =                             
600200         (UT37-KRED-KVKREANT * LEV-PRARTBTO * ANM-RELANDCO) / 100         
600300       END-IF                                                             
600400     ELSE                                                                 
600500        MOVE ZERO                         TO UT37-KRED-PREMBHNT           
600600     END-IF                                                               
600700                                                                          
600800     PERFORM S36BA-HAEMTA-BYTES-UPPG                                      
600900     MOVE ANM-PRFRAKT                     TO UT37-KRED-PRFRAKT            
601000     MOVE ANM-PRLEGKST                    TO UT37-KRED-PRLEGKST           
601100     MOVE ANM-PRFOERS                     TO UT37-KRED-PRFOERS            
601200     MOVE ZERO                            TO UT37-KRED-PRMOMS             
601300     MOVE +0                              TO W009-MOMS-REVAT              
601400                                                                          
601500     PERFORM S70-SAETT-EV-MOMS                                            
601600     IF DIST79-DEALER-PRICE OR                                            
601700        DIST79-ECOM-PRICE                                                 
601800       COMPUTE UT37-KRED-PRMOMS ROUNDED =                                 
601900          UT37-KRED-KVKREANT * LEV-PRARTBTO-LOC * W009-MOMS-REVAT         
602000     ELSE                                                                 
602100       COMPUTE UT37-KRED-PRMOMS ROUNDED =                                 
602200          UT37-KRED-KVKREANT *  LEV-PRARTBTO * W009-MOMS-REVAT            
602300     END-IF                                                               
602400                                                                          
602500*- DESSA FÄLT ÄR AVSEDDA FÖR BILLIT KREDITNOTOR / DNI.                    
602600     IF DIST79-DEALER-PRICE OR                                            
602700        DIST79-ECOM-PRICE                                                 
602800       MOVE ZERO                        TO UT37-KRED-SUVAT-FAKT           
602900       MOVE ANM-KDVALISO                TO UT37-KRED-KDVALISO             
603000       MOVE LEV-KDVAT                   TO UT37-KRED-KDVAT                
603100       MOVE LEV-PRARTSTD                TO UT37-KRED-PRARTSTD             
603200       MOVE LEV-PRARTSJK                TO UT37-KRED-PRARTSJK             
603300                                                                          
603400       COMPUTE UT37-KRED-SULNELOC = LEV-PRARTBTO-LOC  *                   
603500                              UT37-KRED-KVKREANT                          
603600       MOVE ZERO                        TO UT37-KRED-SUKRENTO             
603700       MOVE ZERO                        TO UT37-KRED-SUKRETOT             
603800     ELSE                                                                 
603900       MOVE ZERO                        TO UT37-KRED-SUVAT-FAKT           
604000       MOVE ANM-KDVALISO                TO UT37-KRED-KDVALISO             
604100       MOVE SPACE                       TO UT37-KRED-KDVAT                
604200       MOVE ZERO                        TO UT37-KRED-PRARTSTD             
604300       MOVE ZERO                        TO UT37-KRED-PRARTSJK             
604400       MOVE ZERO                        TO UT37-KRED-SULNELOC             
604500       MOVE ZERO                        TO UT37-KRED-SUKRENTO             
604600       MOVE ZERO                        TO UT37-KRED-SUKRETOT             
604700     END-IF                                                               
604800                                                                          
604900     PERFORM S16-SKRIV-W41837                                             
605000                                                                          
605100     END-IF                                                               
605200     .                                                                    
605300     EJECT                                                                
605400 S36BA-HAEMTA-BYTES-UPPG SECTION.                                         
605500*    DISPLAY '*** S36BA-HAEMTA-BYTES-UPPG'                                
605600                                                                          
605700     IF DIST07-USA-RETAILER AND CDC-SE                                    
605800        MOVE WC-NDC-US-RU                 TO W-IDDC                       
605900     ELSE                                                                 
606000        IF DIST07-CAN-RETAILER AND CDC-SE                                 
606100           MOVE WC-NDC-CA                 TO W-IDDC                       
606200        ELSE                                                              
606300           MOVE LEV-IDDC                  TO W-IDDC                       
606400        END-IF                                                            
606500     END-IF                                                               
606600     PERFORM IMS-GU-WDK711                                                
606700     IF BYT16-BYTES                                                       
606800       IF SEGMENT-FINNS                                                   
606900         MOVE SLAG-PRAVCOST               TO UT37-KRED-PRAVCOST           
607000         ADD 6000                         TO W-IDARTNR-K7                 
607100         PERFORM IMS-GU-WDK711                                            
607200         IF SEGMENT-FINNS                                                 
607300            MOVE SLAG-PRAVCOST            TO                              
607400                                           UT37-KRED-PRAVCOST-CORE        
607500         ELSE                                                             
607600            MOVE +0                       TO                              
607700                                           UT37-KRED-PRAVCOST-CORE        
607800         END-IF                                                           
607900       ELSE                                                               
608000         MOVE +0                          TO UT37-KRED-PRAVCOST           
608100                                           UT37-KRED-PRAVCOST-CORE        
608200       END-IF                                                             
608300     ELSE                                                                 
608400        IF BYT16-RADIO                                                    
608500          IF SEGMENT-FINNS                                                
608600             MOVE SLAG-PRAVCOST           TO UT37-KRED-PRAVCOST           
608700             ADD 1000                     TO W-IDARTNR-K7                 
608800             PERFORM IMS-GU-WDK711                                        
608900             IF SEGMENT-FINNS                                             
609000                MOVE SLAG-PRAVCOST        TO                              
609100                                           UT37-KRED-PRAVCOST-CORE        
609200             ELSE                                                         
609300                MOVE +0                   TO                              
609400                                           UT37-KRED-PRAVCOST-CORE        
609500             END-IF                                                       
609600          ELSE                                                            
609700             MOVE +0                      TO UT37-KRED-PRAVCOST           
609800                                           UT37-KRED-PRAVCOST-CORE        
609900          END-IF                                                          
610000        ELSE                                                              
610100          IF SEGMENT-FINNS                                                
610200            MOVE SLAG-PRAVCOST            TO UT37-KRED-PRAVCOST           
610300            MOVE +0                       TO                              
610400                                           UT37-KRED-PRAVCOST-CORE        
610500          ELSE                                                            
610600            MOVE +0                       TO UT37-KRED-PRAVCOST           
610700                                           UT37-KRED-PRAVCOST-CORE        
610800          END-IF                                                          
610900        END-IF                                                            
611000     END-IF                                                               
611100     .                                                                    
611200     EJECT                                                                
611300 S36C-SKAPA-RADPOST-BYTES SECTION.                                        
611400*    DISPLAY '*** S36C-SKAPA-RADPOST-BYTES'                               
611500                                                                          
611600     MOVE 'KRE'                           TO UTAG-IDPTYP                  
611700     MOVE ANM-IDDISTR                     TO UTAG-IDDISTR                 
611800     MOVE ANM-IDKUNDNR                    TO UTAG-IDKUNDNR                
611900     MOVE LEV-IDARTNR                     TO UTAG-IDARTNR                 
612000     MOVE LEV-IDDC                        TO UTAG-IDDC                    
612100     MOVE LEV-IDORDNR7                    TO UTAG-IDORDNR                 
612200     IF SKRIV-BYTES-OK                                                    
612300        COMPUTE UTAG-KVLEVART = LEV-KVLEVANM-BEKR -                       
612400                                LEV-KVAVV-KVANT                           
612500        END-COMPUTE                                                       
612600     ELSE                                                                 
612700        COMPUTE UTAG-KVLEVART = LEV-KVLEVANM-BEKR -                       
612800                                LEV-KVAVV-KVANT   -                       
612900                                LEV-KVAVV-KVAL                            
613000        END-COMPUTE                                                       
613100     END-IF                                                               
613200     MOVE SPACE                           TO UTAG-FLINVEST                
613300                                                                          
613400     PERFORM S31-SKRIV-W418AG                                             
613500     .                                                                    
613600     EJECT                                                                
613700 S37-SKRIV-W4183P SECTION.                                                
613800*    DISPLAY '*** S37-SKRIV-W4183P '                                      
613900                                                                          
614000     WRITE UT3P-POST FROM UT3P-AREA                                       
614100                                                                          
614200     MOVE UT3P-IDPTYP                     TO POSTSUM-TRANSTYP             
614300     MOVE 'W4183P'                        TO POSTSUM-FDNAMN               
614400     MOVE 'W41830DP'                      TO POSTSUM-DDNAMN2              
614500     CALL POSTSUM USING POSTSUM-PARM                                      
614600     .                                                                    
614700     EJECT                                                                
614800 S38-SKRIV-W4183Q SECTION.                                                
614900*    DISPLAY '*** S38-SKRIV-W4183Q '                                      
615000                                                                          
615100     WRITE UT3Q-POST FROM UT3Q-AREA                                       
615200                                                                          
615300     MOVE UT3Q-IDPTYP                     TO POSTSUM-TRANSTYP             
615400     MOVE 'W4183Q'                        TO POSTSUM-FDNAMN               
615500     MOVE 'W41830DQ'                      TO POSTSUM-DDNAMN2              
615600     CALL POSTSUM USING POSTSUM-PARM                                      
615700     .                                                                    
615800     EJECT                                                                
615900 S39-SKRIV-W4183R SECTION.                                                
616000*    DISPLAY '*** S39-SKRIV-W4183R '                                      
616100                                                                          
616200     WRITE UT3R-POST FROM UT3R-AREA                                       
616300                                                                          
616400     MOVE UT3R-IDPTYP                     TO POSTSUM-TRANSTYP             
616500     MOVE 'W4183R'                        TO POSTSUM-FDNAMN               
616600     MOVE 'W41830DR'                      TO POSTSUM-DDNAMN2              
616700     CALL POSTSUM USING POSTSUM-PARM                                      
616800     .                                                                    
616900     EJECT                                                                
617000 S391-SKRIV-W4183T SECTION.                                               
617100*    DISPLAY '*** S391-SKRIV-W4183T '                                     
617200                                                                          
617300     WRITE UT3T-POST FROM UT3T-AREA                                       
617400                                                                          
617500     MOVE UT3T-IDPTYP                     TO POSTSUM-TRANSTYP             
617600     MOVE 'W4183T'                        TO POSTSUM-FDNAMN               
617700     MOVE 'W41830DT'                      TO POSTSUM-DDNAMN2              
617800     CALL POSTSUM USING POSTSUM-PARM                                      
617900     .                                                                    
618000     EJECT                                                                
618100 S392-SKRIV-W4183U SECTION.                                               
618200*    DISPLAY '*** S392-SKRIV-W4183U '                                     
618300                                                                          
618400     WRITE UT3U-POST FROM UT3U-AREA                                       
618500                                                                          
618600     MOVE UT3U-IDPTYP                     TO POSTSUM-TRANSTYP             
618700     MOVE 'W4183U'                        TO POSTSUM-FDNAMN               
618800     MOVE 'W41830DU'                      TO POSTSUM-DDNAMN2              
618900     CALL POSTSUM USING POSTSUM-PARM                                      
619000     .                                                                    
619100     EJECT                                                                
619200 S393-SKRIV-W4183V SECTION.                                               
619300*    DISPLAY '*** S393-SKRIV-W4183V '                                     
619400                                                                          
619500     WRITE UT3V-POST FROM UT3V-AREA                                       
619600                                                                          
619700     MOVE UT3V-IDPTYP                     TO POSTSUM-TRANSTYP             
619800     MOVE 'W4183V'                        TO POSTSUM-FDNAMN               
619900     MOVE 'W41830DV'                      TO POSTSUM-DDNAMN2              
620000     CALL POSTSUM USING POSTSUM-PARM                                      
620100     .                                                                    
620200     EJECT                                                                
620300 S394-SKRIV-W4183X SECTION.                                               
620400*    DISPLAY '*** S394-SKRIV-W4183X '                                     
620500                                                                          
620600     WRITE UT3X-POST FROM UT3X-AREA                                       
620700                                                                          
620800     MOVE UT3X-IDPTYP                     TO POSTSUM-TRANSTYP             
620900     MOVE 'W4183X'                        TO POSTSUM-FDNAMN               
621000     MOVE 'W41830DX'                      TO POSTSUM-DDNAMN2              
621100     CALL POSTSUM USING POSTSUM-PARM                                      
621200     .                                                                    
621300     EJECT                                                                
621400 S395-SKRIV-W4183W SECTION.                                               
621500*    DISPLAY '*** S395-SKRIV-W4183W '                                     
621600                                                                          
621700     WRITE UT3W-POST FROM UT3W-AREA                                       
621800                                                                          
621900     MOVE UT3W-IDPTYP                     TO POSTSUM-TRANSTYP             
622000     MOVE 'W4183W'                        TO POSTSUM-FDNAMN               
622100     MOVE 'W41830DW'                      TO POSTSUM-DDNAMN2              
622200     CALL POSTSUM USING POSTSUM-PARM                                      
622300     .                                                                    
622400     EJECT                                                                
622500 S396-SKRIV-W4183Y SECTION.                                               
622600*    DISPLAY '*** S396-SKRIV-W4183Y '                                     
622700                                                                          
622800     WRITE UT3Y-POST FROM UT3Y-AREA                                       
622900                                                                          
623000     MOVE UT3Y-IDPTYP                     TO POSTSUM-TRANSTYP             
623100     MOVE 'W4183Y'                        TO POSTSUM-FDNAMN               
623200     MOVE 'W41830DY'                      TO POSTSUM-DDNAMN2              
623300     CALL POSTSUM USING POSTSUM-PARM                                      
623400     .                                                                    
623500     EJECT                                                                
623600 S397-SKRIV-W4183Z SECTION.                                               
623700*    DISPLAY '*** S397-SKRIV-W4183Z '                                     
623800                                                                          
623900     WRITE UT3Z-POST FROM UT3Z-AREA                                       
624000                                                                          
624100     MOVE UT3Z-IDPTYP                     TO POSTSUM-TRANSTYP             
624200     MOVE 'W4183Z'                        TO POSTSUM-FDNAMN               
624300     MOVE 'W41830DZ'                      TO POSTSUM-DDNAMN2              
624400     CALL POSTSUM USING POSTSUM-PARM                                      
624500     .                                                                    
624600     EJECT                                                                
624700 S398-SKRIV-W418AA SECTION.                                               
624800*    DISPLAY '*** S398-SKRIV-W418AA '                                     
624900                                                                          
625000     WRITE UTAA-POST FROM UTAA-AREA                                       
625100                                                                          
625200     MOVE UTAA-IDPTYP                     TO POSTSUM-TRANSTYP             
625300     MOVE 'W418AA'                        TO POSTSUM-FDNAMN               
625400     MOVE 'W41830E1'                      TO POSTSUM-DDNAMN2              
625500     CALL POSTSUM USING POSTSUM-PARM                                      
625600     .                                                                    
625700     EJECT                                                                
625800 S399-SKRIV-W418AB SECTION.                                               
625900*    DISPLAY '*** S399-SKRIV-W418AB '                                     
626000                                                                          
626100     WRITE UTAB-POST FROM UTAB-AREA                                       
626200                                                                          
626300     MOVE UTAB-IDPTYP                     TO POSTSUM-TRANSTYP             
626400     MOVE 'W418AB'                        TO POSTSUM-FDNAMN               
626500     MOVE 'W41830E2'                      TO POSTSUM-DDNAMN2              
626600     CALL POSTSUM USING POSTSUM-PARM                                      
626700     .                                                                    
626800     EJECT                                                                
626900 S400-SKRIV-W418AC SECTION.                                               
627000*    DISPLAY '*** S400-SKRIV-W418AC '                                     
627100                                                                          
627200     WRITE UTAC-POST FROM UTAC-AREA                                       
627300                                                                          
627400     MOVE UTAC-IDPTYP                     TO POSTSUM-TRANSTYP             
627500     MOVE 'W418AC'                        TO POSTSUM-FDNAMN               
627600     MOVE 'W41830E3'                      TO POSTSUM-DDNAMN2              
627700     CALL POSTSUM USING POSTSUM-PARM                                      
627800     .                                                                    
627900     EJECT                                                                
628000 S401-SKRIV-W418AD SECTION.                                               
628100*    DISPLAY '*** S401-SKRIV-W418AD '                                     
628200                                                                          
628300     WRITE UTAD-POST FROM UTAD-AREA                                       
628400                                                                          
628500     MOVE UTAD-IDPTYP                     TO POSTSUM-TRANSTYP             
628600     MOVE 'W418AD'                        TO POSTSUM-FDNAMN               
628700     MOVE 'W41830E4'                      TO POSTSUM-DDNAMN2              
628800     CALL POSTSUM USING POSTSUM-PARM                                      
628900     .                                                                    
629000     EJECT                                                                
629100 S402-SKRIV-W418AE SECTION.                                               
629200*    DISPLAY '*** S402-SKRIV-W418AE '                                     
629300                                                                          
629400     WRITE UTAE-POST FROM UTAE-AREA                                       
629500                                                                          
629600     MOVE UTAE-IDPTYP                     TO POSTSUM-TRANSTYP             
629700     MOVE 'W418AE'                        TO POSTSUM-FDNAMN               
629800     MOVE 'W41830E6'                      TO POSTSUM-DDNAMN2              
629900     CALL POSTSUM USING POSTSUM-PARM                                      
630000     .                                                                    
630100     EJECT                                                                
630200 S403-SKRIV-W418AF SECTION.                                               
630300*    DISPLAY '*** S403-SKRIV-W418AF '                                     
630400                                                                          
630500     WRITE UTAF-POST FROM UTAF-AREA                                       
630600                                                                          
630700     MOVE UTAF-IDPTYP                     TO POSTSUM-TRANSTYP             
630800     MOVE 'W418AF'                        TO POSTSUM-FDNAMN               
630900     MOVE 'W41830E7'                      TO POSTSUM-DDNAMN2              
631000     CALL POSTSUM USING POSTSUM-PARM                                      
631100     .                                                                    
631200     EJECT                                                                
631300 S404-SKRIV-W418AH SECTION.                                               
631400*    DISPLAY '*** S404-SKRIV-W418AH '                                     
631500                                                                          
631600     WRITE UTAH-POST FROM UTAH-AREA                                       
631700                                                                          
631800     MOVE UTAH-IDPTYP                     TO POSTSUM-TRANSTYP             
631900     MOVE 'W418AH'                        TO POSTSUM-FDNAMN               
632000     MOVE 'W41830E8'                      TO POSTSUM-DDNAMN2              
632100     CALL POSTSUM USING POSTSUM-PARM                                      
632200     .                                                                    
632300     EJECT                                                                
632400 S405-SKRIV-W4183L SECTION.                                               
632500*    DISPLAY '*** S405-SKRIV-W4183L '                                     
632600                                                                          
632700     WRITE UT3L-POST FROM UT3L-AREA                                       
632800                                                                          
632900     MOVE UT3L-IDPTYP                     TO POSTSUM-TRANSTYP             
633000     MOVE 'W4183L'                        TO POSTSUM-FDNAMN               
633100     MOVE 'W41830E9'                      TO POSTSUM-DDNAMN2              
633200     CALL POSTSUM USING POSTSUM-PARM                                      
633300     .                                                                    
633400     EJECT                                                                
633500 S406-SKRIV-W418AP SECTION.                                               
633600*    DISPLAY '*** S406-SKRIV-W418AP '                                     
633700                                                                          
633800     WRITE UTAP-POST FROM UTAP-AREA                                       
633900                                                                          
634000     MOVE UTAP-IDPTYP                     TO POSTSUM-TRANSTYP             
634100     MOVE 'W418AP'                        TO POSTSUM-FDNAMN               
634200     MOVE 'W41830EB'                      TO POSTSUM-DDNAMN2              
634300     CALL POSTSUM USING POSTSUM-PARM                                      
634400     .                                                                    
634500     EJECT                                                                
634600 S407-SKRIV-W418AQ SECTION.                                               
634700*    DISPLAY '*** S407-SKRIV-W418AQ '                                     
634800                                                                          
634900     WRITE UTAQ-POST FROM UTAQ-AREA                                       
635000                                                                          
635100     MOVE UTAQ-IDPTYP                     TO POSTSUM-TRANSTYP             
635200     MOVE 'W418AQ'                        TO POSTSUM-FDNAMN               
635300     MOVE 'W41830EC'                      TO POSTSUM-DDNAMN2              
635400     CALL POSTSUM USING POSTSUM-PARM                                      
635500     .                                                                    
635600     EJECT                                                                
635700 S408-SKRIV-W418C1 SECTION.                                               
635800*    DISPLAY '*** S408-SKRIV-W418C1 '                                     
635900                                                                          
636000     WRITE UTC1-POST FROM UTC1-AREA                                       
636100                                                                          
636200     MOVE UTC1-IDPTYP                     TO POSTSUM-TRANSTYP             
636300     MOVE 'W418C1'                        TO POSTSUM-FDNAMN               
636400     MOVE 'W41830EG'                      TO POSTSUM-DDNAMN2              
636500     CALL POSTSUM USING POSTSUM-PARM                                      
636600     .                                                                    
636700     EJECT                                                                
636800 S409-SKRIV-W418AS SECTION.                                               
636900*    DISPLAY '*** S409-SKRIV-W418AS '                                     
637000                                                                          
637100     WRITE UTAS-POST FROM UTAS-AREA                                       
637200                                                                          
637300     MOVE UTAS-IDPTYP                     TO POSTSUM-TRANSTYP             
637400     MOVE 'W418AS'                        TO POSTSUM-FDNAMN               
637500     MOVE 'W41830EE'                      TO POSTSUM-DDNAMN2              
637600     CALL POSTSUM USING POSTSUM-PARM                                      
637700     .                                                                    
637800     EJECT                                                                
637900 S410-SKRIV-W418AT SECTION.                                               
638000*    DISPLAY '*** S410-SKRIV-W418AT '                                     
638100                                                                          
638200     WRITE UTAT-POST FROM UTAT-AREA                                       
638300                                                                          
638400     MOVE UTAT-RP-IDPTYP                  TO POSTSUM-TRANSTYP             
638500     MOVE 'W418AT'                        TO POSTSUM-FDNAMN               
638600     MOVE 'W41830EF'                      TO POSTSUM-DDNAMN2              
638700     CALL POSTSUM USING POSTSUM-PARM                                      
638800     .                                                                    
638900 S411-SKRIV-W418IN SECTION.                                               
639000*    DISPLAY '*** S411-SKRIV-W418IN '                                     
639100                                                                          
639200     WRITE UTIN-POST FROM UTIN-AREA                                       
639300                                                                          
639400     MOVE UTIN-IDPTYP                     TO POSTSUM-TRANSTYP             
639500     MOVE 'W418IN'                        TO POSTSUM-FDNAMN               
639600     MOVE 'W41830EI'                      TO POSTSUM-DDNAMN2              
639700     CALL POSTSUM USING POSTSUM-PARM                                      
639800     .                                                                    
639900     EJECT                                                                
640000 S412-SKRIV-W418CZ SECTION.                                               
640100*    DISPLAY '*** S412-SKRIV-W418CZ '                                     
640200                                                                          
640300     WRITE UTCZ-POST FROM UTCZ-AREA                                       
640400                                                                          
640500     MOVE UTCZ-IDPTYP                     TO POSTSUM-TRANSTYP             
640600     MOVE 'W418CZ'                        TO POSTSUM-FDNAMN               
640700     MOVE 'W41830EJ'                      TO POSTSUM-DDNAMN2              
640800     CALL POSTSUM USING POSTSUM-PARM                                      
640900     .                                                                    
641000     EJECT                                                                
641100 S413-SKRIV-W418HU SECTION.                                               
641200*    DISPLAY '*** S413-SKRIV-W418HU '                                     
641300                                                                          
641400     WRITE UTHU-POST FROM UTHU-AREA                                       
641500                                                                          
641600     MOVE UTHU-IDPTYP                     TO POSTSUM-TRANSTYP             
641700     MOVE 'W418HU'                        TO POSTSUM-FDNAMN               
641800     MOVE 'W41830EK'                      TO POSTSUM-DDNAMN2              
641900     CALL POSTSUM USING POSTSUM-PARM                                      
642000     .                                                                    
642100     EJECT                                                                
642200** CREATING TEXT FOR THE MQ FLOW **                                       
642300 S414-SKRIV-W418MQA SECTION.                                              
642400*    DISPLAY '*** S414-SKRIV-W418MQA '                                    
642500                                                                          
642600     MOVE 'WC1'                TO UTMQ-IDPTYP                             
642700     MOVE ANM-IDDISTR          TO UTMQ-IDDISTR                            
642800     MOVE ANM-IDKUNDNR         TO UTMQ-IDKUNDNR                           
642900     MOVE ANM-IDRAPPNR         TO UTMQ-IDRAPPNR                           
643000     MOVE LEV-IDRADNR          TO UTMQ-IDRADNR                            
643100     MOVE TXT-TEANMNOT-DLR (1) TO UTMQ-TEANMNOT-DLR (1)                   
643200     MOVE TXT-TEANMNOT-DLR (2) TO UTMQ-TEANMNOT-DLR (2)                   
643300     MOVE TXT-TEANMNOT-DLR (3) TO UTMQ-TEANMNOT-DLR (3)                   
643400                                                                          
643500     WRITE UTMQ-POST FROM UTMQ-AREA                                       
643600                                                                          
643700     MOVE UTMQ-IDPTYP                     TO POSTSUM-TRANSTYP             
643800     MOVE 'W418MQA'                       TO POSTSUM-FDNAMN               
643900     MOVE 'W41830MQ'                      TO POSTSUM-DDNAMN2              
644000     CALL POSTSUM USING POSTSUM-PARM                                      
644100     .                                                                    
644200     EJECT                                                                
644300 S40-KNOTNR  SECTION.                                                     
644400*      DISPLAY '******** S40-KNOTNR '                                     
644500*                            *** KREDITNOTNUMMER                          
644600                                                                          
644700     IF CDC                                                               
644800        IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                      
644900           MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT                
645000        END-IF                                                            
645100        ADD +1                         TO IN-IDKNOTNR-CDC-AKT             
645200        MOVE IN-IDKNOTNR-CDC-AKT       TO W-IDKNOTNR-CDC                  
645300     ELSE                                                                 
645400        EVALUATE TRUE                                                     
645500           WHEN SDC                                                       
645600              IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                
645700                MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT           
645800              END-IF                                                      
645900              ADD +1                   TO                                 
646000                                         IN-IDKNOTNR-CDC-AKT              
646100              MOVE IN-IDKNOTNR-CDC-AKT TO W-IDKNOTNR-SDC                  
646200           WHEN NDC-US                                                    
646300              IF IN-IDKNOTNR-USA-AKT = IN-IDKNOTNR-USA-MAX                
646400                 MOVE +0                  TO IN-IDKNOTNR-USA-AKT          
646500              END-IF                                                      
646600              ADD +1                      TO IN-IDKNOTNR-USA-AKT          
646700              MOVE IN-IDKNOTNR-USA-AKT    TO W-IDKNOTNR-USA               
646800           WHEN NDC-CA                                                    
646900              IF IN-IDKNOTNR-CAN-AKT = IN-IDKNOTNR-CAN-MAX                
647000                 MOVE +0                  TO IN-IDKNOTNR-CAN-AKT          
647100              END-IF                                                      
647200              ADD +1                      TO IN-IDKNOTNR-CAN-AKT          
647300              MOVE IN-IDKNOTNR-CAN-AKT    TO W-IDKNOTNR-CAN               
647400           WHEN NDC-JP                                                    
647500              IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                
647600                MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT           
647700              END-IF                                                      
647800              ADD +1                   TO                                 
647900                                         IN-IDKNOTNR-CDC-AKT              
648000              MOVE IN-IDKNOTNR-CDC-AKT    TO W-IDKNOTNR-JAP               
648100           WHEN NDC-AU                                                    
648200              IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                
648300                MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT           
648400              END-IF                                                      
648500              ADD +1                   TO                                 
648600                                         IN-IDKNOTNR-CDC-AKT              
648700              MOVE IN-IDKNOTNR-CDC-AKT    TO W-IDKNOTNR-AUS               
648800           WHEN GOOD-DDC                                                  
648900              IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                
649000                MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT           
649100              END-IF                                                      
649200              ADD +1                   TO                                 
649300                                          IN-IDKNOTNR-CDC-AKT             
649400                                                                          
649500              EVALUATE TRUE                                               
649600                WHEN DDC-SE                                               
649700                  MOVE IN-IDKNOTNR-CDC-AKT    TO W-IDKNOTNR-SE            
649800                WHEN DDC-NO                                               
649900                  MOVE IN-IDKNOTNR-CDC-AKT    TO W-IDKNOTNR-NO            
650000                WHEN DDC-BE                                               
650100                  MOVE IN-IDKNOTNR-CDC-AKT    TO W-IDKNOTNR-BE            
650200              END-EVALUATE                                                
650300        END-EVALUATE                                                      
650400     END-IF                                                               
650500     .                                                                    
650600     EJECT                                                                
650700 S41-KNOTNR-RETUR-ITL  SECTION.                                           
650800*      DISPLAY '******** S41-KNOTNR '                                     
650900*                            *** KREDITNOTNUMMER                          
651000                                                                          
651100     IF IN-IDKNOTNR-CDC-AKT = IN-IDKNOTNR-CDC-MAX                         
651200        MOVE IN-IDKNOTNR-CDC-MIN TO IN-IDKNOTNR-CDC-AKT                   
651300     END-IF                                                               
651400     ADD +1                            TO IN-IDKNOTNR-CDC-AKT             
651500     MOVE IN-IDKNOTNR-CDC-AKT          TO W-IDKNOTNR-ITL                  
651600     .                                                                    
651700     EJECT                                                                
651800 S42-SKAPA-RADER-VIR SECTION.                                             
651900*    DISPLAY '*** S42-SKAPA-RADER-VIR'                                    
652000                                                                          
652100     MOVE 'VIR'                           TO VIR-IDPTYP                   
652200     MOVE ANM-IDDISTR                     TO VIR-IDDISTR                  
652300                                               W-IDDISTR-L5               
652400     MOVE ANM-IDKUNDNR                    TO VIR-IDKUNDNR                 
652500                                               W-IDKUNDNR-L5              
652600     MOVE ANM-IDRAPPNR                    TO VIR-IDRAPPNR                 
652700     MOVE LEV-IDDC                        TO VIR-IDDC                     
652800                                             MOMS-WS-IDDC                 
652900     MOVE LEV-KDANMORS                    TO VIR-KDANMORS                 
653000     MOVE LEV-IDARTNR                     TO VIR-IDARTNR                  
653100                                                                          
653200*--- PRISET SKALL VARA UTL.BEST.PRIS ENL. TUULA                           
653300     PERFORM S44-TA-FRAM-LEV-PRIS                                         
653400                                                                          
653500     MOVE LEV-IDORDNR7                    TO VIR-IDORDNR5                 
653600     MOVE LEV-IDKOLLI                     TO VIR-IDKOLLI                  
653700                                               W-IDKOLLI-L5               
653800     MOVE LEV-IDKUNDRF                    TO W-IDKUNDRF-L5                
653900                                                                          
654000     MOVE LEV-IDFAKT                      TO W-IDFAKT                     
654100*                                                                         
654200     PERFORM IMS-GU-WDL501                                                
654300     IF SEGMENT-FINNS                                                     
654400                                                                          
654500       MOVE W-IDDISTR                     TO W-IDDISTR-L5                 
654600       MOVE W-IDKUNDNR                    TO W-IDKUNDNR-L5                
654700       MOVE LEV-IDKUNDRF                  TO W-IDKUNDRF-L5                
654800       MOVE LEV-IDKOLLI                   TO W-IDKOLLI-L5                 
654900       PERFORM IMS-GNP-WDL511                                             
655000       IF SEGMENT-FINNS                                                   
655100         MOVE FAKC-IDPRODNR               TO VIR-IDPRODNR                 
655200                                                                          
655300         MOVE LEV-IDARTNR                 TO W-IDARTNR                    
655400         PERFORM IMS-GNP-WDL521                                           
655500         IF SEGMENT-FINNS                                                 
655600            MOVE FAKL-IDLEVNR             TO VIR-IDLEVNR                  
655700         ELSE                                                             
655800           MOVE SPACE                     TO VIR-IDLEVNR                  
655900         END-IF                                                           
656000       ELSE                                                               
656100         MOVE ZERO                        TO VIR-IDPRODNR                 
656200         MOVE SPACE                       TO VIR-IDLEVNR                  
656300       END-IF                                                             
656400     ELSE                                                                 
656500       MOVE ZERO                          TO VIR-IDPRODNR                 
656600       MOVE SPACE                         TO VIR-IDLEVNR                  
656700     END-IF                                                               
656800                                                                          
656900     IF OKOD-FL-KOD-SOM-BAER-TK  = JA                                     
657000       COMPUTE VIR-PRLANDCO ROUNDED =                                     
657100        (VIR-KVLEVANM * LEV-PRARTBTO * ANM-RELANDCO) / 100                
657200     ELSE                                                                 
657300        MOVE ZERO                         TO VIR-PRLANDCO                 
657400     END-IF                                                               
657500                                                                          
657600     IF ANM-REEMBHNT > ZERO                                               
657700        COMPUTE VIR-PREMBHNT ROUNDED =                                    
657800        (VIR-KVLEVANM * LEV-PRARTBTO * ANM-REEMBHNT) / 100                
657900     ELSE                                                                 
658000        MOVE ZERO                          TO VIR-PREMBHNT                
658100     END-IF                                                               
658200                                                                          
658300     MOVE ANM-PRFOERS                     TO VIR-PRFOERS                  
658400     MOVE ANM-PRFRAKT                     TO VIR-PRFRAKT                  
658500     MOVE ANM-PRLEGKST                    TO VIR-PRLEGKST                 
658600     MOVE ZERO                            TO VIR-PRMOMS                   
658700     MOVE +0                              TO W009-MOMS-REVAT              
658800                                                                          
658900     IF LEV-KDANMORS = '13' OR '23' OR '12' OR '22' OR                    
659000                                       '27' OR '28'                       
659100       CONTINUE                                                           
659200     ELSE                                                                 
659300       PERFORM S70-SAETT-EV-MOMS                                          
659400       COMPUTE VIR-PRMOMS ROUNDED =                                       
659500               VIR-KVLEVANM *  LEV-PRARTBTO * W009-MOMS-REVAT             
659600     END-IF                                                               
659700                                                                          
659800     MOVE ANM-IDFTG    TO WS-IDFTG                                        
659900     IF IDFTG-NON-VCC                                                     
660000       CONTINUE                                                           
660100     ELSE                                                                 
660200       PERFORM S43-SKRIV-W41843-VIR                                       
660300     END-IF                                                               
660400                                                                          
660500     .                                                                    
660600     EJECT                                                                
660700 S43-SKRIV-W41843-VIR SECTION.                                            
660800*    DISPLAY '*** S43-SKRIV-W41843 '                                      
660900                                                                          
661000     WRITE UT43-POST FROM UT43-AREA                                       
661100                                                                          
661200     MOVE VIR-IDPTYP                      TO POSTSUM-TRANSTYP             
661300     MOVE 'W41843'                        TO POSTSUM-FDNAMN               
661400     MOVE 'W41830E5'                      TO POSTSUM-DDNAMN2              
661500     CALL POSTSUM USING POSTSUM-PARM                                      
661600     .                                                                    
661700     EJECT                                                                
661800 S44-TA-FRAM-LEV-PRIS SECTION.                                            
661900                                                                          
662000     PERFORM IMS-GNP-WLARTC21-FIRST                                       
662100                                                                          
662200     IF SEGMENT-FINNS                                                     
662300       IF PRL-KDSTATUS-PR = 1  AND                                        
662400          PRL-SUINLEV-PR > 0   AND                                        
662500          PRL-FLHUVLEV = JA                                               
662600         MOVE  PRL-IDLEVNR      TO VIR-IDLEVNR-PRIS                       
662700         MOVE  PRL-KDVALISO     TO VIR-KDVALISO                           
662800         COMPUTE VIR-PRARTBTO ROUNDED =                                   
662900                   (VIR-KVLEVANM * PRL-PRARTBEL-PR )                      
663000       ELSE                                                               
663100         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
663200                OR (PRL-KDSTATUS-PR = 1 AND                               
663300                    PRL-SUINLEV-PR > 0  AND                               
663400                    PRL-FLHUVLEV = JA )                                   
663500           PERFORM IMS-GNP-WLARTC21                                       
663600           IF PRL-KDSTATUS-PR = 1  AND                                    
663700               PRL-SUINLEV-PR > 0  AND                                    
663800                 PRL-FLHUVLEV = JA                                        
663900             MOVE  PRL-IDLEVNR      TO VIR-IDLEVNR-PRIS                   
664000             MOVE  PRL-KDVALISO     TO VIR-KDVALISO                       
664100             COMPUTE VIR-PRARTBTO ROUNDED =                               
664200                   (VIR-KVLEVANM * PRL-PRARTBEL-PR )                      
664300           ELSE                                                           
664400             MOVE  SPACE            TO VIR-IDLEVNR-PRIS                   
664500             MOVE  SPACE            TO VIR-KDVALISO                       
664600             MOVE  ZERO             TO VIR-PRARTBTO                       
664700           END-IF                                                         
664800         END-PERFORM                                                      
664900       END-IF                                                             
665000     ELSE                                                                 
665100       MOVE  SPACE            TO VIR-IDLEVNR-PRIS                         
665200       MOVE  SPACE            TO VIR-KDVALISO                             
665300       MOVE  ZERO             TO VIR-PRARTBTO                             
665400     END-IF                                                               
665500                                                                          
665600     .                                                                    
665700     EJECT                                                                
665800 S45-SKAPA-TXT-TILL-VIPS SECTION.                                         
665900                                                                          
666000     INITIALIZE UTMQ-AREA                                                 
666100     MOVE LEV-IDARTNR                     TO W-IDARTNR-A211               
666200     MOVE LEV-IDRADNR                     TO W-IDRADNR-A211               
666300     PERFORM IMS-GNP-KREE-TXT                                             
666400     IF SEGMENT-FINNS                                                     
666500       IF TXT-TEANMNOT-DLR (1) = SPACE AND                                
666600          TXT-TEANMNOT-DLR (2) = SPACE AND                                
666700          TXT-TEANMNOT-DLR (3) = SPACE                                    
666800         CONTINUE                                                         
666900       ELSE                                                               
667000         EVALUATE LEV-IDFTG                                               
667100           WHEN WC-IDFTG-US                                               
667200             MOVE 'WC1'                TO UT3C-IDPTYP                     
667300             MOVE ANM-IDDISTR          TO UT3C-IDDISTR                    
667400             MOVE ANM-IDKUNDNR         TO UT3C-IDKUNDNR                   
667500             MOVE ANM-IDRAPPNR         TO UT3C-IDRAPPNR                   
667600             MOVE LEV-IDRADNR          TO UT3C-IDRADNR                    
667700             MOVE TXT-TEANMNOT-DLR (1) TO UT3C-TEANMNOT-DLR (1)           
667800             MOVE TXT-TEANMNOT-DLR (2) TO UT3C-TEANMNOT-DLR (2)           
667900             MOVE TXT-TEANMNOT-DLR (3) TO UT3C-TEANMNOT-DLR (3)           
668000             MOVE ISO-USA              TO UTMQ-IDLANDX2                   
668100                                                                          
668200             PERFORM S21-SKRIV-W4183C                                     
668300                                                                          
668400           WHEN WC-IDFTG-CA                                               
668500             MOVE 'WC1'                TO UT3D-IDPTYP                     
668600             MOVE ANM-IDDISTR          TO UT3D-IDDISTR                    
668700             MOVE ANM-IDKUNDNR         TO UT3D-IDKUNDNR                   
668800             MOVE ANM-IDRAPPNR         TO UT3D-IDRAPPNR                   
668900             MOVE LEV-IDRADNR          TO UT3D-IDRADNR                    
669000             MOVE TXT-TEANMNOT-DLR (1) TO UT3D-TEANMNOT-DLR (1)           
669100             MOVE TXT-TEANMNOT-DLR (2) TO UT3D-TEANMNOT-DLR (2)           
669200             MOVE TXT-TEANMNOT-DLR (3) TO UT3D-TEANMNOT-DLR (3)           
669300             MOVE ISO-CANADA           TO UTMQ-IDLANDX2                   
669400                                                                          
669500             PERFORM S22-SKRIV-W4183D                                     
669600                                                                          
669700           WHEN WC-IDFTG-CN                                               
669800             MOVE 'WC1'                TO UTC1-IDPTYP                     
669900             MOVE ANM-IDDISTR          TO UTC1-IDDISTR                    
670000             MOVE ANM-IDKUNDNR         TO UTC1-IDKUNDNR                   
670100             MOVE ANM-IDRAPPNR         TO UTC1-IDRAPPNR                   
670200             MOVE LEV-IDRADNR          TO UTC1-IDRADNR                    
670300             MOVE TXT-TEANMNOT-DLR (1) TO UTC1-TEANMNOT-DLR (1)           
670400             MOVE TXT-TEANMNOT-DLR (2) TO UTC1-TEANMNOT-DLR (2)           
670500             MOVE TXT-TEANMNOT-DLR (3) TO UTC1-TEANMNOT-DLR (3)           
670600             MOVE ISO-KINA             TO UTMQ-IDLANDX2                   
670700                                                                          
670800             PERFORM S408-SKRIV-W418C1                                    
670900                                                                          
671000           WHEN WC-IDFTG-IN                                               
671100*HÄR SKAPAS EN FIL TILL VIPS FÖR INDIEN                                   
671200             MOVE 'WC1'                TO UTIN-IDPTYP                     
671300             MOVE ANM-IDDISTR          TO UTIN-IDDISTR                    
671400             MOVE ANM-IDKUNDNR         TO UTIN-IDKUNDNR                   
671500             MOVE ANM-IDRAPPNR         TO UTIN-IDRAPPNR                   
671600             MOVE LEV-IDRADNR          TO UTIN-IDRADNR                    
671700             MOVE TXT-TEANMNOT-DLR (1) TO UTIN-TEANMNOT-DLR (1)           
671800             MOVE TXT-TEANMNOT-DLR (2) TO UTIN-TEANMNOT-DLR (2)           
671900             MOVE TXT-TEANMNOT-DLR (3) TO UTIN-TEANMNOT-DLR (3)           
672000             MOVE ISO-INDIEN           TO UTMQ-IDLANDX2                   
672100                                                                          
672200             PERFORM S411-SKRIV-W418IN                                    
672300                                                                          
672400           WHEN WC-IDFTG-KR                                               
672500*HÄR SKAPAS EN FIL TILL VIPS FÖR KOREA                                    
672600             MOVE 'WC1'                TO UT3I-IDPTYP                     
672700             MOVE ANM-IDDISTR          TO UT3I-IDDISTR                    
672800             MOVE ANM-IDKUNDNR         TO UT3I-IDKUNDNR                   
672900             MOVE ANM-IDRAPPNR         TO UT3I-IDRAPPNR                   
673000             MOVE LEV-IDRADNR          TO UT3I-IDRADNR                    
673100             MOVE TXT-TEANMNOT-DLR (1) TO UT3I-TEANMNOT-DLR (1)           
673200             MOVE TXT-TEANMNOT-DLR (2) TO UT3I-TEANMNOT-DLR (2)           
673300             MOVE TXT-TEANMNOT-DLR (3) TO UT3I-TEANMNOT-DLR (3)           
673400             MOVE ISO-KOREA            TO UTMQ-IDLANDX2                   
673500                                                                          
673600             PERFORM S27-SKRIV-W4183I                                     
673700                                                                          
673800           WHEN WC-IDFTG-TR                                               
673900             MOVE 'WC1'                TO UTAH-IDPTYP                     
674000             MOVE ANM-IDDISTR          TO UTAH-IDDISTR                    
674100             MOVE ANM-IDKUNDNR         TO UTAH-IDKUNDNR                   
674200             MOVE ANM-IDRAPPNR         TO UTAH-IDRAPPNR                   
674300             MOVE LEV-IDRADNR          TO UTAH-IDRADNR                    
674400             MOVE TXT-TEANMNOT-DLR (1) TO UTAH-TEANMNOT-DLR (1)           
674500             MOVE TXT-TEANMNOT-DLR (2) TO UTAH-TEANMNOT-DLR (2)           
674600             MOVE TXT-TEANMNOT-DLR (3) TO UTAH-TEANMNOT-DLR (3)           
674700             MOVE ISO-TURKIET          TO UTMQ-IDLANDX2                   
674800                                                                          
674900             PERFORM S404-SKRIV-W418AH                                    
675000                                                                          
675100           WHEN WC-IDFTG-TW                                               
675200             MOVE 'WC1'                TO UT3L-IDPTYP                     
675300             MOVE ANM-IDDISTR          TO UT3L-IDDISTR                    
675400             MOVE ANM-IDKUNDNR         TO UT3L-IDKUNDNR                   
675500             MOVE ANM-IDRAPPNR         TO UT3L-IDRAPPNR                   
675600             MOVE LEV-IDRADNR          TO UT3L-IDRADNR                    
675700             MOVE TXT-TEANMNOT-DLR (1) TO UT3L-TEANMNOT-DLR (1)           
675800             MOVE TXT-TEANMNOT-DLR (2) TO UT3L-TEANMNOT-DLR (2)           
675900             MOVE TXT-TEANMNOT-DLR (3) TO UT3L-TEANMNOT-DLR (3)           
676000             MOVE ISO-TAIWAN2          TO UTMQ-IDLANDX2                   
676100                                                                          
676200             PERFORM S405-SKRIV-W4183L                                    
676300                                                                          
676400           WHEN WC-IDFTG-MY                                               
676500             MOVE 'WC1'                TO UTAB-IDPTYP                     
676600             MOVE ANM-IDDISTR          TO UTAB-IDDISTR                    
676700             MOVE ANM-IDKUNDNR         TO UTAB-IDKUNDNR                   
676800             MOVE ANM-IDRAPPNR         TO UTAB-IDRAPPNR                   
676900             MOVE LEV-IDRADNR          TO UTAB-IDRADNR                    
677000             MOVE TXT-TEANMNOT-DLR (1) TO UTAB-TEANMNOT-DLR (1)           
677100             MOVE TXT-TEANMNOT-DLR (2) TO UTAB-TEANMNOT-DLR (2)           
677200             MOVE TXT-TEANMNOT-DLR (3) TO UTAB-TEANMNOT-DLR (3)           
677300             MOVE ISO-MALAYSIA         TO UTMQ-IDLANDX2                   
677400                                                                          
677500             PERFORM S399-SKRIV-W418AB                                    
677600                                                                          
677700           WHEN WC-IDFTG-TH                                               
677800             MOVE 'WC1'                TO UTAC-IDPTYP                     
677900             MOVE ANM-IDDISTR          TO UTAC-IDDISTR                    
678000             MOVE ANM-IDKUNDNR         TO UTAC-IDKUNDNR                   
678100             MOVE ANM-IDRAPPNR         TO UTAC-IDRAPPNR                   
678200             MOVE LEV-IDRADNR          TO UTAC-IDRADNR                    
678300             MOVE TXT-TEANMNOT-DLR (1) TO UTAC-TEANMNOT-DLR (1)           
678400             MOVE TXT-TEANMNOT-DLR (2) TO UTAC-TEANMNOT-DLR (2)           
678500             MOVE TXT-TEANMNOT-DLR (3) TO UTAC-TEANMNOT-DLR (3)           
678600             MOVE ISO-THAILAND         TO UTMQ-IDLANDX2                   
678700                                                                          
678800             PERFORM S400-SKRIV-W418AC                                    
678900                                                                          
679000           WHEN WC-IDFTG-PV                                               
679100                                                                          
679200             MOVE ANM-IDDISTR            TO DIS1-IDDISTR                  
679300             CALL W460DIS1 USING DIS1-W460DIS1                            
679400                                                                          
679500             IF DIS1-IDLANDX2 = ISO-BELGIEN                               
679600               MOVE 'WC1'                TO UT3E-IDPTYP                   
679700               MOVE ANM-IDDISTR          TO UT3E-IDDISTR                  
679800               MOVE ANM-IDKUNDNR         TO UT3E-IDKUNDNR                 
679900               MOVE ANM-IDRAPPNR         TO UT3E-IDRAPPNR                 
680000               MOVE LEV-IDRADNR          TO UT3E-IDRADNR                  
680100               MOVE TXT-TEANMNOT-DLR (1) TO UT3E-TEANMNOT-DLR (1)         
680200               MOVE TXT-TEANMNOT-DLR (2) TO UT3E-TEANMNOT-DLR (2)         
680300               MOVE TXT-TEANMNOT-DLR (3) TO UT3E-TEANMNOT-DLR (3)         
680400               MOVE ISO-BELGIEN          TO UTMQ-IDLANDX2                 
680500                                                                          
680600               PERFORM S23-SKRIV-W4183E                                   
680700             END-IF                                                       
680800                                                                          
680900             IF DIS1-IDLANDX2 = ISO-HOLLAND                               
681000               MOVE 'WC1'                TO UT3F-IDPTYP                   
681100               MOVE ANM-IDDISTR          TO UT3F-IDDISTR                  
681200               MOVE ANM-IDKUNDNR         TO UT3F-IDKUNDNR                 
681300               MOVE ANM-IDRAPPNR         TO UT3F-IDRAPPNR                 
681400               MOVE LEV-IDRADNR          TO UT3F-IDRADNR                  
681500               MOVE TXT-TEANMNOT-DLR (1) TO UT3F-TEANMNOT-DLR (1)         
681600               MOVE TXT-TEANMNOT-DLR (2) TO UT3F-TEANMNOT-DLR (2)         
681700               MOVE TXT-TEANMNOT-DLR (3) TO UT3F-TEANMNOT-DLR (3)         
681800               MOVE ISO-HOLLAND          TO UTMQ-IDLANDX2                 
681900                                                                          
682000               PERFORM S24-SKRIV-W4183F                                   
682100             END-IF                                                       
682200                                                                          
682300             IF DIS1-IDLANDX2 = ISO-ENGLAND                               
682400               MOVE 'WC1'                TO UT3G-IDPTYP                   
682500               MOVE ANM-IDDISTR          TO UT3G-IDDISTR                  
682600               MOVE ANM-IDKUNDNR         TO UT3G-IDKUNDNR                 
682700               MOVE ANM-IDRAPPNR         TO UT3G-IDRAPPNR                 
682800               MOVE LEV-IDRADNR          TO UT3G-IDRADNR                  
682900               MOVE TXT-TEANMNOT-DLR (1) TO UT3G-TEANMNOT-DLR (1)         
683000               MOVE TXT-TEANMNOT-DLR (2) TO UT3G-TEANMNOT-DLR (2)         
683100               MOVE TXT-TEANMNOT-DLR (3) TO UT3G-TEANMNOT-DLR (3)         
683200               MOVE ISO-ENGLAND          TO UTMQ-IDLANDX2                 
683300                                                                          
683400               PERFORM S25-SKRIV-W4183G                                   
683500             END-IF                                                       
683600                                                                          
683700             IF DIS1-IDLANDX2 = ISO-KOREA                                 
683800               MOVE 'WC1'                TO UT3I-IDPTYP                   
683900               MOVE ANM-IDDISTR          TO UT3I-IDDISTR                  
684000               MOVE ANM-IDKUNDNR         TO UT3I-IDKUNDNR                 
684100               MOVE ANM-IDRAPPNR         TO UT3I-IDRAPPNR                 
684200               MOVE LEV-IDRADNR          TO UT3I-IDRADNR                  
684300               MOVE TXT-TEANMNOT-DLR (1) TO UT3I-TEANMNOT-DLR (1)         
684400               MOVE TXT-TEANMNOT-DLR (2) TO UT3I-TEANMNOT-DLR (2)         
684500               MOVE TXT-TEANMNOT-DLR (3) TO UT3I-TEANMNOT-DLR (3)         
684600               MOVE ISO-KOREA            TO UTMQ-IDLANDX2                 
684700                                                                          
684800               PERFORM S27-SKRIV-W4183I                                   
684900             END-IF                                                       
685000                                                                          
685100             IF DIS1-IDLANDX2 = ISO-POLEN                                 
685200               MOVE 'WC1'                TO UT3J-IDPTYP                   
685300               MOVE ANM-IDDISTR          TO UT3J-IDDISTR                  
685400               MOVE ANM-IDKUNDNR         TO UT3J-IDKUNDNR                 
685500               MOVE ANM-IDRAPPNR         TO UT3J-IDRAPPNR                 
685600               MOVE LEV-IDRADNR          TO UT3J-IDRADNR                  
685700               MOVE TXT-TEANMNOT-DLR (1) TO UT3J-TEANMNOT-DLR (1)         
685800               MOVE TXT-TEANMNOT-DLR (2) TO UT3J-TEANMNOT-DLR (2)         
685900               MOVE TXT-TEANMNOT-DLR (3) TO UT3J-TEANMNOT-DLR (3)         
686000               MOVE ISO-POLEN            TO UTMQ-IDLANDX2                 
686100                                                                          
686200               PERFORM S28-SKRIV-W4183J                                   
686300             END-IF                                                       
686400                                                                          
686500             IF DIS1-IDLANDX2 = ISO-TYSKLAND                              
686600               MOVE 'WC1'                TO UT3K-IDPTYP                   
686700               MOVE ANM-IDDISTR          TO UT3K-IDDISTR                  
686800               MOVE ANM-IDKUNDNR         TO UT3K-IDKUNDNR                 
686900               MOVE ANM-IDRAPPNR         TO UT3K-IDRAPPNR                 
687000               MOVE LEV-IDRADNR          TO UT3K-IDRADNR                  
687100               MOVE TXT-TEANMNOT-DLR (1) TO UT3K-TEANMNOT-DLR (1)         
687200               MOVE TXT-TEANMNOT-DLR (2) TO UT3K-TEANMNOT-DLR (2)         
687300               MOVE TXT-TEANMNOT-DLR (3) TO UT3K-TEANMNOT-DLR (3)         
687400               MOVE ISO-TYSKLAND         TO UTMQ-IDLANDX2                 
687500                                                                          
687600               PERFORM S29-SKRIV-W4183K                                   
687700             END-IF                                                       
687800                                                                          
687900                                                                          
688000             IF DIS1-IDLANDX2 = ISO-NORGE                                 
688100               MOVE 'WC1'                TO UT3N-IDPTYP                   
688200               MOVE ANM-IDDISTR          TO UT3N-IDDISTR                  
688300               MOVE ANM-IDKUNDNR         TO UT3N-IDKUNDNR                 
688400               MOVE ANM-IDRAPPNR         TO UT3N-IDRAPPNR                 
688500               MOVE LEV-IDRADNR          TO UT3N-IDRADNR                  
688600               MOVE TXT-TEANMNOT-DLR (1) TO UT3N-TEANMNOT-DLR (1)         
688700               MOVE TXT-TEANMNOT-DLR (2) TO UT3N-TEANMNOT-DLR (2)         
688800               MOVE TXT-TEANMNOT-DLR (3) TO UT3N-TEANMNOT-DLR (3)         
688900               MOVE ISO-NORGE            TO UTMQ-IDLANDX2                 
689000                                                                          
689100               PERFORM S33-SKRIV-W4183N                                   
689200             END-IF                                                       
689300                                                                          
689400             IF DIS1-IDLANDX2 = ISO-DANMARK                               
689500               MOVE 'WC1'                TO UT3O-IDPTYP                   
689600               MOVE ANM-IDDISTR          TO UT3O-IDDISTR                  
689700               MOVE ANM-IDKUNDNR         TO UT3O-IDKUNDNR                 
689800               MOVE ANM-IDRAPPNR         TO UT3O-IDRAPPNR                 
689900               MOVE LEV-IDRADNR          TO UT3O-IDRADNR                  
690000               MOVE TXT-TEANMNOT-DLR (1) TO UT3O-TEANMNOT-DLR (1)         
690100               MOVE TXT-TEANMNOT-DLR (2) TO UT3O-TEANMNOT-DLR (2)         
690200               MOVE TXT-TEANMNOT-DLR (3) TO UT3O-TEANMNOT-DLR (3)         
690300               MOVE ISO-DANMARK          TO UTMQ-IDLANDX2                 
690400                                                                          
690500               PERFORM S34-SKRIV-W4183O                                   
690600             END-IF                                                       
690700                                                                          
690800             IF DIS1-IDLANDX2 = ISO-AUSTRALIEN                            
690900               MOVE 'WC1'                TO UT3P-IDPTYP                   
691000               MOVE ANM-IDDISTR          TO UT3P-IDDISTR                  
691100               MOVE ANM-IDKUNDNR         TO UT3P-IDKUNDNR                 
691200               MOVE ANM-IDRAPPNR         TO UT3P-IDRAPPNR                 
691300               MOVE LEV-IDRADNR          TO UT3P-IDRADNR                  
691400               MOVE TXT-TEANMNOT-DLR (1) TO UT3P-TEANMNOT-DLR (1)         
691500               MOVE TXT-TEANMNOT-DLR (2) TO UT3P-TEANMNOT-DLR (2)         
691600               MOVE TXT-TEANMNOT-DLR (3) TO UT3P-TEANMNOT-DLR (3)         
691700               MOVE ISO-AUSTRALIEN       TO UTMQ-IDLANDX2                 
691800                                                                          
691900               PERFORM S37-SKRIV-W4183P                                   
692000             END-IF                                                       
692100                                                                          
692200             IF DIS1-IDLANDX2 = ISO-SPANIEN                               
692300               MOVE 'WC1'                TO UT3Q-IDPTYP                   
692400               MOVE ANM-IDDISTR          TO UT3Q-IDDISTR                  
692500               MOVE ANM-IDKUNDNR         TO UT3Q-IDKUNDNR                 
692600               MOVE ANM-IDRAPPNR         TO UT3Q-IDRAPPNR                 
692700               MOVE LEV-IDRADNR          TO UT3Q-IDRADNR                  
692800               MOVE TXT-TEANMNOT-DLR (1) TO UT3Q-TEANMNOT-DLR (1)         
692900               MOVE TXT-TEANMNOT-DLR (2) TO UT3Q-TEANMNOT-DLR (2)         
693000               MOVE TXT-TEANMNOT-DLR (3) TO UT3Q-TEANMNOT-DLR (3)         
693100               MOVE ISO-SPANIEN          TO UTMQ-IDLANDX2                 
693200                                                                          
693300               PERFORM S38-SKRIV-W4183Q                                   
693400             END-IF                                                       
693500                                                                          
693600             IF DIS1-IDLANDX2 = ISO-SVERIGE                               
693700               MOVE 'WC1'                TO UT3R-IDPTYP                   
693800               MOVE ANM-IDDISTR          TO UT3R-IDDISTR                  
693900               MOVE ANM-IDKUNDNR         TO UT3R-IDKUNDNR                 
694000               MOVE ANM-IDRAPPNR         TO UT3R-IDRAPPNR                 
694100               MOVE LEV-IDRADNR          TO UT3R-IDRADNR                  
694200               MOVE TXT-TEANMNOT-DLR (1) TO UT3R-TEANMNOT-DLR (1)         
694300               MOVE TXT-TEANMNOT-DLR (2) TO UT3R-TEANMNOT-DLR (2)         
694400               MOVE TXT-TEANMNOT-DLR (3) TO UT3R-TEANMNOT-DLR (3)         
694500               MOVE ISO-SVERIGE          TO UTMQ-IDLANDX2                 
694600                                                                          
694700               PERFORM S39-SKRIV-W4183R                                   
694800             END-IF                                                       
694900                                                                          
695000             IF DIS1-IDLANDX2 = ISO-ITALIEN                               
695100               MOVE 'WC1'                TO UT3T-IDPTYP                   
695200               MOVE ANM-IDDISTR          TO UT3T-IDDISTR                  
695300               MOVE ANM-IDKUNDNR         TO UT3T-IDKUNDNR                 
695400               MOVE ANM-IDRAPPNR         TO UT3T-IDRAPPNR                 
695500               MOVE LEV-IDRADNR          TO UT3T-IDRADNR                  
695600               MOVE TXT-TEANMNOT-DLR (1) TO UT3T-TEANMNOT-DLR (1)         
695700               MOVE TXT-TEANMNOT-DLR (2) TO UT3T-TEANMNOT-DLR (2)         
695800               MOVE TXT-TEANMNOT-DLR (3) TO UT3T-TEANMNOT-DLR (3)         
695900               MOVE ISO-ITALIEN          TO UTMQ-IDLANDX2                 
696000                                                                          
696100               PERFORM S391-SKRIV-W4183T                                  
696200             END-IF                                                       
696300                                                                          
696400             IF DIS1-IDLANDX2 = ISO-FINLAND                               
696500               MOVE 'WC1'                TO UT3U-IDPTYP                   
696600               MOVE ANM-IDDISTR          TO UT3U-IDDISTR                  
696700               MOVE ANM-IDKUNDNR         TO UT3U-IDKUNDNR                 
696800               MOVE ANM-IDRAPPNR         TO UT3U-IDRAPPNR                 
696900               MOVE LEV-IDRADNR          TO UT3U-IDRADNR                  
697000               MOVE TXT-TEANMNOT-DLR (1) TO UT3U-TEANMNOT-DLR (1)         
697100               MOVE TXT-TEANMNOT-DLR (2) TO UT3U-TEANMNOT-DLR (2)         
697200               MOVE TXT-TEANMNOT-DLR (3) TO UT3U-TEANMNOT-DLR (3)         
697300               MOVE ISO-FINLAND          TO UTMQ-IDLANDX2                 
697400                                                                          
697500               PERFORM S392-SKRIV-W4183U                                  
697600             END-IF                                                       
697700                                                                          
697800             IF DIS1-IDLANDX2 = ISO-FRANKRIKE                             
697900               MOVE 'WC1'                TO UT3V-IDPTYP                   
698000               MOVE ANM-IDDISTR          TO UT3V-IDDISTR                  
698100               MOVE ANM-IDKUNDNR         TO UT3V-IDKUNDNR                 
698200               MOVE ANM-IDRAPPNR         TO UT3V-IDRAPPNR                 
698300               MOVE LEV-IDRADNR          TO UT3V-IDRADNR                  
698400               MOVE TXT-TEANMNOT-DLR (1) TO UT3V-TEANMNOT-DLR (1)         
698500               MOVE TXT-TEANMNOT-DLR (2) TO UT3V-TEANMNOT-DLR (2)         
698600               MOVE TXT-TEANMNOT-DLR (3) TO UT3V-TEANMNOT-DLR (3)         
698700               MOVE ISO-FRANKRIKE        TO UTMQ-IDLANDX2                 
698800                                                                          
698900               PERFORM S393-SKRIV-W4183V                                  
699000             END-IF                                                       
699100                                                                          
699200             IF DIS1-IDLANDX2 = ISO-JAPAN                                 
699300               MOVE 'WC1'                TO UT3X-IDPTYP                   
699400               MOVE ANM-IDDISTR          TO UT3X-IDDISTR                  
699500               MOVE ANM-IDKUNDNR         TO UT3X-IDKUNDNR                 
699600               MOVE ANM-IDRAPPNR         TO UT3X-IDRAPPNR                 
699700               MOVE LEV-IDRADNR          TO UT3X-IDRADNR                  
699800               MOVE TXT-TEANMNOT-DLR (1) TO UT3X-TEANMNOT-DLR (1)         
699900               MOVE TXT-TEANMNOT-DLR (2) TO UT3X-TEANMNOT-DLR (2)         
700000               MOVE TXT-TEANMNOT-DLR (3) TO UT3X-TEANMNOT-DLR (3)         
700100               MOVE ISO-JAPAN            TO UTMQ-IDLANDX2                 
700200                                                                          
700300               PERFORM S394-SKRIV-W4183X                                  
700400             END-IF                                                       
700500                                                                          
700600             IF DIS1-IDLANDX2 = ISO-OSTERRIKE                             
700700               MOVE 'WC1'                TO UT3W-IDPTYP                   
700800               MOVE ANM-IDDISTR          TO UT3W-IDDISTR                  
700900               MOVE ANM-IDKUNDNR         TO UT3W-IDKUNDNR                 
701000               MOVE ANM-IDRAPPNR         TO UT3W-IDRAPPNR                 
701100               MOVE LEV-IDRADNR          TO UT3W-IDRADNR                  
701200               MOVE TXT-TEANMNOT-DLR (1) TO UT3W-TEANMNOT-DLR (1)         
701300               MOVE TXT-TEANMNOT-DLR (2) TO UT3W-TEANMNOT-DLR (2)         
701400               MOVE TXT-TEANMNOT-DLR (3) TO UT3W-TEANMNOT-DLR (3)         
701500               MOVE ISO-OSTERRIKE        TO UTMQ-IDLANDX2                 
701600                                                                          
701700               PERFORM S395-SKRIV-W4183W                                  
701800             END-IF                                                       
701900                                                                          
702000             IF DIS1-IDLANDX2 = ISO-SCHWEIZ                               
702100               MOVE 'WC1'                TO UT3Y-IDPTYP                   
702200               MOVE ANM-IDDISTR          TO UT3Y-IDDISTR                  
702300               MOVE ANM-IDKUNDNR         TO UT3Y-IDKUNDNR                 
702400               MOVE ANM-IDRAPPNR         TO UT3Y-IDRAPPNR                 
702500               MOVE LEV-IDRADNR          TO UT3Y-IDRADNR                  
702600               MOVE TXT-TEANMNOT-DLR (1) TO UT3Y-TEANMNOT-DLR (1)         
702700               MOVE TXT-TEANMNOT-DLR (2) TO UT3Y-TEANMNOT-DLR (2)         
702800               MOVE TXT-TEANMNOT-DLR (3) TO UT3Y-TEANMNOT-DLR (3)         
702900               MOVE ISO-SCHWEIZ          TO UTMQ-IDLANDX2                 
703000                                                                          
703100               PERFORM S396-SKRIV-W4183Y                                  
703200             END-IF                                                       
703300                                                                          
703400             IF DIS1-IDLANDX2 = ISO-TAIWAN                                
703500               MOVE 'WC1'                TO UT3Z-IDPTYP                   
703600               MOVE ANM-IDDISTR          TO UT3Z-IDDISTR                  
703700               MOVE ANM-IDKUNDNR         TO UT3Z-IDKUNDNR                 
703800               MOVE ANM-IDRAPPNR         TO UT3Z-IDRAPPNR                 
703900               MOVE LEV-IDRADNR          TO UT3Z-IDRADNR                  
704000               MOVE TXT-TEANMNOT-DLR (1) TO UT3Z-TEANMNOT-DLR (1)         
704100               MOVE TXT-TEANMNOT-DLR (2) TO UT3Z-TEANMNOT-DLR (2)         
704200               MOVE TXT-TEANMNOT-DLR (3) TO UT3Z-TEANMNOT-DLR (3)         
704300               MOVE ISO-TAIWAN           TO UTMQ-IDLANDX2                 
704400                                                                          
704500               PERFORM S397-SKRIV-W4183Z                                  
704600             END-IF                                                       
704700                                                                          
704800             IF DIS1-IDLANDX2 = ISO-TAIWAN2                               
704900               MOVE 'WC1'                TO UT3L-IDPTYP                   
705000               MOVE ANM-IDDISTR          TO UT3L-IDDISTR                  
705100               MOVE ANM-IDKUNDNR         TO UT3L-IDKUNDNR                 
705200               MOVE ANM-IDRAPPNR         TO UT3L-IDRAPPNR                 
705300               MOVE LEV-IDRADNR          TO UT3L-IDRADNR                  
705400               MOVE TXT-TEANMNOT-DLR (1) TO UT3L-TEANMNOT-DLR (1)         
705500               MOVE TXT-TEANMNOT-DLR (2) TO UT3L-TEANMNOT-DLR (2)         
705600               MOVE TXT-TEANMNOT-DLR (3) TO UT3L-TEANMNOT-DLR (3)         
705700               MOVE ISO-TAIWAN2          TO UTMQ-IDLANDX2                 
705800                                                                          
705900               PERFORM S405-SKRIV-W4183L                                  
706000             END-IF                                                       
706100                                                                          
706200             IF DIS1-IDLANDX2 = ISO-PORTUGAL                              
706300               MOVE 'WC1'                TO UTAA-IDPTYP                   
706400               MOVE ANM-IDDISTR          TO UTAA-IDDISTR                  
706500               MOVE ANM-IDKUNDNR         TO UTAA-IDKUNDNR                 
706600               MOVE ANM-IDRAPPNR         TO UTAA-IDRAPPNR                 
706700               MOVE LEV-IDRADNR          TO UTAA-IDRADNR                  
706800               MOVE TXT-TEANMNOT-DLR (1) TO UTAA-TEANMNOT-DLR (1)         
706900               MOVE TXT-TEANMNOT-DLR (2) TO UTAA-TEANMNOT-DLR (2)         
707000               MOVE TXT-TEANMNOT-DLR (3) TO UTAA-TEANMNOT-DLR (3)         
707100               MOVE ISO-PORTUGAL         TO UTMQ-IDLANDX2                 
707200                                                                          
707300               PERFORM S398-SKRIV-W418AA                                  
707400             END-IF                                                       
707500                                                                          
707600             IF DIS1-IDLANDX2 = ISO-MALAYSIA                              
707700               MOVE 'WC1'                TO UTAB-IDPTYP                   
707800               MOVE ANM-IDDISTR          TO UTAB-IDDISTR                  
707900               MOVE ANM-IDKUNDNR         TO UTAB-IDKUNDNR                 
708000               MOVE ANM-IDRAPPNR         TO UTAB-IDRAPPNR                 
708100               MOVE LEV-IDRADNR          TO UTAB-IDRADNR                  
708200               MOVE TXT-TEANMNOT-DLR (1) TO UTAB-TEANMNOT-DLR (1)         
708300               MOVE TXT-TEANMNOT-DLR (2) TO UTAB-TEANMNOT-DLR (2)         
708400               MOVE TXT-TEANMNOT-DLR (3) TO UTAB-TEANMNOT-DLR (3)         
708500               MOVE ISO-MALAYSIA         TO UTMQ-IDLANDX2                 
708600                                                                          
708700               PERFORM S399-SKRIV-W418AB                                  
708800             END-IF                                                       
708900                                                                          
709000             IF DIS1-IDLANDX2 = ISO-THAILAND                              
709100               MOVE 'WC1'                TO UTAC-IDPTYP                   
709200               MOVE ANM-IDDISTR          TO UTAC-IDDISTR                  
709300               MOVE ANM-IDKUNDNR         TO UTAC-IDKUNDNR                 
709400               MOVE ANM-IDRAPPNR         TO UTAC-IDRAPPNR                 
709500               MOVE LEV-IDRADNR          TO UTAC-IDRADNR                  
709600               MOVE TXT-TEANMNOT-DLR (1) TO UTAC-TEANMNOT-DLR (1)         
709700               MOVE TXT-TEANMNOT-DLR (2) TO UTAC-TEANMNOT-DLR (2)         
709800               MOVE TXT-TEANMNOT-DLR (3) TO UTAC-TEANMNOT-DLR (3)         
709900               MOVE ISO-THAILAND         TO UTMQ-IDLANDX2                 
710000                                                                          
710100               PERFORM S400-SKRIV-W418AC                                  
710200             END-IF                                                       
710300                                                                          
710400             IF DIS1-IDLANDX2 = ISO-IRLAND                                
710500               MOVE 'WC1'                TO UTAD-IDPTYP                   
710600               MOVE ANM-IDDISTR          TO UTAD-IDDISTR                  
710700               MOVE ANM-IDKUNDNR         TO UTAD-IDKUNDNR                 
710800               MOVE ANM-IDRAPPNR         TO UTAD-IDRAPPNR                 
710900               MOVE LEV-IDRADNR          TO UTAD-IDRADNR                  
711000               MOVE TXT-TEANMNOT-DLR (1) TO UTAD-TEANMNOT-DLR (1)         
711100               MOVE TXT-TEANMNOT-DLR (2) TO UTAD-TEANMNOT-DLR (2)         
711200               MOVE TXT-TEANMNOT-DLR (3) TO UTAD-TEANMNOT-DLR (3)         
711300               MOVE ISO-IRLAND           TO UTMQ-IDLANDX2                 
711400                                                                          
711500               PERFORM S401-SKRIV-W418AD                                  
711600             END-IF                                                       
711700                                                                          
711800             IF DIS1-IDLANDX2 = ISO-BRASILIEN                             
711900               MOVE 'WC1'                TO UTAE-IDPTYP                   
712000               MOVE ANM-IDDISTR          TO UTAE-IDDISTR                  
712100               MOVE ANM-IDKUNDNR         TO UTAE-IDKUNDNR                 
712200               MOVE ANM-IDRAPPNR         TO UTAE-IDRAPPNR                 
712300               MOVE LEV-IDRADNR          TO UTAE-IDRADNR                  
712400               MOVE TXT-TEANMNOT-DLR (1) TO UTAE-TEANMNOT-DLR (1)         
712500               MOVE TXT-TEANMNOT-DLR (2) TO UTAE-TEANMNOT-DLR (2)         
712600               MOVE TXT-TEANMNOT-DLR (3) TO UTAE-TEANMNOT-DLR (3)         
712700               MOVE ISO-BRASILIEN        TO UTMQ-IDLANDX2                 
712800                                                                          
712900               PERFORM S402-SKRIV-W418AE                                  
713000             END-IF                                                       
713100                                                                          
713200             IF DIS1-IDLANDX2 = ISO-MEXICO                                
713300               MOVE 'WC1'                TO UTAF-IDPTYP                   
713400               MOVE ANM-IDDISTR          TO UTAF-IDDISTR                  
713500               MOVE ANM-IDKUNDNR         TO UTAF-IDKUNDNR                 
713600               MOVE ANM-IDRAPPNR         TO UTAF-IDRAPPNR                 
713700               MOVE LEV-IDRADNR          TO UTAF-IDRADNR                  
713800               MOVE TXT-TEANMNOT-DLR (1) TO UTAF-TEANMNOT-DLR (1)         
713900               MOVE TXT-TEANMNOT-DLR (2) TO UTAF-TEANMNOT-DLR (2)         
714000               MOVE TXT-TEANMNOT-DLR (3) TO UTAF-TEANMNOT-DLR (3)         
714100               MOVE ISO-MEXICO           TO UTMQ-IDLANDX2                 
714200                                                                          
714300               PERFORM S403-SKRIV-W418AF                                  
714400             END-IF                                                       
714500                                                                          
714600             IF DIS1-IDLANDX2 = ISO-TURKIET                               
714700               MOVE 'WC1'                TO UTAH-IDPTYP                   
714800               MOVE ANM-IDDISTR          TO UTAH-IDDISTR                  
714900               MOVE ANM-IDKUNDNR         TO UTAH-IDKUNDNR                 
715000               MOVE ANM-IDRAPPNR         TO UTAH-IDRAPPNR                 
715100               MOVE LEV-IDRADNR          TO UTAH-IDRADNR                  
715200               MOVE TXT-TEANMNOT-DLR (1) TO UTAH-TEANMNOT-DLR (1)         
715300               MOVE TXT-TEANMNOT-DLR (2) TO UTAH-TEANMNOT-DLR (2)         
715400               MOVE TXT-TEANMNOT-DLR (3) TO UTAH-TEANMNOT-DLR (3)         
715500               MOVE ISO-TURKIET          TO UTMQ-IDLANDX2                 
715600                                                                          
715700               PERFORM S404-SKRIV-W418AH                                  
715800             END-IF                                                       
715900                                                                          
716000             IF DIS1-IDLANDX2 = ISO-RYSSLAND                              
716100               MOVE 'WC1'                TO UTAP-IDPTYP                   
716200               MOVE ANM-IDDISTR          TO UTAP-IDDISTR                  
716300               MOVE ANM-IDKUNDNR         TO UTAP-IDKUNDNR                 
716400               MOVE ANM-IDRAPPNR         TO UTAP-IDRAPPNR                 
716500               MOVE LEV-IDRADNR          TO UTAP-IDRADNR                  
716600               MOVE TXT-TEANMNOT-DLR (1) TO UTAP-TEANMNOT-DLR (1)         
716700               MOVE TXT-TEANMNOT-DLR (2) TO UTAP-TEANMNOT-DLR (2)         
716800               MOVE TXT-TEANMNOT-DLR (3) TO UTAP-TEANMNOT-DLR (3)         
716900               MOVE ISO-RYSSLAND         TO UTMQ-IDLANDX2                 
717000                                                                          
717100               PERFORM S406-SKRIV-W418AP                                  
717200             END-IF                                                       
717300                                                                          
717400             IF DIS1-IDLANDX2 = ISO-SYDAFRIKA                             
717500               MOVE 'WC1'                TO UTAQ-IDPTYP                   
717600               MOVE ANM-IDDISTR          TO UTAQ-IDDISTR                  
717700               MOVE ANM-IDKUNDNR         TO UTAQ-IDKUNDNR                 
717800               MOVE ANM-IDRAPPNR         TO UTAQ-IDRAPPNR                 
717900               MOVE LEV-IDRADNR          TO UTAQ-IDRADNR                  
718000               MOVE TXT-TEANMNOT-DLR (1) TO UTAQ-TEANMNOT-DLR (1)         
718100               MOVE TXT-TEANMNOT-DLR (2) TO UTAQ-TEANMNOT-DLR (2)         
718200               MOVE TXT-TEANMNOT-DLR (3) TO UTAQ-TEANMNOT-DLR (3)         
718300               MOVE ISO-SYDAFRIKA        TO UTMQ-IDLANDX2                 
718400                                                                          
718500               PERFORM S407-SKRIV-W418AQ                                  
718600             END-IF                                                       
718700                                                                          
718800             IF DIS1-IDLANDX2 = ISO-KINA-C1                               
718900               MOVE 'WC1'                TO UTC1-IDPTYP                   
719000               MOVE ANM-IDDISTR          TO UTC1-IDDISTR                  
719100               MOVE ANM-IDKUNDNR         TO UTC1-IDKUNDNR                 
719200               MOVE ANM-IDRAPPNR         TO UTC1-IDRAPPNR                 
719300               MOVE LEV-IDRADNR          TO UTC1-IDRADNR                  
719400               MOVE TXT-TEANMNOT-DLR (1) TO UTC1-TEANMNOT-DLR (1)         
719500               MOVE TXT-TEANMNOT-DLR (2) TO UTC1-TEANMNOT-DLR (2)         
719600               MOVE TXT-TEANMNOT-DLR (3) TO UTC1-TEANMNOT-DLR (3)         
719700               MOVE ISO-KINA             TO UTMQ-IDLANDX2                 
719800                                                                          
719900               PERFORM S408-SKRIV-W418C1                                  
720000             END-IF                                                       
720100                                                                          
720200             IF DIS1-IDLANDX2 = ISO-PORTUGAL2                             
720300               MOVE 'WC1'                TO UTAS-IDPTYP                   
720400               MOVE ANM-IDDISTR          TO UTAS-IDDISTR                  
720500               MOVE ANM-IDKUNDNR         TO UTAS-IDKUNDNR                 
720600               MOVE ANM-IDRAPPNR         TO UTAS-IDRAPPNR                 
720700               MOVE LEV-IDRADNR          TO UTAS-IDRADNR                  
720800               MOVE TXT-TEANMNOT-DLR (1) TO UTAS-TEANMNOT-DLR (1)         
720900               MOVE TXT-TEANMNOT-DLR (2) TO UTAS-TEANMNOT-DLR (2)         
721000               MOVE TXT-TEANMNOT-DLR (3) TO UTAS-TEANMNOT-DLR (3)         
721100               MOVE ISO-PORTUGAL         TO UTMQ-IDLANDX2                 
721200                                                                          
721300               PERFORM S409-SKRIV-W418AS                                  
721400             END-IF                                                       
721500                                                                          
721600             IF DIS1-IDLANDX2 = ISO-INDIEN                                
721700*HÄR SKAPAS EN FIL TILL VIPS FÖR INDIEN                                   
721800               MOVE 'WC1'              TO UTIN-IDPTYP                     
721900               MOVE ANM-IDDISTR        TO UTIN-IDDISTR                    
722000               MOVE ANM-IDKUNDNR       TO UTIN-IDKUNDNR                   
722100               MOVE ANM-IDRAPPNR       TO UTIN-IDRAPPNR                   
722200               MOVE LEV-IDRADNR        TO UTIN-IDRADNR                    
722300               MOVE TXT-TEANMNOT-DLR (1) TO UTIN-TEANMNOT-DLR (1)         
722400               MOVE TXT-TEANMNOT-DLR (2) TO UTIN-TEANMNOT-DLR (2)         
722500               MOVE TXT-TEANMNOT-DLR (3) TO UTIN-TEANMNOT-DLR (3)         
722600               MOVE ISO-INDIEN           TO UTMQ-IDLANDX2                 
722700                                                                          
722800               PERFORM S411-SKRIV-W418IN                                  
722900             END-IF                                                       
723000                                                                          
723100             IF DIS1-IDLANDX2 = ISO-TJECKIEN                              
723200               MOVE 'WC1'              TO UTCZ-IDPTYP                     
723300               MOVE ANM-IDDISTR        TO UTCZ-IDDISTR                    
723400               MOVE ANM-IDKUNDNR       TO UTCZ-IDKUNDNR                   
723500               MOVE ANM-IDRAPPNR       TO UTCZ-IDRAPPNR                   
723600               MOVE LEV-IDRADNR        TO UTCZ-IDRADNR                    
723700               MOVE TXT-TEANMNOT-DLR (1) TO UTCZ-TEANMNOT-DLR (1)         
723800               MOVE TXT-TEANMNOT-DLR (2) TO UTCZ-TEANMNOT-DLR (2)         
723900               MOVE TXT-TEANMNOT-DLR (3) TO UTCZ-TEANMNOT-DLR (3)         
724000               MOVE ISO-TJECKIEN         TO UTMQ-IDLANDX2                 
724100                                                                          
724200               PERFORM S412-SKRIV-W418CZ                                  
724300             END-IF                                                       
724400                                                                          
724500             IF DIS1-IDLANDX2 = ISO-UNGERN                                
724600               MOVE 'WC1'              TO UTHU-IDPTYP                     
724700               MOVE ANM-IDDISTR        TO UTHU-IDDISTR                    
724800               MOVE ANM-IDKUNDNR       TO UTHU-IDKUNDNR                   
724900               MOVE ANM-IDRAPPNR       TO UTHU-IDRAPPNR                   
725000               MOVE LEV-IDRADNR        TO UTHU-IDRADNR                    
725100               MOVE TXT-TEANMNOT-DLR (1) TO UTHU-TEANMNOT-DLR (1)         
725200               MOVE TXT-TEANMNOT-DLR (2) TO UTHU-TEANMNOT-DLR (2)         
725300               MOVE TXT-TEANMNOT-DLR (3) TO UTHU-TEANMNOT-DLR (3)         
725400               MOVE ISO-UNGERN           TO UTMQ-IDLANDX2                 
725500                                                                          
725600               PERFORM S413-SKRIV-W418HU                                  
725700             END-IF                                                       
725800           END-EVALUATE                                                   
725900           PERFORM S414-SKRIV-W418MQA                                     
726000        END-IF                                                            
726100     END-IF                                                               
726200     .                                                                    
726300     EJECT                                                                
726400 S46-SKAPA-KN-RAD-BILLIT SECTION.                                         
726500                                                                          
726600     SEARCH ALL DC-LAND                                                   
726700        AT END                                                            
726800           MOVE SPACE          TO W-IDLAND                                
726900        WHEN DCLAND-IDDC (DCLAND-IX) = LEV-IDDC                           
727000           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
727100                               TO W-IDLAND                                
727200     END-SEARCH                                                           
727300                                                                          
727400     IF TKOST-SKRIVNA-FOERUT                                              
727500        MOVE JA                          TO TILLAEGGSKOSTNADER-SW         
727600     END-IF                                                               
727700                                                                          
727800     MOVE '31B'                          TO 31B-IDPTYP                    
727900     MOVE ANM-IDDISTR                    TO 31B-IDDISTR                   
728000                                            TEST-IDDISTR                  
728100     MOVE ANM-IDKUNDNR                   TO 31B-IDKUNDNR                  
728200     MOVE ANM-IDRAPPNR                   TO 31B-IDRAPPNR                  
728300     MOVE ANM-IDUSER-ADM                 TO 31B-IDUSER-ADM                
728400     MOVE ANM-BEANST                     TO 31B-BEANST                    
728500     MOVE LEV-IDDC                       TO 31B-IDDC                      
728600     MOVE LEV-IDDC-RET                   TO 31B-IDDC-RET                  
728700     MOVE LEV-IDARTNR                    TO 31B-IDARTNR                   
728800     MOVE LEV-IDRADNR                    TO 31B-IDRADNR                   
728900     MOVE LEV-KDANMORS                   TO 31B-KDANMORS                  
729000     MOVE LEV-DALEVANM                   TO 31B-DALEVANM                  
729100                                                                          
729200     IF ART-KDSORT = 'SW'                                                 
729300       MOVE 'Y'                          TO 31B-FLSOFT                    
729400     ELSE                                                                 
729500       MOVE 'N'                          TO 31B-FLSOFT                    
729600     END-IF                                                               
729700                                                                          
729800     MOVE WS-FLLSBOK                     TO 31B-FLLSBOK                   
729900     MOVE LEV-BEART-VIPS                 TO 31B-BEART-VIPS                
730000     MOVE ANM-KDVALISO                   TO 31B-KDVALISO                  
730100     MOVE 'N'                            TO 31B-FLFREE                    
730200                                                                          
730300*- OM DET ÄR EN BILLIT-KUND , SKALL W009MOMS-VATKODEN                     
730400*- GÄLLA ENLIGT BOSSE HAMMARIN 2002-04                                    
730500                                                                          
730600     IF DIST79-DEALER-PRICE                                               
730700       MOVE LEV-KDVAT                    TO 31B-KDVAT                     
730800                                                                          
730900**CO JAN.'04                                                              
731000*** ITALIEN SKALL INTE HA MOMS PÅ VISSA RETURKODER                        
731100       MOVE LEV-IDDC                     TO WS-IDDC                       
731200                                                                          
731300       IF SDC-IT OR LDC-IT                                                
731400         IF LEV-KDANMORS = '42' OR '52' OR '62' OR '72' OR                
731500                           '75' OR '82' OR '92' OR '97' OR '98'           
731600           IF ANM-IDDISTR = 1558                                          
731700           OR ANM-IDDISTR = 1578                                          
731800             MOVE 'ID'                   TO 31B-KDVAT                     
731900           ELSE                                                           
732000             MOVE 'IC'                   TO 31B-KDVAT                     
732100           END-IF                                                         
732200         END-IF                                                           
732300       END-IF                                                             
732400**CO JAN.'04                                                              
732500                                                                          
732600*** HOLLAND SKALL HA MOMS PÅ RETURER FÖR ATT VIPS SKICKAR FEL             
732700       IF SDC-NL OR SDC-NL-ET OR LDC-NL                                   
732800         MOVE 'NZ'                     TO 31B-KDVAT                       
732900       END-IF                                                             
733000                                                                          
733100**** TO OVERRIDE SOFTWARE VAT                                             
733200       IF ART-KDSORT = 'SW' AND 31B-FLSOFT = 'Y'                          
733300****   OUTSIDE EU                                                         
733400         IF DIST42-EJ-EU                                                  
733500           MOVE '80'            TO 31B-KDVAT                              
733600****   SOFTWARE FOR RUSSIA SHOULD HAVE 20% VAT                            
733700           IF DIST42-RU-SOFT                                              
733800             MOVE 'RU'          TO 31B-KDVAT                              
733900           END-IF                                                         
734000         ELSE                                                             
734100****   SWEDEN                                                             
734200           IF DIST42-EJ-EU-PLUS-SE                                        
734300             MOVE '21'          TO 31B-KDVAT                              
734400           ELSE                                                           
734500****   EU                                                                 
734600             MOVE '60'          TO 31B-KDVAT                              
734700           END-IF                                                         
734800         END-IF                                                           
734900       END-IF                                                             
735000                                                                          
735100     ELSE                                                                 
735200       PERFORM S46A-SAETT-VATKOD                                          
735300     END-IF                                                               
735400                                                                          
735500     MOVE LEV-IDFAKT                     TO 31B-IDFAKREF                  
735600                                                                          
735700     IF LEV-TIFAKT > ZERO                                                 
735800       MOVE LEV-TIFAKT      TO W-DATUM                                    
735900                                                                          
736000       IF W-DATUM < 500000                                                
736100         MOVE 20                     TO W-DATUM (1:2)                     
736200       ELSE                                                               
736300         IF W-DATUM < 999999                                              
736400           MOVE 19                   TO W-DATUM (1:2)                     
736500         ELSE                                                             
736600           MOVE 99999999             TO W-DATUM                           
736700         END-IF                                                           
736800       END-IF                                                             
736900                                                                          
737000       MOVE W-DATUM                      TO 31B-DAFAKREF                  
737100     ELSE                                                                 
737200       MOVE ZERO                         TO 31B-DAFAKREF                  
737300     END-IF                                                               
737400                                                                          
737500     MOVE LEV-KVLEVANM-BEKR              TO 31B-KVLEVANM                  
737600                                                                          
737700     COMPUTE 31B-KVKREANT = LEV-KVLEVANM-BEKR -                           
737800                             LEV-KVAVV-KVANT   -                          
737900                             LEV-KVAVV-KVAL                               
738000     END-COMPUTE                                                          
738100                                                                          
738200* IF NON-VCC OWNED DEALER AND VIPS IS SENDING AVG COST IN LOC             
738300* CURRENCY THEN MOVE COST OF SALE PRICE TO BILLIT                         
738400* OTHERWISE MOVE PRARTBTO                                                 
738500*                                                                         
738600* TURKEY,THAILAND AND TAIWAN HAVE BEEN CHANGED IN VIPS SO FAR.            
738700* ADD OTHER MARKETS WHEN VIPS CHANGES THEM.                               
738800*                                                                         
738900     IF DIST07-NON-VCC-OWNED                                              
739000     AND (DIST07-TURKEY OR DIST07-THAILAND OR                             
739100          DIST07-TAIWAN OR DIST07-MEXICO   OR DIST07-BRAZIL OR            
739200                           DIST07-S-AFRICA)                               
739300       IF ART-KDSORT = 'SW'                                               
739400          MOVE LEV-PRARTBTO              TO 31B-PRARTBTO                  
739500       ELSE                                                               
739600         IF LEV-PRARTSJK = ZERO                                           
739700           MOVE LEV-PRARTBTO             TO 31B-PRARTBTO                  
739800         ELSE                                                             
739900           MOVE LEV-PRARTSJK             TO 31B-PRARTBTO                  
740000         END-IF                                                           
740100       END-IF                                                             
740200     ELSE                                                                 
740300        MOVE LEV-PRARTBTO                TO 31B-PRARTBTO                  
740400     END-IF                                                               
740500     MOVE LEV-PRARTBTO-LOC               TO 31B-PRARTBTO-LOC              
740600                                                                          
740700     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB2                
740800                                            W-IDDISTR-WDB2-MIN            
740900                                            W-IDDISTR-WDB2-MAX            
741000     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB2               
741100     PERFORM IMS-GET-WLGMTA01-UNIK                                        
741200     IF SEGMENT-SAKNAS                                                    
741300        PERFORM IMS-GET-WLGMTA01                                          
741400     END-IF                                                               
741500                                                                          
741600     MOVE GMT-IDPARTNR                   TO 31B-IDPARTNR                  
741700     MOVE GMT-IDSKYLT                    TO 31B-IDSKYLT                   
741800                                                                          
741900     IF CLAG-IDSTATNR(3) = ZERO                                           
742000       MOVE GEN-IDSTATNR                 TO 31B-IDSTATNR                  
742100     ELSE                                                                 
742200       MOVE CLAG-IDSTATNR(3)             TO 31B-IDSTATNR                  
742300     END-IF                                                               
742400                                                                          
742500     MOVE CLAG-KDARTURS                  TO 31B-KDARTURS                  
742600     MOVE CLAG-VKART                     TO 31B-VKART                     
742700                                                                          
742800     IF DCLAND-CHINA (DCLAND-IX)                                          
742900*    FÖR KINA FÖRSÖKER VI HITTA UNIKA VÄRDEN OM DET GÅR...                
743000                                                                          
743100        MOVE LEV-IDARTNR  TO W-IDARTNR-K7                                 
743200        PERFORM IMS-GU-WDK712                                             
743300        IF SEGMENT-FINNS                                                  
743400          IF LART-KDARTURS > SPACE                                        
743500            MOVE LART-KDARTURS            TO 31B-KDARTURS                 
743600          END-IF                                                          
743700          IF LART-VKART > 0                                               
743800            MOVE LART-VKART               TO 31B-VKART                    
743900          END-IF                                                          
744000        END-IF                                                            
744100     END-IF                                                               
744200                                                                          
744300     IF OKOD-FL-KOD-SOM-BAER-TK  = JA OR                                  
744400        OKOD-FL-INTERNUPPACKNING = JA                                     
744500       IF DIST79-DEALER-PRICE                                             
744600                                                                          
744700*-- ENLIGT KLAS SÅ BETALAS EJ L/C UT TILL ÅF, BARA MELLAN CDC-IMP/        
744800*-- SÄLJBOLAG.......... APRIL 2002.DEALER-PRICE = NOLL-AFFÄR.             
744900                                                                          
745000         MOVE ZERO                       TO 31B-PRLANDCO-RAD              
745100       ELSE                                                               
745200         COMPUTE 31B-PRLANDCO-RAD    ROUNDED =                            
745300         (31B-KVKREANT * LEV-PRARTBTO * ANM-RELANDCO) / 100               
745400       END-IF                                                             
745500     ELSE                                                                 
745600        MOVE ZERO                        TO 31B-PRLANDCO-RAD              
745700     END-IF                                                               
745800                                                                          
745900     IF TILLAEGGSK-SKRIVNA                                                
746000        MOVE +0                          TO 31B-PRFOERS                   
746100                                            31B-PRFRAKT                   
746200                                            31B-PRLEGKST                  
746300     ELSE                                                                 
746400        MOVE ANM-PRFOERS                 TO 31B-PRFOERS                   
746500        MOVE ANM-PRFRAKT                 TO 31B-PRFRAKT                   
746600        MOVE ANM-PRLEGKST                TO 31B-PRLEGKST                  
746700        MOVE JA                          TO TILLAEGGSKOSTNADER-SW         
746800     END-IF                                                               
746900                                                                          
747000     PERFORM S32-SKRIV-W418AI                                             
747100                                                                          
747200     IF LEV-KDANMORS = '74'                                               
747300       CONTINUE                                                           
747400     ELSE                                                                 
747500       PERFORM S36A-SKAPA-RADPOST-ARTSTAT                                 
747600                                                                          
747700       IF BYT16-BYTES OR BYT16-RADIO                                      
747800         IF LEV-KDANMORS = '30' OR '40'                                   
747900            CONTINUE                                                      
748000         ELSE                                                             
748100            IF SKRIV-BYTES-OK                                             
748200              CONTINUE                                                    
748300            ELSE                                                          
748400              PERFORM S36C-SKAPA-RADPOST-BYTES                            
748500            END-IF                                                        
748600         END-IF                                                           
748700       END-IF                                                             
748800     END-IF                                                               
748900     .                                                                    
749000     EJECT                                                                
749100 S46A-SAETT-VATKOD SECTION.                                               
749200*                                                                         
749300* GÖRS ÄNDRING I DENNA SECTION SKA MAN ÄVEN SE IGENOM                     
749400* FÖLJANDE PROGRAM OCH EV. ÄNDRA ÄVEN DÄR                                 
749500* - W41830 !!!!!!!!!!!!!!!!!!!!!!                                         
749600* - W40636 !!!!!!!!!!!!!!!!!!!!!!                                         
749700* - WF0202 !!!!!!!!!!!!!!!!!!!!!!                                         
749800* - W418FX (W.FIXA.COBOL)!!!!!!!!                                         
749900*                                                                         
750000*SO 2011-10-12 VI SÄTTER XX SOM MOMSKOD KINA TILLS.VIDARE ENL.AH          
750100*                                                                         
750200     MOVE JA                 TO MOMS-FRITT                                
750300     MOVE LEV-IDDC           TO WS-IDDC                                   
750400                                MOMS-WS-IDDC                              
750500     PERFORM S70-SAETT-EV-MOMS                                            
750600                                                                          
750700     IF MOMS-FRITT = JA                                                   
750800       IF SDC-IT OR LDC-IT                                                
750900         IF ANM-IDDISTR = 1558                                            
751000         OR ANM-IDDISTR = 1578                                            
751100           MOVE 'ID'           TO 31B-KDVAT                               
751200         ELSE                                                             
751300           MOVE 'IC'           TO 31B-KDVAT                               
751400         END-IF                                                           
751500       ELSE                                                               
751600         IF SDC-NL OR SDC-NL-ET OR LDC-NL                                 
751700           IF ANM-IDDISTR = 1619 OR 1620 OR 1622 OR 1628 OR               
751800                            1678                                          
751900             MOVE 'NZ'         TO 31B-KDVAT                               
752000           ELSE                                                           
752100             IF DIST42-EU                                                 
752200               MOVE 'NI'       TO 31B-KDVAT                               
752300             ELSE                                                         
752400               MOVE 'NJ'       TO 31B-KDVAT                               
752500             END-IF                                                       
752600           END-IF                                                         
752700         ELSE                                                             
752800           IF DDC-DE OR LDC-DE                                            
752900             IF DIST42-EU                                                 
753000               MOVE 'VI'       TO 31B-KDVAT                               
753100             ELSE                                                         
753200               MOVE 'VJ'       TO 31B-KDVAT                               
753300             END-IF                                                       
753400           ELSE                                                           
753500             IF DDC-BE OR LDC-BE                                          
753600               IF DIST34-BELGIEN-DDC OR DIST34-BELGIEN-LDC                
753700                 MOVE 'BD'     TO 31B-KDVAT                               
753800               ELSE                                                       
753900                 IF DIST42-EU                                             
754000                   MOVE 'BQ'   TO 31B-KDVAT                               
754100                 ELSE                                                     
754200                   MOVE 'BX'   TO 31B-KDVAT                               
754300                 END-IF                                                   
754400               END-IF                                                     
754500             ELSE                                                         
754600              IF DDC-FR                                                   
754700                IF DIST34-FRANKRIKE-DDC                                   
754800                  MOVE 'F3'     TO 31B-KDVAT                              
754900                ELSE                                                      
755000                  IF DIST42-EU                                            
755100                    MOVE 'F4'   TO 31B-KDVAT                              
755200                  ELSE                                                    
755300                    MOVE 'F5'   TO 31B-KDVAT                              
755400                  END-IF                                                  
755500                END-IF                                                    
755600              ELSE                                                        
755700*** E'TRACKER 4823800 NEW LDC,HAR EJ FÅTT VATKOD FRÅN EKONOMI,            
755800*** VI FÅR INSTALLERA MED FIX ENLIGT BIRGITTA/PETER D. LDC 2O/3I          
755900               IF LDC-CH                                                  
756000                 IF DIST34-SCHWEIZ-LDC                                    
756100                   MOVE 'XX'     TO 31B-KDVAT                             
756200                 ELSE                                                     
756300                   IF DIST42-EU                                           
756400                     MOVE '70'   TO 31B-KDVAT                             
756500                   ELSE                                                   
756600                     MOVE '90'   TO 31B-KDVAT                             
756700                   END-IF                                                 
756800                 END-IF                                                   
756900               ELSE                                                       
757000                IF DDC-FI OR LDC-FI                                       
757100                 IF DIST34-FINLAND-DDC OR DIST34-FINLAND-LDC              
757200                   MOVE '48'     TO 31B-KDVAT                             
757300                 ELSE                                                     
757400                   IF DIST42-EU                                           
757500                     MOVE '70'   TO 31B-KDVAT                             
757600                   ELSE                                                   
757700                     MOVE '90'   TO 31B-KDVAT                             
757800                   END-IF                                                 
757900                 END-IF                                                   
758000                ELSE                                                      
758100                 IF DDC-PL OR LDC-PL                                      
758200                  IF DIST34-POLAND-DDC OR DIST34-POLAND-LDC               
758300                    MOVE 'P2'     TO 31B-KDVAT                            
758400                  ELSE                                                    
758500                    IF DIST42-EU                                          
758600                      MOVE 'PC'   TO 31B-KDVAT                            
758700                    ELSE                                                  
758800                      MOVE 'PD'   TO 31B-KDVAT                            
758900                    END-IF                                                
759000                  END-IF                                                  
759100                 ELSE                                                     
759200                  IF DDC-AU                                               
759300                   IF DIST34-AUSTRALIA-DDC                                
759400                     MOVE '90'   TO 31B-KDVAT                             
759500                   ELSE                                                   
759600                     IF DIST42-EU                                         
759700                       MOVE '90' TO 31B-KDVAT                             
759800                     ELSE                                                 
759900                       MOVE '90' TO 31B-KDVAT                             
760000                     END-IF                                               
760100                   END-IF                                                 
760200                  ELSE                                                    
760300                   IF DDC-GB OR LDC-GB                                    
760400                     IF DIST34-ENGLAND-DDC                                
760500                     OR DIST34-ENGLAND-LDC                                
760600                     OR DIST34-ENGLAND-SDC                                
760700                       MOVE 'G7' TO 31B-KDVAT                             
760800*** SENDING FROM GB TO EU                                                 
760900                     ELSE                                                 
761000                       IF DIST42-EU                                       
761100                         MOVE 'G9' TO 31B-KDVAT                           
761200                       ELSE                                               
761300                         MOVE '90' TO 31B-KDVAT                           
761400                       END-IF                                             
761500                     END-IF                                               
761600                   ELSE                                                   
761700                     IF DIST42-EU                                         
761800                       IF DIST03-SVERIGE                                  
761900                         MOVE '21' TO 31B-KDVAT                           
762000                       ELSE                                               
762100                         MOVE '70' TO 31B-KDVAT                           
762200                       END-IF                                             
762300                     ELSE                                                 
762400                       MOVE '90'   TO 31B-KDVAT                           
762500                       IF NDC-PACIFIC                                     
762600                       OR XDC-NON-VCC-OWNED                               
762700                         MOVE 'XX' TO 31B-KDVAT                           
762800                         IF (NDC-AU AND DIST34-JAPAN-NDC )                
762900                         OR (NDC-JP AND DIST34-AUSTRALIA-NDC)             
763000                         OR  DIST35-PACIFIC-TRANSFER                      
763100                         OR  DIST35-REFILL-INOM-JP                        
763200                           MOVE '90' TO 31B-KDVAT                         
763300                         END-IF                                           
763400                         IF NDC-TH                                        
763500                           MOVE 'XZ' TO 31B-KDVAT                         
763600                         END-IF                                           
763700                       END-IF                                             
763800                     END-IF                                               
763900                    END-IF                                                
764000                  END-IF                                                  
764100                 END-IF                                                   
764200                END-IF                                                    
764300               END-IF                                                     
764400              END-IF                                                      
764500             END-IF                                                       
764600           END-IF                                                         
764700         END-IF                                                           
764800       END-IF                                                             
764900     ELSE                                                                 
765000                                                                          
765100       EVALUATE TRUE                                                      
765200       WHEN CDC-SE OR LDC-SE                                              
765300            MOVE '21'        TO 31B-KDVAT                                 
765400       WHEN SDC-NL OR SDC-NL-ET OR LDC-NL                                 
765500            MOVE 'NZ'        TO 31B-KDVAT                                 
765600       WHEN DDC-GB OR LDC-GB                                              
765700            MOVE 'G7'        TO 31B-KDVAT                                 
765800       WHEN SDC-ES                                                        
765900            MOVE 'S4'        TO 31B-KDVAT                                 
766000       WHEN SDC-IT OR LDC-IT                                              
766100            MOVE 'I2'        TO 31B-KDVAT                                 
766200       WHEN SDC-AT                                                        
766300            MOVE 'A2'        TO 31B-KDVAT                                 
766400       WHEN NDC-JP                                                        
766500            MOVE 'J4'        TO 31B-KDVAT                                 
766600       WHEN NDC-AU                                                        
766700            MOVE '90'        TO 31B-KDVAT                                 
766800       WHEN XDC-NON-VCC-OWNED                                             
766900            MOVE 'XX'        TO 31B-KDVAT                                 
767000       WHEN DDC-SE                                                        
767100            MOVE '21'        TO 31B-KDVAT                                 
767200       WHEN DDC-NO                                                        
767300            MOVE 'Y1'        TO 31B-KDVAT                                 
767400       WHEN LDC-NO                                                        
767500            MOVE 'Y1'        TO 31B-KDVAT                                 
767600       WHEN DDC-BE OR LDC-BE                                              
767700            MOVE 'BD'        TO 31B-KDVAT                                 
767800       WHEN DDC-DE OR LDC-DE                                              
767900            MOVE 'VE'        TO 31B-KDVAT                                 
768000       WHEN DDC-FR                                                        
768100            MOVE 'F3'        TO 31B-KDVAT                                 
768200       WHEN DDC-FI OR LDC-FI                                              
768300            MOVE '48'        TO 31B-KDVAT                                 
768400       WHEN DDC-AU                                                        
768500            MOVE '90'        TO 31B-KDVAT                                 
768600*      WHEN DDC-HU                                                        
768700*           MOVE '??'        TO 31B-KDVAT                                 
768800       WHEN DDC-PL OR LDC-PL                                              
768900            MOVE 'P2'        TO 31B-KDVAT                                 
769000       WHEN LDC-CH                                                        
769100            MOVE 'XX'        TO 31B-KDVAT                                 
769200       END-EVALUATE                                                       
769300     END-IF                                                               
769400     PERFORM S47A-VAT-ADAPTION                                            
769500     .                                                                    
769600     EJECT                                                                
769700                                                                          
769800 S47A-VAT-ADAPTION  SECTION.                                              
769900**** ADAPT VAT CODES FOR SOFTWARE                                         
770000     IF  ART-KDSORT = 'SW' AND 31B-FLSOFT = 'Y'                           
770100********** OUTSIDE EU                                                     
770200       IF DIST42-EJ-EU                                                    
770300         MOVE '80'              TO 31B-KDVAT                              
770400**** SOFTWARE FOR RUSSIA SHOULD HAVE 20% VAT                              
770500         IF DIST42-RU-SOFT                                                
770600           MOVE 'RU'            TO 31B-KDVAT                              
770700         END-IF                                                           
770800       ELSE                                                               
770900********** SWEDEN                                                         
771000         IF DIST42-EJ-EU-PLUS-SE                                          
771100           MOVE '21'            TO 31B-KDVAT                              
771200         ELSE                                                             
771300********** EU                                                             
771400           MOVE '60'            TO 31B-KDVAT                              
771500         END-IF                                                           
771600       END-IF                                                             
771700     END-IF                                                               
771800**** ADAPT FOR DDGS                                                       
771900     IF GOOD-DDC                                                          
772000       MOVE ANM-IDDISTR                   TO W-IDDISTR-WDB2               
772100                                             W-IDDISTR-WDB2-MIN           
772200                                             W-IDDISTR-WDB2-MAX           
772300       MOVE ANM-IDKUNDNR                  TO W-IDKUNDNR-WDB2              
772400       PERFORM IMS-GET-WLGMTA01-UNIK                                      
772500       IF SEGMENT-SAKNAS                                                  
772600          PERFORM IMS-GET-WLGMTA01                                        
772700       END-IF                                                             
772800                                                                          
772900       MOVE GMT-IDPARTNR                  TO W-WDB1-IDPARTNR              
773000       MOVE GMT-IDFTG                     TO W-WDB1-IDFTG                 
773100       PERFORM IMS-GET-WDB101                                             
773200                                                                          
773300**** RECEIVER COUNTRY SHOULD BE IN EUROPE                                 
773400       MOVE BET-IDLANDX2       TO LANDX2-IDLANDX2                         
773500       IF LANDX2-EU-IDLANDX2                                              
773600**** SENDING COUNTRY SHOULD BE IN EUROPE                                  
773700         MOVE LEV-IDDC         TO LANDX2-IDLANDX2                         
773800         IF LANDX2-EU-IDLANDX2                                            
773900           IF BET-FLDIRVAT = 'J'                                          
774000             IF BET-IDLANDX2 = 'AT'                                       
774100               MOVE 'A2' TO 31B-KDVAT                                     
774200             END-IF                                                       
774300             IF BET-IDLANDX2 = 'BE'                                       
774400               MOVE 'BD' TO 31B-KDVAT                                     
774500             END-IF                                                       
774600             IF BET-IDLANDX2 = 'DE'                                       
774700               MOVE 'VE' TO 31B-KDVAT                                     
774800             END-IF                                                       
774900             IF BET-IDLANDX2 = 'ES'                                       
775000               MOVE 'S4' TO 31B-KDVAT                                     
775100             END-IF                                                       
775200             IF BET-IDLANDX2 = 'FI'                                       
775300               MOVE '48' TO 31B-KDVAT                                     
775400             END-IF                                                       
775500             IF BET-IDLANDX2 = 'FR'                                       
775600               MOVE 'F3' TO 31B-KDVAT                                     
775700             END-IF                                                       
775800             IF BET-IDLANDX2 = 'IT'                                       
775900               MOVE 'IC' TO 31B-KDVAT                                     
776000             END-IF                                                       
776100             IF BET-IDLANDX2 = 'NL'                                       
776200               MOVE 'NZ' TO 31B-KDVAT                                     
776300             END-IF                                                       
776400             IF BET-IDLANDX2 = 'PL'                                       
776500               MOVE 'P2' TO 31B-KDVAT                                     
776600             END-IF                                                       
776700           END-IF                                                         
776800         END-IF                                                           
776900       END-IF                                                             
777000     END-IF                                                               
777100     .                                                                    
777200     EJECT                                                                
777300                                                                          
777400 S50-SKRIV-UPPDATPOST-KVLS SECTION.                                       
777500*     DISPLAY '**** S50-SKRIV-UPPDATPOST-KVLS'                            
777600                                                                          
777700     MOVE '005'                           TO UT34-IDPTYP                  
777800                                                                          
777900     MOVE ANM-IDLEVANM                    TO UT34-IDLEVANM                
778000     MOVE LEV-IDARTNR                     TO UT34-IDARTNR                 
778100     MOVE ZERO                            TO UT34-IDRADNR                 
778200     MOVE LEV-IDDC                        TO UT34-IDDC                    
778300     MOVE SPACE                           TO UT34-FLFARLIG                
778400     MOVE ZERO                            TO UT34-IDKNOTNR                
778500     MOVE SPACE                           TO UT34-KDFAKTYP-KNOT           
778600                                             UT34-KDLEVANM                
778700     MOVE LEV-KVLEVANM-BEKR               TO UT34-KVLEVANM                
778800     MOVE ZERO                            TO UT34-TIKNOTA                 
778900                                             UT34-TIRETILL                
779000                                             UT34-KVRADER-RT              
779100                                             UT34-PRARTBTO                
779200     MOVE ZERO                            TO UT34-KDINVKAT                
779300     MOVE ZERO                            TO UT34-KVJUSTKV                
779400     MOVE ZERO                            TO UT34-TIM-INV                 
779500     MOVE SPACE                           TO UT34-TEINVANM                
779600     MOVE SPACE                           TO UT34-KDARBTYP                
779700     MOVE ZERO                            TO UT34-IDPERSON                
779800     MOVE SPACE                           TO UT34-IDDC-RET                
779900     MOVE ZERO                            TO UT34-IXDCCLEAR               
780000                                                                          
780100     PERFORM S20-SKRIV-W41834                                             
780200     .                                                                    
780300     EJECT                                                                
780400 S51-SKRIV-UPPDAT-POST-KREE SECTION.                                      
780500*    DISPLAY '*** S51-SKRIV-UPPDAT-POST-KREE'                             
780600*                                                                         
780700*** UPPDATERAR WDA211 MED KNOTANR                                         
780800*                                                                         
780900     MOVE '006'                     TO UT34-IDPTYP                        
781000     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
781100     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
781200     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
781300     MOVE SPACE                     TO UT34-IDDC                          
781400     MOVE SPACE                     TO UT34-FLFARLIG                      
781500     EVALUATE TRUE                                                        
781600        WHEN CDC-SE                                                       
781700           MOVE W-IDKNOTNR-CDC      TO UT34-IDKNOTNR                      
781800        WHEN SDC-NL                                                       
781900        WHEN SDC-NL-ET                                                    
782000        WHEN SDC-ES                                                       
782100        WHEN SDC-AT                                                       
782200           MOVE W-IDKNOTNR-SDC      TO UT34-IDKNOTNR                      
782300        WHEN SDC-IT                                                       
782400           IF W-IDKNOTNR-ITL > +0                                         
782500              MOVE W-IDKNOTNR-ITL   TO UT34-IDKNOTNR                      
782600           ELSE                                                           
782700              MOVE W-IDKNOTNR-SDC   TO UT34-IDKNOTNR                      
782800           END-IF                                                         
782900        WHEN NDC-US                                                       
783000           MOVE W-IDKNOTNR-USA      TO UT34-IDKNOTNR                      
783100        WHEN NDC-CA                                                       
783200           MOVE W-IDKNOTNR-CAN      TO UT34-IDKNOTNR                      
783300        WHEN NDC-JP                                                       
783400           MOVE W-IDKNOTNR-JAP      TO UT34-IDKNOTNR                      
783500        WHEN NDC-AU                                                       
783600           MOVE W-IDKNOTNR-AUS      TO UT34-IDKNOTNR                      
783700        WHEN DDC-SE                                                       
783800           MOVE W-IDKNOTNR-SE       TO UT34-IDKNOTNR                      
783900        WHEN DDC-NO                                                       
784000           MOVE W-IDKNOTNR-NO       TO UT34-IDKNOTNR                      
784100        WHEN DDC-BE                                                       
784200           MOVE W-IDKNOTNR-BE       TO UT34-IDKNOTNR                      
784300     END-EVALUATE                                                         
784400     IF CDC-SE OR SDC OR LDC                                              
784500        OR NDC-JP OR NDC-AU OR GOOD-DDC                                   
784600        IF LEV-KDANMORS = '13' OR '23'                                    
784700           IF LEV-PRARTBTO = +0                                           
784800              MOVE +0               TO UT34-IDKNOTNR                      
784900           END-IF                                                         
785000        END-IF                                                            
785100     ELSE                                                                 
785200        IF LEV-KDANMORS = '13' OR '23'                                    
785300           IF LEV-PRARTBTO-LOC = +0                                       
785400              MOVE +0               TO UT34-IDKNOTNR                      
785500           END-IF                                                         
785600        END-IF                                                            
785700     END-IF                                                               
785800     MOVE ZERO                      TO UT34-KDAVVTYP                      
785900     MOVE 'C'                       TO UT34-KDFAKTYP-KNOT                 
786000     MOVE SPACE                     TO UT34-KDLEVANM                      
786100     MOVE ZERO                      TO UT34-KVLEVANM                      
786200     MOVE DAGENS-DATUM              TO UT34-TIKNOTA                       
786300     MOVE ZERO                      TO UT34-TIRETILL                      
786400                                       UT34-KVRADER-RT                    
786500     MOVE ZERO                      TO UT34-PRARTBTO                      
786600     MOVE ZERO                      TO UT34-KDINVKAT                      
786700     MOVE ZERO                      TO UT34-KVJUSTKV                      
786800     MOVE ZERO                      TO UT34-TIM-INV                       
786900     MOVE SPACE                     TO UT34-TEINVANM                      
787000     MOVE SPACE                     TO UT34-KDARBTYP                      
787100     MOVE ZERO                      TO UT34-IDPERSON                      
787200     MOVE SPACE                     TO UT34-IDDC-RET                      
787300     MOVE ZERO                      TO UT34-IXDCCLEAR                     
787400                                                                          
787500     PERFORM S20-SKRIV-W41834                                             
787600     .                                                                    
787700     EJECT                                                                
787800 S52-SKRIV-UPPDAT-POST-KREE SECTION.                                      
787900*    DISPLAY '*** S52-SKRIV-UPPDAT-POST-KREE'                             
788000*                                                                         
788100*** UPPDATERAR WDA211 MED KNOTANR                                         
788200*                                                                         
788300     MOVE '006'                     TO UT34-IDPTYP                        
788400     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
788500     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
788600     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
788700     MOVE SPACE                     TO UT34-IDDC                          
788800     MOVE SPACE                     TO UT34-FLFARLIG                      
788900     MOVE ZERO                      TO UT34-TIKNOTA                       
789000     IF WS-KVRETINL > +0 AND                                              
789100       (LEV-PRARTBTO > +0 OR LEV-PRARTBTO-LOC > 0)                        
789200       EVALUATE TRUE                                                      
789300          WHEN CDC-SE                                                     
789400             MOVE W-IDKNOTNR-CDC    TO UT34-IDKNOTNR                      
789500          WHEN SDC-NL                                                     
789600          WHEN SDC-NL-ET                                                  
789700          WHEN SDC-ES                                                     
789800          WHEN SDC-AT                                                     
789900             MOVE W-IDKNOTNR-SDC    TO UT34-IDKNOTNR                      
790000          WHEN SDC-IT                                                     
790100             IF W-IDKNOTNR-ITL > +0                                       
790200                MOVE W-IDKNOTNR-ITL TO UT34-IDKNOTNR                      
790300             ELSE                                                         
790400                MOVE W-IDKNOTNR-SDC TO UT34-IDKNOTNR                      
790500             END-IF                                                       
790600          WHEN NDC-US                                                     
790700             MOVE W-IDKNOTNR-USA    TO UT34-IDKNOTNR                      
790800          WHEN NDC-CA                                                     
790900             MOVE W-IDKNOTNR-CAN    TO UT34-IDKNOTNR                      
791000          WHEN NDC-JP                                                     
791100             MOVE W-IDKNOTNR-JAP    TO UT34-IDKNOTNR                      
791200          WHEN NDC-AU                                                     
791300             MOVE W-IDKNOTNR-AUS    TO UT34-IDKNOTNR                      
791400          WHEN DDC-SE                                                     
791500             MOVE W-IDKNOTNR-SE     TO UT34-IDKNOTNR                      
791600          WHEN DDC-NO                                                     
791700             MOVE W-IDKNOTNR-NO     TO UT34-IDKNOTNR                      
791800          WHEN DDC-BE                                                     
791900             MOVE W-IDKNOTNR-BE     TO UT34-IDKNOTNR                      
792000       END-EVALUATE                                                       
792100       MOVE DAGENS-DATUM            TO UT34-TIKNOTA                       
792200     END-IF                                                               
792300     MOVE ZERO                      TO UT34-KDAVVTYP                      
792400     MOVE 'C'                       TO UT34-KDFAKTYP-KNOT                 
792500     MOVE SPACE                     TO UT34-KDLEVANM                      
792600     MOVE ZERO                      TO UT34-KVLEVANM                      
792700     MOVE ZERO                      TO UT34-TIRETILL                      
792800                                       UT34-KVRADER-RT                    
792900     MOVE ZERO                      TO UT34-PRARTBTO                      
793000     MOVE ZERO                      TO UT34-KDINVKAT                      
793100     MOVE ZERO                      TO UT34-KVJUSTKV                      
793200     MOVE ZERO                      TO UT34-TIM-INV                       
793300     MOVE SPACE                     TO UT34-TEINVANM                      
793400     MOVE SPACE                     TO UT34-KDARBTYP                      
793500     MOVE ZERO                      TO UT34-IDPERSON                      
793600     MOVE SPACE                     TO UT34-IDDC-RET                      
793700     MOVE ZERO                      TO UT34-IXDCCLEAR                     
793800                                                                          
793900     PERFORM S20-SKRIV-W41834                                             
794000     .                                                                    
794100     EJECT                                                                
794200 S60-RADPOST-EKO-LAB SECTION.                                             
794300*    DISPLAY '*** S60-RADPOST-EKO-LAB'                                    
794400                                                                          
794500     MOVE '720'                           TO 720-IDPTYP                   
794600     IF GOOD-DDC                                                          
794700       MOVE '11'                          TO 720-IDDC                     
794800     ELSE                                                                 
794900       MOVE LEV-IDDC                      TO 720-IDDC                     
795000     END-IF                                                               
795100     MOVE LEV-IDDC                        TO MOMS-WS-IDDC                 
795200                                             WS-IDDC                      
795300                                                                          
795400     MOVE ANM-IDDISTR                     TO 720-IDDISTR                  
795500                                             TEST-IDDISTR                 
795600                                             W-IDDISTR-WDB2               
795700                                             W-IDDISTR-WDB2-MIN           
795800                                             W-IDDISTR-WDB2-MAX           
795900     MOVE ANM-IDKUNDNR                    TO W-IDKUNDNR-WDB2              
796000                                             KUND16-IDKUNDNR              
796100     PERFORM IMS-GET-WLGMTA01-UNIK                                        
796200     IF SEGMENT-SAKNAS                                                    
796300        PERFORM IMS-GET-WLGMTA01                                          
796400     END-IF                                                               
796500                                                                          
796600     IF GMT-FLSAMFAK = JA                                                 
796700        MOVE ZERO                         TO 720-IDKUNDNR                 
796800     ELSE                                                                 
796900        MOVE ANM-IDKUNDNR                 TO 720-IDKUNDNR                 
797000     END-IF                                                               
797100                                                                          
797200     MOVE GMT-IDPARTNR                    TO W-WDB1-IDPARTNR              
797300*    MOVE ANM-IDFTG                       TO W-WDB1-IDFTG                 
797400* FIX NA                                                                  
797500* VI HÄMTAR IDFTG FÖR USA OCH CA FRÅN WDB2 SÅ LÄNGE DE HAR                
797600* IDFTG = 57 I TRANSARNA VI SKICKAR TILL DEM.                             
797700*    IF NDC-NA                                                            
797800       MOVE GMT-IDFTG                     TO W-WDB1-IDFTG                 
797900*    END-IF                               TO W-WDB1-IDFTG                 
798000* FIX NA                                                                  
798100     PERFORM IMS-GET-WDB101                                               
798200                                                                          
798300     EVALUATE TRUE                                                        
798400        WHEN CDC-SE                                                       
798500           MOVE W-IDKNOTNR-CDC            TO 720-IDKNOTNR                 
798600        WHEN SDC-NL                                                       
798700        WHEN SDC-NL-ET                                                    
798800        WHEN SDC-ES                                                       
798900        WHEN SDC-AT                                                       
799000           MOVE W-IDKNOTNR-SDC            TO 720-IDKNOTNR                 
799100        WHEN SDC-IT                                                       
799200           IF W-IDKNOTNR-ITL > +0                                         
799300              MOVE W-IDKNOTNR-ITL         TO 720-IDKNOTNR                 
799400           ELSE                                                           
799500              MOVE W-IDKNOTNR-SDC         TO 720-IDKNOTNR                 
799600           END-IF                                                         
799700        WHEN NDC-US                                                       
799800           MOVE W-IDKNOTNR-USA            TO 720-IDKNOTNR                 
799900        WHEN NDC-CA                                                       
800000           MOVE W-IDKNOTNR-CAN            TO 720-IDKNOTNR                 
800100        WHEN NDC-JP                                                       
800200           MOVE W-IDKNOTNR-JAP            TO 720-IDKNOTNR                 
800300        WHEN NDC-AU                                                       
800400           MOVE W-IDKNOTNR-AUS            TO 720-IDKNOTNR                 
800500        WHEN DDC-SE                                                       
800600           MOVE W-IDKNOTNR-SE             TO 720-IDKNOTNR                 
800700        WHEN DDC-NO                                                       
800800           MOVE W-IDKNOTNR-NO             TO 720-IDKNOTNR                 
800900        WHEN DDC-BE                                                       
801000           MOVE W-IDKNOTNR-BE             TO 720-IDKNOTNR                 
801100     END-EVALUATE                                                         
801200     MOVE ANM-IDRAPPNR                    TO 720-IDRAPPNR                 
801300     MOVE DAGENS-DATUM-SEKEL              TO 720-DAKRENOT                 
801400                                                                          
801500     IF DIST79-DEALER-PRICE                                               
801600       MOVE 1.0                           TO 720-PRKURS                   
801700     ELSE                                                                 
801800       MOVE BET-KDVALISO                  TO CURR-KDVALISO-ROW            
801900                                                                          
802000**** LÄS PRKURS HTYP 9305               *****                             
802100       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
802200       IF CURR-KDSVAR = ' '                                               
802300         CONTINUE                                                         
802400       ELSE                                                               
802500         MOVE 1                           TO CURR-PRKURS-NEW              
802600       END-IF                                                             
802700       MOVE CURR-PRKURS-NEW               TO 720-PRKURS                   
802800     END-IF                                                               
802900                                                                          
803000     IF TKOST-SKRIVNA-FOERUT                                              
803100        MOVE +0                           TO 720-PRFOERS                  
803200                                             720-PRFRAKT                  
803300                                             720-PRLEGKST                 
803400     ELSE                                                                 
803500        MOVE ANM-PRFOERS                  TO 720-PRFOERS                  
803600        MOVE ANM-PRFRAKT                  TO 720-PRFRAKT                  
803700        MOVE ANM-PRLEGKST                 TO 720-PRLEGKST                 
803800     END-IF                                                               
803900     MOVE +0                              TO W009-MOMS-REVAT              
804000     PERFORM S70-SAETT-EV-MOMS                                            
804100     MOVE W009-MOMS-REVAT                 TO 720-REVAT                    
804200     MOVE LEV-IDARTNR                     TO 720-IDARTNR                  
804300     MOVE LEV-KDANMORS                    TO 720-KDANMORS                 
804400     MOVE ART-KDPRODSL                    TO 720-KDPRODSL                 
804500     MOVE CLAG-KDPSLLOC                   TO 720-KDPSLLOC                 
804600     COMPUTE 720-KVKREANT  =                (LEV-KVLEVANM-BEKR -          
804700                                             LEV-KVAVV-KVANT   -          
804800                                             LEV-KVAVV-KVAL)              
804900     IF OKOD-FL-KOD-SOM-BAER-TK  = JA OR                                  
805000        OKOD-FL-INTERNUPPACKNING = JA                                     
805100                                                                          
805200       IF DIST79-DEALER-PRICE                                             
805300         COMPUTE 720-PRLANDCO ROUNDED =                                   
805400         (720-KVKREANT * LEV-PRARTBTO-LOC * ANM-RELANDCO) / 100           
805500       ELSE                                                               
805600         COMPUTE 720-PRLANDCO ROUNDED =                                   
805700         (720-KVKREANT * LEV-PRARTBTO * ANM-RELANDCO) / 100               
805800       END-IF                                                             
805900     END-IF                                                               
806000                                                                          
806100     IF DIST79-DEALER-PRICE                                               
806200       MOVE LEV-PRARTBTO-LOC              TO 720-PRARTNTO                 
806300     ELSE                                                                 
806400       MOVE LEV-PRARTBTO                  TO 720-PRARTNTO                 
806500     END-IF                                                               
806600                                                                          
806700     IF NDC-US OR NDC-CA                                                  
806800        MOVE LEV-IDDC                     TO W-IDDC                       
806900        PERFORM IMS-GU-WDK711                                             
807000        IF SEGMENT-FINNS                                                  
807100           MOVE SLAG-PRAVCOST             TO 720-PRAVCOST                 
807200        ELSE                                                              
807300           MOVE +0                        TO 720-PRAVCOST                 
807400        END-IF                                                            
807500     ELSE                                                                 
807600        MOVE +0                           TO 720-PRAVCOST                 
807700     END-IF                                                               
807800                                                                          
807900     IF NDC-NA                                                            
808000       PERFORM S13-SKRIV-W41833                                           
808100     ELSE                                                                 
808200       IF DIST35-REFILL-NA      OR                                        
808300          DIST35-NA-CDC-RETURN  OR                                        
808400          DIST35-REFILL-NA-JAP  OR                                        
808500          DIST07-USA-RET-DISCR  OR                                        
808600          DIST07-CAN-RET-DISCR  OR                                        
808700         (DIST18-SCRAP-NDC-QUAL AND KUND16-NDC-SKROT)                     
808800         PERFORM S13-SKRIV-W41833                                         
808900       END-IF                                                             
809000     END-IF                                                               
809100     .                                                                    
809200     EJECT                                                                
809300 S65-UPPD-INVENTERING SECTION.                                            
809400*     DISPLAY '**** S65-UPPD-INVENTERING '                                
809500     PERFORM IMS-GU-INVA-INVA01                                           
809600     IF SEGMENT-SAKNAS                                                    
809700       PERFORM S65A-SKAPA-UPD-POST-INV-ROT                                
809800                                                                          
809900       MOVE LEV-IDDC                      TO INV-IDDC                     
810000       MOVE +4                            TO INV-KDINVKAT                 
810100       MOVE +0                            TO INV-KVJUSTKV                 
810200       MOVE SPACE                         TO INV-TEINVANM                 
810300       MOVE SPACE                         TO WS-KOMMENTAR                 
810400*--- INV-K-  = FÄLT I WORKING-STORAGE                                     
810500       MOVE LEV-KDANMORS                  TO INV-K-KDANMORS               
810600       MOVE LEV-KVLEVANM                  TO INV-K-KVLEVANM               
810700       MOVE ANM-IDDISTR                   TO INV-K-IDDISTR                
810800       MOVE ANM-IDKUNDNR                  TO INV-K-IDKUNDNR               
810900       MOVE WS-KOMMENTAR                  TO INV-TEINVANM                 
811000       PERFORM S65B-SKAPA-UPD-POST-INV-RAD                                
811100     ELSE                                                                 
811200**** SÖK OM DET FINNS NÅGON EJ BEHANDLAD KATEGORI      ***                
811300**** OM EJ, LÄGG UPP EN NY KATEGORI 4                  ***                
811400       MOVE LEV-IDDC                      TO IDDC-SEARCH-MIN              
811500                                             IDDC-SEARCH-MAX              
811600       MOVE NEJ                           TO INV-FINNS                    
811700       PERFORM IMS-GNP-INVA-INVA11                                        
811800       PERFORM UNTIL SEGMENT-SAKNAS                                       
811900         IF INV-FLINVBEH = NEJ                                            
812000           MOVE JA                        TO INV-FINNS                    
812100         END-IF                                                           
812200         PERFORM IMS-GNP-INVA-INVA11                                      
812300       END-PERFORM                                                        
812400                                                                          
812500       IF INV-FINNS = NEJ                                                 
812600**** ALLA KATEGORIER PÅ DENNA ART/DC HAR FLINVBEH = JA ***                
812700         MOVE LEV-IDDC                    TO INV-IDDC                     
812800         MOVE +4                          TO INV-KDINVKAT                 
812900         MOVE +0                          TO INV-KVJUSTKV                 
813000         MOVE SPACE                       TO INV-TEINVANM                 
813100         MOVE SPACE                       TO WS-KOMMENTAR                 
813200         MOVE LEV-KDANMORS                TO INV-K-KDANMORS               
813300         MOVE LEV-KVLEVANM                TO INV-K-KVLEVANM               
813400         MOVE ANM-IDDISTR                 TO INV-K-IDDISTR                
813500         MOVE ANM-IDKUNDNR                TO INV-K-IDKUNDNR               
813600         MOVE WS-KOMMENTAR                TO INV-TEINVANM                 
813700         PERFORM S65B-SKAPA-UPD-POST-INV-RAD                              
813800       END-IF                                                             
813900     END-IF                                                               
814000     .                                                                    
814100     EJECT                                                                
814200 S65A-SKAPA-UPD-POST-INV-ROT SECTION.                                     
814300*    DISPLAY '*** S65A-SKAPA-UPD-POST-INV-ROT '                           
814400                                                                          
814500     MOVE '007'                           TO UT34-IDPTYP                  
814600     MOVE ANM-IDLEVANM                    TO UT34-IDLEVANM                
814700     MOVE LEV-IDARTNR                     TO UT34-IDARTNR                 
814800     MOVE ZERO                            TO UT34-IDRADNR                 
814900     MOVE SPACE                           TO UT34-IDDC                    
815000     MOVE SPACE                           TO UT34-FLFARLIG                
815100     MOVE ZERO                            TO UT34-IDKNOTNR                
815200     MOVE ZERO                            TO UT34-KDAVVTYP                
815300     MOVE SPACE                           TO UT34-KDFAKTYP-KNOT           
815400     MOVE SPACE                           TO UT34-KDLEVANM                
815500     MOVE ZERO                            TO UT34-KVLEVANM                
815600     MOVE ZERO                            TO UT34-PRARTBTO                
815700     MOVE ZERO                            TO UT34-TIKNOTA                 
815800     MOVE ZERO                            TO UT34-TIRETILL                
815900                                             UT34-KVRADER-RT              
816000     MOVE ZERO                            TO UT34-KDINVKAT                
816100     MOVE ZERO                            TO UT34-KVJUSTKV                
816200     MOVE ZERO                            TO UT34-TIM-INV                 
816300     MOVE SPACE                           TO UT34-TEINVANM                
816400     MOVE SPACE                           TO UT34-KDARBTYP                
816500     MOVE ZERO                            TO UT34-IDPERSON                
816600     MOVE SPACE                           TO UT34-IDDC-RET                
816700     MOVE ZERO                            TO UT34-IXDCCLEAR               
816800                                                                          
816900**** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                              
817000     PERFORM IMS-GU-ARTC01                                                
817100     IF ART-KDSORT = 'SW'                                                 
817200       CONTINUE                                                           
817300     ELSE                                                                 
817400       PERFORM S20-SKRIV-W41834                                           
817500     END-IF                                                               
817600     .                                                                    
817700     EJECT                                                                
817800 S65B-SKAPA-UPD-POST-INV-RAD SECTION.                                     
817900*    DISPLAY '*** S65B-SKAPA-UPD-POST-INV-RAD'                            
818000                                                                          
818100     MOVE '008'                           TO UT34-IDPTYP                  
818200     MOVE ANM-IDLEVANM                    TO UT34-IDLEVANM                
818300     MOVE LEV-IDARTNR                     TO UT34-IDARTNR                 
818400     MOVE ZERO                            TO UT34-IDRADNR                 
818500     MOVE LEV-IDDC                        TO UT34-IDDC                    
818600     MOVE SPACE                           TO UT34-FLFARLIG                
818700     MOVE ZERO                            TO UT34-IDKNOTNR                
818800     MOVE ZERO                            TO UT34-KDAVVTYP                
818900     MOVE SPACE                           TO UT34-KDFAKTYP-KNOT           
819000     MOVE SPACE                           TO UT34-KDLEVANM                
819100     MOVE ZERO                            TO UT34-KVLEVANM                
819200     MOVE ZERO                            TO UT34-PRARTBTO                
819300     MOVE ZERO                            TO UT34-TIKNOTA                 
819400     MOVE ZERO                            TO UT34-TIRETILL                
819500                                             UT34-KVRADER-RT              
819600     MOVE +4                              TO UT34-KDINVKAT                
819700     MOVE +0                              TO UT34-KVJUSTKV                
819800     MOVE DAGENS-DATUM                    TO UT34-TIM-INV                 
819900     MOVE SPACE                           TO UT34-TEINVANM                
820000                                                                          
820100     MOVE SPACE                           TO WS-KOMMENTAR                 
820200     MOVE LEV-KDANMORS                    TO INV-K-KDANMORS               
820300     MOVE LEV-KVLEVANM                    TO INV-K-KVLEVANM               
820400     MOVE ANM-IDDISTR                     TO INV-K-IDDISTR                
820500     MOVE ANM-IDKUNDNR                    TO INV-K-IDKUNDNR               
820600     MOVE WS-KOMMENTAR                    TO UT34-TEINVANM                
820700     MOVE SPACE                           TO UT34-KDARBTYP                
820800     MOVE ZERO                            TO UT34-IDPERSON                
820900     MOVE SPACE                           TO UT34-IDDC-RET                
821000     MOVE ZERO                            TO UT34-IXDCCLEAR               
821100                                                                          
821200**** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                              
821300     IF ART-KDSORT = 'SW'                                                 
821400       CONTINUE                                                           
821500     ELSE                                                                 
821600       PERFORM S20-SKRIV-W41834                                           
821700     END-IF                                                               
821800     .                                                                    
821900     EJECT                                                                
822000 S66-KOLLA-OK-ORDERKLASS SECTION.                                         
822100***- OM KLASS 1 INTE ÄR OK FÖR DC'T SÅ FASTNAR ORDERN PÅ DISP.            
822200                                                                          
822300     MOVE NEJ        TO KLASS-1-SW                                        
822400                        KLASS-0-SW                                        
822500     MOVE +1         TO WDB2-IX                                           
822600     PERFORM UNTIL KLASS-1-OK OR WDB2-IX > IX-DCCLEAR-MAX OR              
822700                   GMT-IDDC-DAY (WDB2-IX) = SPACE                         
822800       IF LEV-IDDC = GMT-IDDC-DAY (WDB2-IX)                               
822900         MOVE JA     TO KLASS-1-SW                                        
823000       END-IF                                                             
823100       ADD +1        TO WDB2-IX                                           
823200     END-PERFORM                                                          
823300                                                                          
823400     IF KLASS-1-OK                                                        
823500       CONTINUE                                                           
823600     ELSE                                                                 
823700       MOVE +1         TO WDB2-IX                                         
823800       PERFORM UNTIL KLASS-0-OK OR WDB2-IX > IX-DCCLEAR-MAX OR            
823900                     GMT-IDDC-VOR (WDB2-IX) = SPACE                       
824000         IF LEV-IDDC = GMT-IDDC-VOR (WDB2-IX)                             
824100           MOVE JA     TO KLASS-0-SW                                      
824200         END-IF                                                           
824300         ADD +1        TO WDB2-IX                                         
824400       END-PERFORM                                                        
824500     END-IF                                                               
824600     .                                                                    
824700     EJECT                                                                
824800 S70-SAETT-EV-MOMS SECTION.                                               
824900                                                                          
825000     IF OKOD-FL-TF = NEJ                                                  
825100        MOVE ANM-IDDISTR                 TO TEST-IDDISTR                  
825200        IF MOMS-CDC-SE OR MOMS-DDC-SE OR                                  
825300           MOMS-LDC-SE                                                    
825400           IF DIST03-SVERIGE OR DIST34-SWEDEN-LDC                         
825500              IF ( ANM-IDDISTR < 100 ) AND MOMS-CDC-SE                    
825600                 PERFORM S71A-SAETT-MOMS-SE                               
825700              ELSE                                                        
825800                 PERFORM S71B-SAETT-MOMS-SE                               
825900              END-IF                                                      
826000           END-IF                                                         
826100        ELSE                                                              
826200          IF DIST34-HOLLAND-SDC AND                                       
826300                   (MOMS-SDC-NL OR MOMS-LDC-NL)                           
826400             PERFORM S72-SAETT-MOMS-NL                                    
826500          ELSE                                                            
826600            IF DIST34-ENGLAND-SDC AND MOMS-LDC-GB                         
826700                PERFORM S74-SAETT-MOMS-GB                                 
826800            ELSE                                                          
826900              IF DIST34-SPANIEN-SDC AND MOMS-SDC-ES                       
827000                PERFORM S75-SAETT-MOMS-ES                                 
827100              ELSE                                                        
827200                IF DIST34-ITALIEN-SDC AND (MOMS-SDC-IT OR                 
827300                          MOMS-LDC-IT)                                    
827400                  PERFORM S76-SAETT-MOMS-IT                               
827500                ELSE                                                      
827600                  IF DIST34-AUSTRIA-SDC AND MOMS-SDC-AT                   
827700                    PERFORM S77-SAETT-MOMS-AT                             
827800                  ELSE                                                    
827900                    IF DIST34-JAPAN-NDC AND MOMS-NDC-JP                   
828000                     PERFORM S78-SAETT-MOMS-JP                            
828100                    ELSE                                                  
828200                      IF DIST34-NORGE-DDC AND MOMS-DDC-NO                 
828300                       PERFORM S79A-SAETT-MOMS-NO                         
828400                      ELSE                                                
828500                        IF DIST34-TYSKLAND-DDC AND MOMS-LDC-DE            
828600                          PERFORM S79-SAETT-MOMS-DE                       
828700                        ELSE                                              
828800                          IF DIST34-SCHWEIZ-LDC AND MOMS-LDC-CH           
828900                            PERFORM S81-SAETT-MOMS-CH                     
829000                          ELSE                                            
829100                            IF DIST34-BELGIEN-LDC AND MOMS-LDC-BE         
829200                              PERFORM S83-SAETT-MOMS-BE                   
829300                            END-IF                                        
829400                          END-IF                                          
829500                        END-IF                                            
829600                      END-IF                                              
829700                    END-IF                                                
829800                  END-IF                                                  
829900                END-IF                                                    
830000              END-IF                                                      
830100            END-IF                                                        
830200          END-IF                                                          
830300        END-IF                                                            
830400     END-IF                                                               
830500     .                                                                    
830600     EJECT                                                                
830700 S71A-SAETT-MOMS-SE SECTION.                                              
830800                                                                          
830900     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
831000                                            W-IDDC-WDB3-DEF               
831100     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
831200                                            W-IDDISTR-WDB3-DEF            
831300     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
831400     PERFORM IMS-GU-WLGMTB01                                              
831500                                                                          
831600     IF DC-KDMOMSIN = +2                                                  
831700*- - - - - -  EJ KREDITERA MOMS                                           
831800        CONTINUE                                                          
831900     ELSE                                                                 
832000        MOVE NEJ  TO MOMS-FRITT                                           
832100                                                                          
832200        MOVE 'SE'                        TO W009-MOMS-IDLANDX2            
832300        MOVE '01'                        TO W009-MOMS-KDVAT               
832400        CALL W009MOMS USING W009-MOMS-W009MOMS                            
832500                                                                          
832600        IF W009-MOMS-KDSVAR NOT = SPACE                                   
832700           DISPLAY                                                        
832800           '*** W4183000 - W009MOMS SAKNAS***'                            
832900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
833000        END-IF                                                            
833100     END-IF                                                               
833200     .                                                                    
833300     EJECT                                                                
833400 S71B-SAETT-MOMS-SE SECTION.                                              
833500                                                                          
833600     MOVE NEJ  TO MOMS-FRITT                                              
833700                                                                          
833800     MOVE 'SE'                           TO W009-MOMS-IDLANDX2            
833900     MOVE '01'                           TO W009-MOMS-KDVAT               
834000     CALL W009MOMS USING W009-MOMS-W009MOMS                               
834100                                                                          
834200     IF W009-MOMS-KDSVAR NOT = SPACE                                      
834300        DISPLAY                                                           
834400        '*** W4183000 - W009MOMS SAKNAS***'                               
834500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
834600     END-IF                                                               
834700     .                                                                    
834800     EJECT                                                                
834900 S72-SAETT-MOMS-NL SECTION.                                               
835000                                                                          
835100     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
835200                                            W-IDDC-WDB3-DEF               
835300     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
835400                                            W-IDDISTR-WDB3-DEF            
835500     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
835600     PERFORM IMS-GU-WLGMTB01                                              
835700                                                                          
835800     IF DC-KDMOMSIN = +2                                                  
835900*- - - - - -  EJ KREDITERA MOMS                                           
836000       CONTINUE                                                           
836100     ELSE                                                                 
836200       MOVE NEJ  TO MOMS-FRITT                                            
836300                                                                          
836400       MOVE 'NL'                         TO W009-MOMS-IDLANDX2            
836500       MOVE '01'                         TO W009-MOMS-KDVAT               
836600       CALL W009MOMS USING W009-MOMS-W009MOMS                             
836700                                                                          
836800       IF W009-MOMS-KDSVAR NOT = SPACE                                    
836900          DISPLAY                                                         
837000          '*** W4183000 - W009MOMS SAKNAS***'                             
837100          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
837200       END-IF                                                             
837300     END-IF                                                               
837400     .                                                                    
837500     EJECT                                                                
837600 S74-SAETT-MOMS-GB SECTION.                                               
837700                                                                          
837800     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
837900                                            W-IDDC-WDB3-DEF               
838000     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
838100                                            W-IDDISTR-WDB3-DEF            
838200     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
838300     PERFORM IMS-GU-WLGMTB01                                              
838400                                                                          
838500     IF DC-KDMOMSIN = +2                                                  
838600*- - - - - -  EJ KREDITERA MOMS                                           
838700       CONTINUE                                                           
838800     ELSE                                                                 
838900       MOVE NEJ  TO MOMS-FRITT                                            
839000                                                                          
839100       MOVE 'GB'                         TO W009-MOMS-IDLANDX2            
839200       MOVE '01'                         TO W009-MOMS-KDVAT               
839300       CALL W009MOMS USING W009-MOMS-W009MOMS                             
839400                                                                          
839500       IF W009-MOMS-KDSVAR NOT = SPACE                                    
839600          DISPLAY                                                         
839700          '*** W4183000 - W009MOMS SAKNAS***'                             
839800          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
839900       END-IF                                                             
840000     END-IF                                                               
840100     .                                                                    
840200     EJECT                                                                
840300 S75-SAETT-MOMS-ES SECTION.                                               
840400                                                                          
840500     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
840600                                            W-IDDC-WDB3-DEF               
840700     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
840800                                            W-IDDISTR-WDB3-DEF            
840900     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
841000     PERFORM IMS-GU-WLGMTB01                                              
841100                                                                          
841200     IF DC-KDMOMSIN = +2                                                  
841300*- - - - - - EJ KREDITERA MOMS                                            
841400       CONTINUE                                                           
841500     ELSE                                                                 
841600       MOVE NEJ  TO MOMS-FRITT                                            
841700                                                                          
841800       MOVE 'ES'                         TO W009-MOMS-IDLANDX2            
841900       MOVE '01'                         TO W009-MOMS-KDVAT               
842000       CALL W009MOMS USING W009-MOMS-W009MOMS                             
842100                                                                          
842200       IF W009-MOMS-KDSVAR NOT = SPACE                                    
842300          DISPLAY                                                         
842400          '*** W4183000 - W009MOMS SAKNAS***'                             
842500          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
842600       END-IF                                                             
842700     END-IF                                                               
842800     .                                                                    
842900     EJECT                                                                
843000 S76-SAETT-MOMS-IT SECTION.                                               
843100                                                                          
843200     IF SKRIV-MOMS-ITL = NEJ                                              
843300*** ITALIEN SKALL INTE HA MOMS PÅ VISSA KODER                             
843400        CONTINUE                                                          
843500     ELSE                                                                 
843600        MOVE MOMS-WS-IDDC                TO W-IDDC-WDB3                   
843700                                            W-IDDC-WDB3-DEF               
843800        MOVE ANM-IDDISTR                 TO W-IDDISTR-WDB3                
843900                                            W-IDDISTR-WDB3-DEF            
844000        MOVE ANM-IDKUNDNR                TO W-IDKUNDNR-WDB3               
844100        PERFORM IMS-GU-WLGMTB01                                           
844200                                                                          
844300        IF DC-KDMOMSIN = +2                                               
844400*-    - - - - - EJ KREDITERA MOMS                                         
844500          CONTINUE                                                        
844600        ELSE                                                              
844700          MOVE NEJ  TO MOMS-FRITT                                         
844800                                                                          
844900          MOVE 'IT'                      TO W009-MOMS-IDLANDX2            
845000          MOVE '01'                      TO W009-MOMS-KDVAT               
845100          CALL W009MOMS USING W009-MOMS-W009MOMS                          
845200                                                                          
845300          IF W009-MOMS-KDSVAR NOT = SPACE                                 
845400             DISPLAY                                                      
845500             '*** W4183000 - W009MOMS SAKNAS***'                          
845600             CALL ABEND USING RKOD-ABEND-UTAN-DUMP                        
845700          END-IF                                                          
845800        END-IF                                                            
845900     END-IF                                                               
846000     .                                                                    
846100     EJECT                                                                
846200 S77-SAETT-MOMS-AT SECTION.                                               
846300                                                                          
846400     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
846500                                            W-IDDC-WDB3-DEF               
846600     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
846700                                            W-IDDISTR-WDB3-DEF            
846800     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
846900     PERFORM IMS-GU-WLGMTB01                                              
847000                                                                          
847100     IF DC-KDMOMSIN = +2                                                  
847200*- - - - - - EJ KREDITERA MOMS                                            
847300       CONTINUE                                                           
847400     ELSE                                                                 
847500       MOVE NEJ  TO MOMS-FRITT                                            
847600                                                                          
847700       MOVE 'AT'                         TO W009-MOMS-IDLANDX2            
847800       MOVE '01'                         TO W009-MOMS-KDVAT               
847900       CALL W009MOMS USING W009-MOMS-W009MOMS                             
848000                                                                          
848100       IF W009-MOMS-KDSVAR NOT = SPACE                                    
848200          DISPLAY                                                         
848300          '*** W4183000 - W009MOMS SAKNAS***'                             
848400          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
848500       END-IF                                                             
848600     END-IF                                                               
848700     .                                                                    
848800     EJECT                                                                
848900 S78-SAETT-MOMS-JP SECTION.                                               
849000                                                                          
849100     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
849200                                            W-IDDC-WDB3-DEF               
849300     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
849400                                            W-IDDISTR-WDB3-DEF            
849500     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
849600     PERFORM IMS-GU-WLGMTB01                                              
849700                                                                          
849800     IF DC-KDMOMSIN = +2                                                  
849900*- - - - - - EJ KREDITERA MOMS                                            
850000       CONTINUE                                                           
850100     ELSE                                                                 
850200       MOVE NEJ  TO MOMS-FRITT                                            
850300                                                                          
850400       MOVE 'JP'                         TO W009-MOMS-IDLANDX2            
850500       MOVE '01'                         TO W009-MOMS-KDVAT               
850600       CALL W009MOMS USING W009-MOMS-W009MOMS                             
850700                                                                          
850800       IF W009-MOMS-KDSVAR NOT = SPACE                                    
850900          DISPLAY                                                         
851000          '*** W4183000 - W009MOMS SAKNAS***'                             
851100          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
851200       END-IF                                                             
851300     END-IF                                                               
851400     .                                                                    
851500     EJECT                                                                
851600 S79-SAETT-MOMS-DE SECTION.                                               
851700                                                                          
851800     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
851900                                            W-IDDC-WDB3-DEF               
852000     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
852100                                            W-IDDISTR-WDB3-DEF            
852200     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
852300     PERFORM IMS-GU-WLGMTB01                                              
852400                                                                          
852500     IF DC-KDMOMSIN = +2                                                  
852600*- - - - - -  EJ KREDITERA MOMS                                           
852700       CONTINUE                                                           
852800     ELSE                                                                 
852900       MOVE NEJ  TO MOMS-FRITT                                            
853000                                                                          
853100       MOVE 'DE'                         TO W009-MOMS-IDLANDX2            
853200       MOVE '01'                         TO W009-MOMS-KDVAT               
853300       CALL W009MOMS USING W009-MOMS-W009MOMS                             
853400                                                                          
853500       IF W009-MOMS-KDSVAR NOT = SPACE                                    
853600          DISPLAY                                                         
853700          '*** W4183000 - W009MOMS SAKNAS***'                             
853800          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
853900       END-IF                                                             
854000     END-IF                                                               
854100     .                                                                    
854200     EJECT                                                                
854300 S79A-SAETT-MOMS-NO SECTION.                                              
854400                                                                          
854500     MOVE NEJ  TO MOMS-FRITT                                              
854600                                                                          
854700     MOVE 'NO'                         TO W009-MOMS-IDLANDX2              
854800     MOVE '01'                         TO W009-MOMS-KDVAT                 
854900     CALL W009MOMS USING W009-MOMS-W009MOMS                               
855000                                                                          
855100     IF W009-MOMS-KDSVAR NOT = SPACE                                      
855200        DISPLAY                                                           
855300        '*** W4183000 - W009MOMS SAKNAS***'                               
855400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
855500     END-IF                                                               
855600     .                                                                    
855700     EJECT                                                                
855800 S80-SKAPA-RKD-POST SECTION.                                              
855900*                            *** SKAPAR TRANS TILL NOAC MED               
856000*                            *** INFO OM ÄNDRADE ELLER AVVISADE           
856100*                            *** RADER OCH ANULLERADE LEVERANS-           
856200*                            *** ANMÄRKNINGSRADER                         
856300     IF NOT LEV-KDANMORS = '74'                                           
856400                                                                          
856500       MOVE SPACE                     TO RKD-AREA                         
856600       MOVE 'RKD'                     TO RKD-IDPTYP                       
856700       MOVE ANM-IDDISTR               TO RKD-IDDISTR                      
856800                                         TEST-IDDISTR                     
856900       MOVE ANM-IDKUNDNR              TO RKD-IDKUNDNR                     
857000       MOVE LEV-IDDC                  TO RKD-IDDC                         
857100       MOVE ANM-IDRAPPNR              TO RKD-IDRAPPNR                     
857200       MOVE LEV-IDORDNR7              TO RKD-IDORDNR                      
857300       MOVE LEV-IDKOLLI               TO RKD-IDKOLLI                      
857400       MOVE LEV-IDARTNR               TO RKD-IDARTNR                      
857500       MOVE ART-REKSIFFR              TO RKD-REKSIFFR                     
857600       MOVE LEV-IDRADNR               TO RKD-IDRADNR                      
857700                                                                          
857800       IF LEV-KDKREBEH = 'ANN'                                            
857900          MOVE 'DEL'                  TO RKD-KDKREBEH                     
858000          MOVE ZERO                   TO RKD-KVLEVANM                     
858100       ELSE                                                               
858200          MOVE LEV-KDKREBEH           TO RKD-KDKREBEH                     
858300                                                                          
858400**   OM ANTALSAVVIKELSE/KVALITETSAVVIKELSE SKA RETURNERAT                 
858500**   GODKÄNT ANTAL LÄGGAS I RKD-KVLEVANM.                                 
858600          IF W-BOKST = 'D'                                                
858700             MOVE WS-KVRETINL         TO RKD-KVLEVANM                     
858800          ELSE                                                            
858900             MOVE LEV-KVLEVANM-BEKR   TO RKD-KVLEVANM                     
859000          END-IF                                                          
859100       END-IF                                                             
859200                                                                          
859300       MOVE LEV-KDANMORS              TO RKD-KDANMORS                     
859400                                                                          
859500*- KODERNA 25,26,27,28 FÅR BARA FINNS INOM PULS.BYTS TILL "GAMLA"         
859600*- KODERNA 20,21,22,23 VID RETUR TILL VIPS.                               
859700       EVALUATE RKD-KDANMORS                                              
859800         WHEN 25                                                          
859900            MOVE 20                   TO RKD-KDANMORS                     
860000         WHEN 26                                                          
860100            MOVE 21                   TO RKD-KDANMORS                     
860200         WHEN 27                                                          
860300            MOVE 22                   TO RKD-KDANMORS                     
860400         WHEN 28                                                          
860500            MOVE 23                   TO RKD-KDANMORS                     
860600       END-EVALUATE                                                       
860700                                                                          
860800       IF LEV-FLSKROT = NEJ                                               
860900          MOVE 0                      TO RKD-FLSKROT                      
861000       ELSE                                                               
861100          MOVE 1                      TO RKD-FLSKROT                      
861200       END-IF                                                             
861300       MOVE ANM-KDVALISO              TO RKD-KDVALISO                     
861400                                                                          
861500       IF DIST79-DEALER-PRICE                                             
861600         MOVE ZERO                    TO RKD-PRARTBTO                     
861700         MOVE LEV-PRARTBTO-LOC        TO RKD-PRARTBTO-LOC                 
861800                                                                          
861900         COMPUTE RKD-SULNELOC = LEV-PRARTBTO-LOC  *                       
862000                                RKD-KVLEVANM                              
862100         MOVE LEV-PRARTSTD            TO RKD-PRARTSTD                     
862200         MOVE LEV-PRARTSJK            TO RKD-PRARTSJK                     
862300       ELSE                                                               
862400         MOVE LEV-PRARTBTO            TO RKD-PRARTBTO                     
862500         MOVE +0                      TO RKD-PRARTBTO-LOC                 
862600         MOVE +0                      TO RKD-SULNELOC                     
862700         MOVE +0                      TO RKD-PRARTSTD                     
862800         MOVE +0                      TO RKD-PRARTSJK                     
862900       END-IF                                                             
863000                                                                          
863100       PERFORM S18-SKRIV-W4183A                                           
863200                                                                          
863300     END-IF                                                               
863400     .                                                                    
863500     EJECT                                                                
863600 S81-SAETT-MOMS-CH SECTION.                                               
863700                                                                          
863800     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
863900                                            W-IDDC-WDB3-DEF               
864000     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
864100                                            W-IDDISTR-WDB3-DEF            
864200     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
864300     PERFORM IMS-GU-WLGMTB01                                              
864400                                                                          
864500     IF DC-KDMOMSIN = +2                                                  
864600*- - - - - -  EJ KREDITERA MOMS                                           
864700       CONTINUE                                                           
864800     ELSE                                                                 
864900       MOVE NEJ  TO MOMS-FRITT                                            
865000                                                                          
865100       MOVE 'CH'                         TO W009-MOMS-IDLANDX2            
865200       MOVE '01'                         TO W009-MOMS-KDVAT               
865300       CALL W009MOMS USING W009-MOMS-W009MOMS                             
865400                                                                          
865500       IF W009-MOMS-KDSVAR NOT = SPACE                                    
865600          DISPLAY                                                         
865700          '*** W4183000 - W009MOMS SAKNAS***'                             
865800          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
865900       END-IF                                                             
866000     END-IF                                                               
866100     .                                                                    
866200     EJECT                                                                
866300 S82-SAETT-MOMS-FI SECTION.                                               
866400                                                                          
866500     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
866600                                            W-IDDC-WDB3-DEF               
866700     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
866800                                            W-IDDISTR-WDB3-DEF            
866900     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
867000     PERFORM IMS-GU-WLGMTB01                                              
867100                                                                          
867200     IF DC-KDMOMSIN = +2                                                  
867300*- - - - - -  EJ KREDITERA MOMS                                           
867400       CONTINUE                                                           
867500     ELSE                                                                 
867600       MOVE NEJ  TO MOMS-FRITT                                            
867700                                                                          
867800       MOVE 'FI'                         TO W009-MOMS-IDLANDX2            
867900       MOVE '01'                         TO W009-MOMS-KDVAT               
868000       CALL W009MOMS USING W009-MOMS-W009MOMS                             
868100                                                                          
868200       IF W009-MOMS-KDSVAR NOT = SPACE                                    
868300          DISPLAY                                                         
868400          '*** W4183000 - W009MOMS SAKNAS***'                             
868500          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
868600       END-IF                                                             
868700     END-IF                                                               
868800     .                                                                    
868900     EJECT                                                                
869000                                                                          
869100 S83-SAETT-MOMS-BE SECTION.                                               
869200     MOVE MOMS-WS-IDDC                   TO W-IDDC-WDB3                   
869300                                            W-IDDC-WDB3-DEF               
869400     MOVE ANM-IDDISTR                    TO W-IDDISTR-WDB3                
869500                                            W-IDDISTR-WDB3-DEF            
869600     MOVE ANM-IDKUNDNR                   TO W-IDKUNDNR-WDB3               
869700     PERFORM IMS-GU-WLGMTB01                                              
869800                                                                          
869900     IF DC-KDMOMSIN = +2                                                  
870000*- - - - - -  EJ KREDITERA MOMS                                           
870100       CONTINUE                                                           
870200     ELSE                                                                 
870300       MOVE NEJ  TO MOMS-FRITT                                            
870400                                                                          
870500       MOVE 'BE'                         TO W009-MOMS-IDLANDX2            
870600       MOVE '01'                         TO W009-MOMS-KDVAT               
870700       CALL W009MOMS USING W009-MOMS-W009MOMS                             
870800                                                                          
870900       IF W009-MOMS-KDSVAR NOT = SPACE                                    
871000          DISPLAY                                                         
871100          '*** W4183000 - W009MOMS SAKNAS***'                             
871200          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
871300       END-IF                                                             
871400     END-IF                                                               
871500     .                                                                    
871600     EJECT                                                                
871700 S90-SKAPA-DDI-LIST SECTION.                                              
871800     MOVE ANM-IDDISTR                     TO UTAN-IDDISTR                 
871900                                             W-IDDISTR-L5                 
872000     MOVE ANM-IDKUNDNR                    TO UTAN-IDKUNDNR                
872100                                             W-IDKUNDNR-L5                
872200     MOVE ANM-IDRAPPNR                    TO UTAN-IDRAPPNR                
872300     MOVE ANM-KDVALISO                    TO UTAN-KDVALISO                
872400                                                                          
872500     MOVE LEV-IDARTNR                     TO UTAN-IDARTNR                 
872600     MOVE LEV-IDDC                        TO UTAN-IDDC                    
872700     MOVE LEV-IDDC-RET                    TO UTAN-IDDC-RET                
872800     MOVE LEV-IDFAKT                      TO UTAN-IDFAKT                  
872900     MOVE LEV-IDFAKT-LOC                  TO UTAN-IDFAKT-LOC              
873000     MOVE IN-IDORDNR                      TO UTAN-IDKNOTNR                
873100     MOVE LEV-IDKONTO                     TO UTAN-IDKONTO                 
873200     MOVE LEV-IDKST                       TO UTAN-IDKST                   
873300     MOVE LEV-IDKUNDRF                    TO UTAN-IDKUNDRF                
873400     MOVE LEV-IDORDNR7                    TO UTAN-IDORDNR7                
873500     MOVE LEV-IDLOPNRM                    TO UTAN-IDLOPNRM                
873600     MOVE LEV-KDANMORS                    TO UTAN-KDANMORS                
873700     MOVE LEV-KVANTAL-ILI                 TO UTAN-KVANTAL-ILI             
873800     MOVE LEV-KVAVV-KVAL                  TO UTAN-KVAVV-KVAL              
873900     MOVE LEV-KVAVV-KVANT                 TO UTAN-KVAVV-KVANT             
874000     MOVE LEV-KVLEVANM                    TO UTAN-KVLEVANM                
874100     MOVE LEV-KVLEVANM-BEKR               TO UTAN-KVLEVANM-BEKR           
874200     MOVE LEV-KVRETINL                    TO UTAN-KVRETINL                
874300     MOVE LEV-KVRETINL-SKR                TO UTAN-KVRETINL-SKR            
874400     MOVE LEV-PRARTBTO                    TO UTAN-PRARTBTO                
874500     MOVE LEV-PRARTBTO-LOC                TO UTAN-PRARTBTO-LOC            
874600     MOVE LEV-PRFRAKT                     TO UTAN-PRFRAKT                 
874700     MOVE LEV-TIFAKT                      TO UTAN-TIFAKT                  
874800     MOVE LEV-TIFAKT-LOC                  TO UTAN-TIFAKT-LOC              
874900     MOVE LEV-KDVAT                       TO UTAN-KDVAT                   
875000     MOVE LEV-BEART-VIPS                  TO UTAN-BEART-VIPS              
875100                                                                          
875200     PERFORM S90-SKRIV-DDI                                                
875300                                                                          
875400     .                                                                    
875500     EJECT                                                                
875600                                                                          
875700 S90-SKRIV-DDI SECTION.                                                   
875800*    DISPLAY '*** S90-SKRIV-DDI '                                         
875900                                                                          
876000     WRITE UTAN-POST FROM UTAN-AREA                                       
876100                                                                          
876200     MOVE SPACE                           TO POSTSUM-TRANSTYP             
876300     MOVE 'W418AN'                        TO POSTSUM-FDNAMN               
876400     MOVE 'W41830EA'                      TO POSTSUM-DDNAMN2              
876500     CALL POSTSUM USING POSTSUM-PARM                                      
876600     .                                                                    
876700     EJECT                                                                
876800                                                                          
876900 S91-UPPDATERA-STATUS-8 SECTION.                                          
877000*    DISPLAY '*** S91-UPPDATERA-STATUS-8 '                                
877100                                                                          
877200     MOVE '010'                     TO UT34-IDPTYP                        
877300     MOVE ANM-IDLEVANM              TO UT34-IDLEVANM                      
877400     MOVE LEV-IDARTNR               TO UT34-IDARTNR                       
877500     MOVE LEV-IDRADNR               TO UT34-IDRADNR                       
877600     MOVE SPACE                     TO UT34-IDDC                          
877700     MOVE ZERO                      TO UT34-IDKNOTNR                      
877800                                       UT34-KDAVVTYP                      
877900                                       UT34-KDFAKTYP-KNOT                 
878000                                       UT34-KVLEVANM                      
878100                                       UT34-TIKNOTA                       
878200                                       UT34-TIRETILL                      
878300     MOVE WS-KVRADER-RT             TO UT34-KVRADER-RT                    
878400     MOVE '8'                       TO UT34-KDLEVANM                      
878500     MOVE NEJ                       TO UT34-FLFARLIG                      
878600     MOVE ZERO                      TO UT34-PRARTBTO                      
878700     MOVE ZERO                      TO UT34-KDINVKAT                      
878800     MOVE ZERO                      TO UT34-KVJUSTKV                      
878900     MOVE ZERO                      TO UT34-TIM-INV                       
879000     MOVE SPACE                     TO UT34-TEINVANM                      
879100     MOVE SPACE                     TO UT34-IDDC-RET                      
879200     MOVE ZERO                      TO UT34-IXDCCLEAR                     
879300                                                                          
879400     PERFORM S20-SKRIV-W41834                                             
879500     .                                                                    
879600     EJECT                                                                
879700 S100-NOLLA-720-AREA SECTION.                                             
879800                                                                          
879900     MOVE SPACE                     TO 720-IDPTYP                         
880000                                       720-IDDC                           
880100     MOVE SPACE                     TO 720-KDANMORS                       
880200     MOVE NEJ                       TO 720-FLLSBOK                        
880300                                        WS-FLLSBOK                        
880400     MOVE ZERO                      TO 720-IDARTNR                        
880500                                       720-IDDISTR                        
880600                                       720-IDKUNDNR                       
880700                                       720-IDKNOTNR                       
880800                                       720-IDRAPPNR                       
880900                                       720-DAKRENOT                       
881000                                       720-PRLANDCO                       
881100                                       720-PRFRAKT                        
881200                                       720-PRLEGKST                       
881300                                       720-PRFOERS                        
881400                                       720-PRMOMS                         
881500                                       720-PRKURS                         
881600                                       720-SUKRENTO                       
881700                                       720-SUKRENOT                       
881800                                       720-SUKREUTL                       
881900                                       720-KVKREANT                       
882000                                       720-PRARTNTO                       
882100                                       720-PRAVCOST                       
882200                                       720-KDPRODSL                       
882300                                       720-KDPSLLOC                       
882400                                       720-REVAT                          
882500     .                                                                    
882600     EJECT                                                                
882700* --- IMS SEKTIONER ---                                                   
882800                                                                          
882900 IMS-GU-WDA2A1-3 SECTION.                                                 
883000                                                                          
883100     STRING 'WDA2A1  (WDA2A1KY >' W-WDA2A1KY-MIN-X                        
883200                    '&WDA2A1KY <' W-WDA2A1KY-MAX-X                        
883300                    '&KDLEVATTNE' W-KDLEVATT-X ')'                        
883400          DELIMITED BY SIZE INTO SSA1                                     
883500     MOVE '  GE' TO GODK-STATUSKODER                                      
883600     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-AREA-WDA2A1 SSA1              
883700     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
883800     PERFORM IMS-STATUSKONTROLL                                           
883900     .                                                                    
884000     EJECT                                                                
884100 IMS-GN-WDA2A1-3 SECTION.                                                 
884200                                                                          
884300     STRING 'WDA2A1  (WDA2A1KY >' W-WDA2A1KY-MIN-X                        
884400                    '&WDA2A1KY <' W-WDA2A1KY-MAX-X                        
884500                    '&KDLEVATTNE' W-KDLEVATT-X ')'                        
884600          DELIMITED BY SIZE INTO SSA1                                     
884700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
884800     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-AREA-WDA2A1 SSA1              
884900     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
885000     PERFORM IMS-STATUSKONTROLL                                           
885100     .                                                                    
885200     EJECT                                                                
885300 IMS-GU-WDA2A1 SECTION.                                                   
885400                                                                          
885500     STRING 'WDA2A1  (WDA2A1KY >' W-WDA2A1KY-MIN-X                        
885600                       '&WDA2A1KY <' W-WDA2A1KY-MAX-X ')'                 
885700          DELIMITED BY SIZE INTO SSA1                                     
885800     MOVE '  GE' TO GODK-STATUSKODER                                      
885900     CALL CBLTDLI USING GU WDA2A-PCB DLI-IO-AREA-WDA2A1 SSA1              
886000     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
886100     PERFORM IMS-STATUSKONTROLL                                           
886200     .                                                                    
886300     EJECT                                                                
886400 IMS-GN-WDA2A1 SECTION.                                                   
886500                                                                          
886600     STRING 'WDA2A1  (WDA2A1KY >' W-WDA2A1KY-MIN-X                        
886700                    '&WDA2A1KY <' W-WDA2A1KY-MAX-X ')'                    
886800          DELIMITED BY SIZE INTO SSA1                                     
886900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
887000     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-AREA-WDA2A1 SSA1              
887100     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
887200     PERFORM IMS-STATUSKONTROLL                                           
887300     .                                                                    
887400     EJECT                                                                
887500 IMS-GU-KREE-ANM SECTION.                                                 
887600*    DISPLAY '*** IMS-GU-KREE-ANM '                                       
887700                                                                          
887800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
887900          DELIMITED BY SIZE INTO SSA1                                     
888000     MOVE '    ' TO GODK-STATUSKODER                                      
888100     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
888200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
888300     PERFORM IMS-STATUSKONTROLL                                           
888400     .                                                                    
888500     EJECT                                                                
888600 IMS-GNP-KREE-LEV SECTION.                                                
888700*    DISPLAY '*** IMS-GNP-KREE-LEV '                                      
888800                                                                          
888900     MOVE 'WLKREE11 ' TO SSA1                                             
889000     MOVE '  GE' TO GODK-STATUSKODER                                      
889100     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
889200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
889300     PERFORM IMS-STATUSKONTROLL                                           
889400     .                                                                    
889500     EJECT                                                                
889600 IMS-GNP-KREE-TXT SECTION.                                                
889700*    DISPLAY '*** IMS-GNP-KREE-TXT '                                      
889800                                                                          
889900     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
890000          DELIMITED BY SIZE INTO SSA1                                     
890100     MOVE 'WLKREE21 ' TO SSA2                                             
890200     MOVE '  GE' TO GODK-STATUSKODER                                      
890300     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA221 SSA1 SSA2         
890400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
890500     PERFORM IMS-STATUSKONTROLL                                           
890600     .                                                                    
890700     EJECT                                                                
890800 IMS-GU-ARTN-ARTN01 SECTION.                                              
890900*    DISPLAY '*** IMS-GU-ARTN-ARTN01'                                     
891000                                                                          
891100     STRING 'WLARTN01(IDARTNR  =' W-IDARTNR-X ')'                         
891200          DELIMITED BY SIZE INTO SSA1                                     
891300     MOVE '  GE' TO GODK-STATUSKODER                                      
891400     CALL CBLTDLI USING GU ARTN-PCB DLI-IO-AREA-WDD501 SSA1               
891500     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
891600     PERFORM IMS-STATUSKONTROLL                                           
891700     .                                                                    
891800     EJECT                                                                
891900 IMS-GU-ARTC01 SECTION.                                                   
892000*     DISPLAY '***** IMS-GU-ARTC01'                                       
892100                                                                          
892200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
892300          DELIMITED BY SIZE INTO SSA1                                     
892400     MOVE '    ' TO GODK-STATUSKODER                                      
892500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1               
892600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
892700     PERFORM IMS-STATUSKONTROLL                                           
892800     .                                                                    
892900     EJECT                                                                
893000 IMS-GNP-ARTC11 SECTION.                                                  
893100*     DISPLAY '***** IMS-GNP-ARTC11'                                      
893200                                                                          
893300     MOVE 'WLARTC11 ' TO SSA1                                             
893400     MOVE '    ' TO GODK-STATUSKODER                                      
893500     CALL CBLTDLI USING                                                   
893600           GNP ARTC-PCB DLI-IO-AREA-WDK611 SSA1                           
893700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
893800     PERFORM IMS-STATUSKONTROLL                                           
893900     .                                                                    
894000     EJECT                                                                
894100 IMS-GNP-WLARTC21-FIRST SECTION.                                          
894200                                                                          
894300     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
894400     MOVE 'WLARTC21*F' TO SSA2                                            
894500     MOVE '  GE' TO GODK-STATUSKODER                                      
894600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK621 SSA1 SSA2              
894700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
894800     PERFORM IMS-STATUSKONTROLL                                           
894900     .                                                                    
895000     SKIP3                                                                
895100 IMS-GNP-WLARTC21 SECTION.                                                
895200                                                                          
895300     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
895400     MOVE 'WLARTC21 ' TO SSA2                                             
895500     MOVE '  GE' TO GODK-STATUSKODER                                      
895600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK621 SSA1 SSA2              
895700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
895800     PERFORM IMS-STATUSKONTROLL                                           
895900     .                                                                    
896000     EJECT                                                                
896100 IMS-GU-WDK711 SECTION.                                                   
896200*     DISPLAY '**** IMS-GU-WDK711'                                        
896300                                                                          
896400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
896500            DELIMITED BY SIZE INTO SSA1                                   
896600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
896700            DELIMITED BY SIZE INTO SSA2                                   
896800                                                                          
896900     MOVE '  GE' TO GODK-STATUSKODER                                      
897000     CALL CBLTDLI USING                                                   
897100           GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2                       
897200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
897300     PERFORM IMS-STATUSKONTROLL                                           
897400     .                                                                    
897500     EJECT                                                                
897600 IMS-GU-WDK712 SECTION.                                                   
897700*     DISPLAY '**** IMS-GU-WDK712'                                        
897800                                                                          
897900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
898000            DELIMITED BY SIZE INTO SSA1                                   
898100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
898200            DELIMITED BY SIZE INTO SSA2                                   
898300                                                                          
898400     MOVE '  GE' TO GODK-STATUSKODER                                      
898500     CALL CBLTDLI USING                                                   
898600           GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2                       
898700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
898800     PERFORM IMS-STATUSKONTROLL                                           
898900     .                                                                    
899000     EJECT                                                                
899100 IMS-GU-INVA-INVA01 SECTION.                                              
899200*    DISPLAY '*** IMS-GU-INVA-INVA01'                                     
899300                                                                          
899400     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
899500          DELIMITED BY SIZE INTO SSA1                                     
899600     MOVE '  GE' TO GODK-STATUSKODER                                      
899700     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA-WDH101 SSA1               
899800     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
899900     PERFORM IMS-STATUSKONTROLL                                           
900000     .                                                                    
900100     EJECT                                                                
900200 IMS-GNP-INVA-INVA11 SECTION.                                             
900300*    DISPLAY '*** IMS-GNP-INVA-INVA11'                                    
900400                                                                          
900500     STRING  'WDH111  (WDH111KY>=' W-WDH111KY-MIN                         
900600                     '&WDH111KY<=' W-WDH111KY-MAX ')'                     
900700              DELIMITED BY SIZE INTO SSA1                                 
900800     MOVE '  GE' TO GODK-STATUSKODER                                      
900900     CALL CBLTDLI USING GNP INVA-PCB DLI-IO-AREA-WDH111 SSA1              
901000     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
901100     PERFORM IMS-STATUSKONTROLL                                           
901200     .                                                                    
901300 IMS-GET-WLGMTA01 SECTION.                                                
901400                                                                          
901500     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
901600                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
901700            DELIMITED BY SIZE INTO SSA1                                   
901800                                                                          
901900     MOVE '  ' TO GODK-STATUSKODER                                        
902000     CALL CBLTDLI USING                                                   
902100           GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1                            
902200     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
902300     PERFORM IMS-STATUSKONTROLL                                           
902400     .                                                                    
902500     EJECT                                                                
902600 IMS-GET-WLGMTA01-UNIK SECTION.                                           
902700                                                                          
902800     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
902900            DELIMITED BY SIZE INTO SSA1                                   
903000                                                                          
903100     MOVE '  GE' TO GODK-STATUSKODER                                      
903200     CALL CBLTDLI USING                                                   
903300           GU GMTA-PCB DLI-IO-AREA-WDB201 SSA1                            
903400     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
903500     PERFORM IMS-STATUSKONTROLL                                           
903600     .                                                                    
903700     EJECT                                                                
903800 IMS-GU-WLGMTB01 SECTION.                                                 
903900                                                                          
904000     STRING 'WLGMTB01(WDB301KY =' W-WDB301-KEY                            
904100                    '!WDB301KY =' W-WDB301-KEY-DEF ')'                    
904200          DELIMITED BY SIZE INTO SSA1                                     
904300     MOVE '  '     TO GODK-STATUSKODER                                    
904400     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-WDB301 SSA1               
904500     MOVE GMTB-STATUS-CODE      TO STATUS-WS                              
904600     PERFORM IMS-STATUSKONTROLL                                           
904700     .                                                                    
904800    SKIP3                                                                 
904900 IMS-GET-WDB101 SECTION.                                                  
905000                                                                          
905100     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
905200          DELIMITED BY SIZE INTO SSA1                                     
905300     MOVE '  ' TO GODK-STATUSKODER                                        
905400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
905500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
905600     PERFORM IMS-STATUSKONTROLL                                           
905700     .                                                                    
905800     EJECT                                                                
905900 IMS-GU-WDL501      SECTION.                                              
906000                                                                          
906100     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
906200          DELIMITED BY SIZE INTO SSA1                                     
906300     MOVE '  GE'           TO GODK-STATUSKODER                            
906400     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
906500     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
906600     PERFORM IMS-STATUSKONTROLL                                           
906700     .                                                                    
906800     EJECT                                                                
906900 IMS-GNP-WDL511      SECTION.                                             
907000                                                                          
907100     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
907200                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
907300          DELIMITED BY SIZE INTO SSA1                                     
907400     MOVE '  GE'           TO GODK-STATUSKODER                            
907500     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
907600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
907700     PERFORM IMS-STATUSKONTROLL                                           
907800     .                                                                    
907900     EJECT                                                                
908000 IMS-GNP-WDL521     SECTION.                                              
908100                                                                          
908200     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
908300          DELIMITED BY SIZE INTO SSA1                                     
908400     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-X ')'                         
908500          DELIMITED BY SIZE INTO SSA2                                     
908600     MOVE '  GE'           TO GODK-STATUSKODER                            
908700     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
908800     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
908900     PERFORM IMS-STATUSKONTROLL                                           
909000     .                                                                    
909100     EJECT                                                                
909200 IMS-01-GU-WDQ2C1 SECTION.                                                
909300                                                                          
909400     STRING 'WDQ2C1  (WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
909500          DELIMITED BY SIZE INTO SSA1                                     
909600     MOVE '  GE'               TO GODK-STATUSKODER                        
909700     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
909800     MOVE WDQ2C-STATUS-CODE    TO STATUS-WS                               
909900     PERFORM IMS-STATUSKONTROLL                                           
910000     .                                                                    
910100     SKIP3                                                                
910200 IMS-GU-WDGX4103 SECTION.                                                 
910300                                                                          
910400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
910500          DELIMITED BY SIZE INTO SSA1                                     
910600     MOVE '  GE' TO GODK-STATUSKODER                                      
910700     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
910800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
910900     PERFORM IMS-STATUSKONTROLL                                           
911000     .                                                                    
911100     EJECT                                                                
911200 IMS-GNP-WDGX4104 SECTION.                                                
911300                                                                          
911400     STRING 'WDGX4104*F(KEY4104  =' W-WDGXKEY-4104-X ')'                  
911500          DELIMITED BY SIZE INTO SSA1                                     
911600     MOVE '  GE' TO GODK-STATUSKODER                                      
911700     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
911800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
911900     PERFORM IMS-STATUSKONTROLL                                           
912000     .                                                                    
912100     EJECT                                                                
912200 IMS-GNP-WDGX4106 SECTION.                                                
912300                                                                          
912400     STRING 'WDGX4104(KEY4104  =' W-WDGXKEY-4104-X ')'                    
912500          DELIMITED BY SIZE INTO SSA1                                     
912600     MOVE 'WDGX4106 ' TO SSA2                                             
912700     MOVE '  GE' TO GODK-STATUSKODER                                      
912800     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4106 SSA1 SSA2            
912900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
913000     PERFORM IMS-STATUSKONTROLL                                           
913100     .                                                                    
913200     EJECT                                                                
913300 IMS-GU-WDB601-RET    SECTION.                                            
913400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-RET-X ')'                     
913500          DELIMITED BY SIZE INTO SSA1                                     
913600     MOVE '  GE' TO GODK-STATUSKODER                                      
913700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601-RET SSA1                  
913800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
913900     PERFORM IMS-STATUSKONTROLL                                           
914000     IF SEGMENT-SAKNAS                                                    
914100         MOVE SPACE TO RET-DCS-KDDC                                       
914200                       RET-DCS-IDLANDX2                                   
914300     END-IF                                                               
914400     .                                                                    
914500     EJECT                                                                
914600 IMS-GU-WDB601    SECTION.                                                
914700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
914800          DELIMITED BY SIZE INTO SSA1                                     
914900     MOVE '  GE' TO GODK-STATUSKODER                                      
915000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
915100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
915200     PERFORM IMS-STATUSKONTROLL                                           
915300     .                                                                    
915400     EJECT                                                                
915500 IMS-GNP-WDB611-FIRST    SECTION.                                         
915600                                                                          
915700     STRING 'WDB611  *F(WDB611KY =' W-WDB611KY-X ')'                      
915800          DELIMITED BY SIZE INTO SSA1                                     
915900     MOVE '  GE' TO GODK-STATUSKODER                                      
916000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
916100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
916200     PERFORM IMS-STATUSKONTROLL                                           
916300     .                                                                    
916400     EJECT                                                                
916500 IMS-STATUSKONTROLL SECTION.                                              
916600                                                                          
916700     SET STATUS-IX TO 1                                                   
916800     SEARCH GODK-STATUS                                                   
916900       AT END                                                             
917000         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT-STR                    
917100         DISPLAY FELTEXT                                                  
917200         CALL FELLOG                                                      
917300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
917400         CONTINUE                                                         
917500     END-SEARCH                                                           
917600     .                                                                    
