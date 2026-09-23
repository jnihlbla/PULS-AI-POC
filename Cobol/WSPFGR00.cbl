000100*COMPOPT ISPFPGM=YES GDDMPGM=YES                                          
000200 Id Division.                                                             
000300 Program-Id.           WSPFGR00.                                          
000400 Author.               Ood Olsen.                                         
000500 Date-Written.         880608.                                            
000600*    Remarks.                                                             
000700*                                                                         
000800*     PROGRAMET VISAR EN GRAF I ICU                                       
000900*                                                                         
001000*     STARTAS MED CLIST WSPFGR                                            
001100*                                                                         
001200 Environment Division.                                                    
001300 Data Division.                                                           
001400     eject                                                                
001500 Working-Storage Section.                                                 
001600                                                                          
001601                                                                          
001610*    -- CHECKED BY WY2000                                                 
001700 77  rc                          Pic 9(4)    Value Zero.                  
001800 77  testfalt                    Pic 9(9)    Value Zero.                  
001900 77  ptr                         Pic  9(8) Comp.                          
002000 77  point                       Pic  9(8) Comp.                          
002100 77  ant-sp                      Pic  9(3)   Value Zero.                  
002200 77  ix                          Pic  9(3)   Value Zero.                  
002300 77  ix-xrad                     Pic  9(3)   Value Zero.                  
002400 77  ix-ygrp                     Pic  9(3)   Value Zero.                  
002500 77  ix-rad                      Pic  9(3)   Value Zero.                  
002600 77  doix                        Pic  9(3)   Value Zero.                  
002700 77  doix1                       Pic  9(3)   Value Zero.                  
002800 77  doix2                       Pic  9(3)   Value Zero.                  
002900 77  ix3                         Pic  9(3)   Value Zero.                  
003000 77  grafdata-ok                 Pic X       Value 'J'.                   
003100 77  ja                          Pic X       Value 'J'.                   
003200 77  nej                         Pic X       Value 'N'.                   
003300 77  end-of-tab                  Pic X       Value 'N'.                   
003400     eject                                                                
003500 01  cobol-area.                                                          
003600     03 cob-table                Pic X(8).                                
003700     03 cob-disp                 Pic S9.                                  
003800     03 cob-help                 Pic S9.                                  
003900     03 cob-isol                 Pic S9.                                  
004000     03 cob-format               Pic X(8).                                
004100     03 cob-data                 Pic X(8).                                
004200     03 cob-expl                 Pic S9.                                  
004300     03 cob-header               Pic X(160).                              
004400     03 cob-headerl              Pic S9(8) Comp.                          
004500     03 cob-xlabels              Pic X(300).                              
004600     03 cob-xlabell              Pic S9(8) Comp.                          
004700     03 cob-ykeys                Pic X(300).                              
004800     03 cob-xcol                 Pic X(8).                                
004900     03 cob-ycols                Pic X(1000).                             
005000     03 cob-ycolsn               Pic S9(8) Comp.                          
005100     03 cob-rows                 Pic S9(8) Comp.                          
005200     03 cob-xvalue               Pic X(50).                               
005300     03 cob-yvalue-grp Occurs 20.                                         
005400       05 cob-yvalue             Pic X(16).                               
005500     03 cob-msg                  Pic X(70).                               
005600     03 cob-formdel              Pic X    Value Space.                    
005700     03 cob-output               Pic X(8) Value Space.                    
005800     03 cob-copies               Pic S9(8) Comp.                          
005900 01  ispf-area.                                                           
006000     03 ispf-table               Pic X(8) Value 'TABLE   '.               
006100     03 ispf-disp                Pic X(8) Value 'DISP    '.               
006200     03 ispf-help                Pic X(8) Value 'HELP    '.               
006300     03 ispf-isol                Pic X(8) Value 'ISOL    '.               
006400     03 ispf-format              Pic X(8) Value 'FORMAT  '.               
006500     03 ispf-data                Pic X(8) Value 'DATA    '.               
006600     03 ispf-expl                Pic X(8) Value 'EXPL    '.               
006700     03 ispf-header              Pic X(8) Value 'HEADER  '.               
006800     03 ispf-headerl             Pic X(8) Value 'HEADERL '.               
006900     03 ispf-xlabels             Pic X(8) Value 'XLABELS '.               
007000     03 ispf-xlabell             Pic X(8) Value 'XLABELL '.               
007100     03 ispf-ykeys               Pic X(8) Value 'YKEYS   '.               
007200     03 ispf-xcol                Pic X(8) Value 'XCOL    '.               
007300     03 ispf-ycols               Pic X(8) Value 'YCOLS   '.               
007400     03 ispf-ycolsn              Pic X(8) Value 'YCOLSN  '.               
007500     03 ispf-rows                Pic X(8) Value 'ROWS    '.               
007600     03 ispf-xvalue              Pic X(8).                                
007700     03 ispf-yvalue1             Pic X(8).                                
007800     03 ispf-yvalue2             Pic X(8).                                
007900     03 ispf-yvalue3             Pic X(8).                                
008000     03 ispf-yvalue4             Pic X(8).                                
008100     03 ispf-yvalue5             Pic X(8).                                
008200     03 ispf-yvalue6             Pic X(8).                                
008300     03 ispf-yvalue7             Pic X(8).                                
008400     03 ispf-yvalue8             Pic X(8).                                
008500     03 ispf-yvalue9             Pic X(8).                                
008600     03 ispf-yvalue10            Pic X(8).                                
008700     03 ispf-yvalue11            Pic X(8).                                
008800     03 ispf-yvalue12            Pic X(8).                                
008900     03 ispf-yvalue13            Pic X(8).                                
009000     03 ispf-yvalue14            Pic X(8).                                
009100     03 ispf-yvalue15            Pic X(8).                                
009200     03 ispf-yvalue16            Pic X(8).                                
009300     03 ispf-yvalue17            Pic X(8).                                
009400     03 ispf-yvalue18            Pic X(8).                                
009500     03 ispf-yvalue19            Pic X(8).                                
009600     03 ispf-yvalue20            Pic X(8).                                
009700     03 ispf-msg                 Pic X(8) Value 'ERRMSG  '.               
009800     03 ispf-formdel             Pic X(8) Value 'FORMDEL '.               
009900     03 ispf-output              Pic X(8) Value 'OUTPUT  '.               
010000     03 ispf-copies              Pic X(8) Value 'COPIES  '.               
010100     eject                                                                
010200***  ISPF-SERVICAR                                                        
010300                                                                          
010400 01  isp-select                  Pic X(8)    Value 'SELECT  '.            
010500 01  setmsg                      Pic X(8)    Value 'SETMSG  '.            
010600 01  tbadd                       Pic X(8)    Value 'TBADD   '.            
010700 01  tbmod                       Pic X(8)    Value 'TBMOD   '.            
010800 01  tbput                       Pic X(8)    Value 'TBPUT   '.            
010900 01  tbcreate                    Pic X(8)    Value 'TBCREATE'.            
011000 01  tbdelete                    Pic X(8)    Value 'TBDELETE'.            
011100 01  tbvclear                    Pic X(8)    Value 'TBVCLEAR'.            
011200 01  tbsave                      Pic X(8)    Value 'TBSAVE  '.            
011300 01  tbopen                      Pic X(8)    Value 'TBOPEN  '.            
011400 01  tbclose                     Pic X(8)    Value 'TBCLOSE '.            
011500 01  tbdispl                     Pic X(8)    Value 'TBDISPL '.            
011600 01  tbend                       Pic X(8)    Value 'TBEND   '.            
011700 01  tbget                       Pic X(8)    Value 'TBGET   '.            
011800 01  tbsarg                      Pic X(8)    Value 'TBSARG  '.            
011900 01  tbscan                      Pic X(8)    Value 'TBSCAN  '.            
012000 01  tbskip                      Pic X(8)    Value 'TBSKIP  '.            
012100 01  tbtop                       Pic X(8)    Value 'TBTOP   '.            
012200 01  tbbottom                    Pic X(8)    Value 'TBBOTTOM'.            
012300 01  vdefine                     Pic X(8)    Value 'VDEFINE '.            
012400 01  vput                        Pic X(8)    Value 'VPUT    '.            
012500 01  vget                        Pic X(8)    Value 'VGET    '.            
012600 01  vreset                      Pic X(8)    Value 'VRESET  '.            
012700 01  isp-control                 Pic X(8)    Value 'CONTROL '.            
012800     eject                                                                
012900***  PARAMETRAR TILL SERVICAR                                             
013000                                                                          
013100 01  char                      Pic X(8)   Value 'CHAR    '.               
013200 01  fixed                     Pic X(8)   Value 'FIXED   '.               
013300 01  v-opt                     Pic X(16)  Value '(COPY NOBSCAN)'.         
013400 01  v-opt2                    Pic X(16)  Value '(NOBSCAN)'.              
013500 01  move-mode                 Pic X(8)   Value 'MOVE    '.               
013600 01  isp-write                 Pic X(8)   Value 'WRITE   '.               
013700 01  isp-display               Pic X(8)   Value 'DISPLAY '.               
013800 01  nowrite                   Pic X(8)   Value 'NOWRITE '.               
013900 01  profile                   Pic X(8)   Value 'PROFILE '.               
014000 01  refresh                   Pic X(8)   Value 'REFRESH '.               
014100 01  save                      Pic X(8)   Value 'SAVE    '.               
014200 01  restore                   Pic X(8)   Value 'RESTORE '.               
014300 01  share                     Pic X(8)   Value 'SHARE   '.               
014310 01  shared                    Pic X(8)   Value 'SHARED  '.               
014400 01  nop                       Pic X(8)   Value '        '.               
014500     eject                                                                
014600                                                                          
014700 01  length-values.                                                       
014800     03 len                    Pic S9(9) Comp.                            
014900     eject                                                                
015000 01  type-values.                                                         
015100     03 type-1                 Pic S9(9) Comp Value +1.                   
015200     03 type-2                 Pic S9(9) Comp Value +2.                   
015300     03 type-3                 Pic S9(9) Comp Value +3.                   
015400     03 type-4                 Pic S9(9) Comp Value +4.                   
015500     03 type-5                 Pic S9(9) Comp Value +5.                   
015600     03 type-6                 Pic S9(9) Comp Value +6.                   
015700     03 type-7                 Pic S9(9) Comp Value +7.                   
015800     03 type-8                 Pic S9(9) Comp Value +8.                   
015900     03 type-9                 Pic S9(9) Comp Value +9.                   
016000     03 type-10                Pic S9(9) Comp Value +10.                  
016100     03 type-11                Pic S9(9) Comp Value +11.                  
016200     eject                                                                
016300*01  -COPY WICUPCS                                                        
016400     eject                                                                
016500*01  -COPY WDECAREA                                                       
016600     eject                                                                
016700***  DATA TILL ICU                                                        
016800                                                                          
016900 01  dummy.                                                               
017000     02 dummy1             Pic S9(8)  Comp Value +0.                      
017100*         PAIRED DATA CONTROL STRUCTURE, NOT USED                         
017200                                                                          
017300 01  xlabels               Pic X(300).                                    
017400*         X DATA LABLES                                                   
017500                                                                          
017600 01  xdata.                                                               
017700     02 x-value            Comp-1 Occurs 54 Times.                        
017800*         X DATA                                                          
017900                                                                          
018000 01  ydata.                                                               
018010*                                        1080 = 20 x 54                   
018100     02 y-value            Comp-1 Occurs 1080 Times.                      
018200*         Y DATA                                                          
018300                                                                          
018400 01  keys                  Pic X(1000).                                   
018500*         CHART KEYS FOR Y DATA GROUPS                                    
018600                                                                          
018700 01  chart-head            Pic X(160) Value Space.                        
018800                                                                          
018900     eject                                                                
019000***  TEMPORÄR LAGRING AV YDATA OCH XKEYS                                  
019100 01  ydata-temp-grp.                                                      
019200     03  ydata-temp Occurs 20 Times.                                      
019300         05 y-temp             Comp-1 Occurs 54 Times.                    
019400 01  keydata-grp.                                                         
019500     02 keydata            Pic X(25) Occurs 20 Times.                     
019600 01  xpkod-grp.                                                           
019700     02 xpkod              Pic X(50) Occurs 54 Times.                     
019800     skip2                                                                
019900 01  xpkod-lengd           Pic S9(4) Comp-3 Value +50.                    
020000     eject                                                                
020100***  NAMN PÅ ALLOCERADE GRAFIKBIBLIOTEK                                   
020200                                                                          
020300 01  ddsymbl.                                                             
020400     03 Filler                 Pic X(8) Value 'ADMSYMBL'.                 
020500     03 Filler                 Pic X(8) Value 'SYSSYMBL'.                 
020600 01  dddata.                                                              
020700     03 Filler                 Pic X(8) Value 'ADMCDATA'.                 
020800     03 Filler                 Pic X(8) Value 'SYSCDATA'.                 
020900 01  ddform.                                                              
021000     03 Filler                 Pic X(8) Value 'ADMCFORM'.                 
021100     03 Filler                 Pic X(8) Value 'SYSCFORM'.                 
021200 01  ddgdf.                                                               
021300     03 Filler                 Pic X(8) Value 'ADMGDF  '.                 
021400     03 Filler                 Pic X(8) Value 'SYSGDF  '.                 
021500     eject                                                                
021600 01  dynamiska-subprogram.                                                
021700     03  isplink               Pic X(8) Value 'ISPLINK'.                  
021800 Procedure Division.                                                      
021900                                                                          
022000 STYR Section.                                                            
022100                                                                          
022200     Perform A-INIT                                                       
022300                                                                          
022400     If cob-ycolsn > Zero And cob-ycolsn <= 20                            
022500       Perform B-LAS-GRAF-DATA                                            
022600                                                                          
022700       If grafdata-ok = ja                                                
022800         Perform C-REDIGERA-GRAFDATA                                      
022900         Perform X-VISA-GRAF                                              
023000       Else                                                               
023100         Move +3 To return-code                                           
023200*>>      TOM TABELL                                                       
023300       End-If                                                             
023400     Else                                                                 
023500        Move +4 To return-code                                            
023600*>>     FELAKTIGT ANTAL GRUPPER                                           
023700     End-If                                                               
023800     If return-code > 0                                                   
023900       Call ISPLINK Using vput ispf-msg profile                           
024000     End-If                                                               
024100     Goback                                                               
024200     .                                                                    
024300     eject                                                                
024400 A-INIT Section.                                                          
024500                                                                          
024600     Perform AA-VDEFINE                                                   
024700     Move cob-disp    To pcsdisp                                          
024800     Move cob-help    To pcshelp                                          
024900     Move cob-isol    To pcsisol                                          
025000     Move cob-format  To pcsfname                                         
025100     Move cob-data    To pcsdname                                         
025200     Move cob-ycolsn  To pcsng                                            
025300     Move cob-output  To pcspname                                         
025400     Move cob-copies  To pcspcopy                                         
025500     Move Zero        To pcsprdep                                         
025600     Move Zero        To pcsprwid                                         
025700     Move Zero        To pcspunit                                         
025800     If cob-rows < +54                                                    
025900       Move cob-rows  To pcsne                                            
026000     Else                                                                 
026100       Move +54       To pcsne                                            
026200     End-If                                                               
026300     Move cob-expl    To pcsexpl                                          
026400* SÄTT NAMN På GRUPPERNA                                                  
026500     Move Space To keydata-grp                                            
026600     Unstring cob-ykeys Delimited By All Space                            
026700         Into keydata(1)                                                  
026800              keydata(2)                                                  
026900              keydata(3)                                                  
027000              keydata(4)                                                  
027100              keydata(5)                                                  
027200              keydata(6)                                                  
027300              keydata(7)                                                  
027400              keydata(8)                                                  
027500              keydata(9)                                                  
027600              keydata(10)                                                 
027700              keydata(11)                                                 
027800              keydata(12)                                                 
027900              keydata(13)                                                 
028000              keydata(14)                                                 
028100              keydata(15)                                                 
028200              keydata(16)                                                 
028300              keydata(17)                                                 
028400              keydata(18)                                                 
028500              keydata(19)                                                 
028600              keydata(20)                                                 
028700     Move Zero To testfalt                                                
028800     Move 99 To ant-sp                                                    
028900     Perform Varying IX From 1 By 1 Until ix > 20                         
029000       Inspect keydata(ix) Tallying testfalt For All ' '                  
029100       If testfalt < ant-sp                                               
029200         Move testfalt To ant-sp                                          
029300       End-If                                                             
029400       Move Zero To testfalt                                              
029500     End-Perform                                                          
029600     Compute pcskeyl = 25 - ant-sp                                        
029700     Move +1 To point ptr                                                 
029800     Move keydata(1) To keys                                              
029900     Perform Varying IX From 2 By 1 Until ix > 20                         
030000       Add pcskeyl To ptr                                                 
030100       Move ptr To point                                                  
030200       String keydata(ix) Delimited By Space Into keys                    
030300          With Pointer point                                              
030400     End-Perform                                                          
030500     Inspect keys Replacing All '_' By Space                              
030600*                                                                         
030700*SÄTT NAMN PÅ LABLAR                                                      
030800     If cob-xlabell = 0                                                   
030900       Move Space To xpkod-grp                                            
031000       Unstring cob-xlabels Delimited By All Space                        
031100           Into xpkod(1)                                                  
031200                xpkod(2)                                                  
031300                xpkod(3)                                                  
031400                xpkod(4)                                                  
031500                xpkod(5)                                                  
031600                xpkod(6)                                                  
031700                xpkod(7)                                                  
031800                xpkod(8)                                                  
031900                xpkod(9)                                                  
032000                xpkod(10)                                                 
032100                xpkod(11)                                                 
032200                xpkod(12)                                                 
032300                xpkod(13)                                                 
032400                xpkod(14)                                                 
032500                xpkod(15)                                                 
032600                xpkod(16)                                                 
032700                xpkod(17)                                                 
032800                xpkod(18)                                                 
032900                xpkod(19)                                                 
033000                xpkod(20)                                                 
033100                xpkod(21)                                                 
033200                xpkod(22)                                                 
033300                xpkod(23)                                                 
033400                xpkod(24)                                                 
033500                xpkod(25)                                                 
033600                xpkod(26)                                                 
033700                xpkod(27)                                                 
033800                xpkod(28)                                                 
033900                xpkod(29)                                                 
034000                xpkod(30)                                                 
034100                xpkod(31)                                                 
034200                xpkod(32)                                                 
034300                xpkod(33)                                                 
034400                xpkod(34)                                                 
034500                xpkod(35)                                                 
034600                xpkod(36)                                                 
034700                xpkod(37)                                                 
034800                xpkod(38)                                                 
034900                xpkod(39)                                                 
035000                xpkod(40)                                                 
035100                xpkod(41)                                                 
035200                xpkod(42)                                                 
035300                xpkod(43)                                                 
035400                xpkod(44)                                                 
035500                xpkod(45)                                                 
035600                xpkod(46)                                                 
035700                xpkod(47)                                                 
035800                xpkod(48)                                                 
035900                xpkod(49)                                                 
036000                xpkod(40)                                                 
036100                xpkod(50)                                                 
036200                xpkod(51)                                                 
036300                xpkod(52)                                                 
036400                xpkod(53)                                                 
036500                xpkod(54)                                                 
036600       Move Zero To testfalt                                              
036700       Move 99 To ant-sp                                                  
036800       Perform Varying IX From 1 By 1 Until ix > 54                       
036900         Inspect xpkod(ix) Tallying testfalt For All ' '                  
037000         If testfalt < ant-sp                                             
037100           Move testfalt To ant-sp                                        
037200         End-If                                                           
037300         Move Zero To testfalt                                            
037400       End-Perform                                                        
037500       Compute pcslabl = xpkod-lengd - ant-sp                             
037600       Move +1 To point ptr                                               
037700       Move xpkod(1) To xlabels                                           
037800       Perform Varying IX From 2 By 1 Until ix > 54                       
037900         Add pcslabl To ptr                                               
038000         Move ptr To point                                                
038100         String xpkod(ix) Delimited By Space Into xlabels                 
038200            With Pointer point                                            
038300       End-Perform                                                        
038400       Inspect xlabels Replacing All '_' By Space                         
038500     Else                                                                 
038600       Move cob-xlabels To xlabels                                        
038700       Move cob-xlabell To pcslabl                                        
038800     End-If                                                               
038900                                                                          
039000     Move cob-header  To chart-head                                       
039100     Move cob-headerl To pcsheadl                                         
039200                                                                          
039300     Move cob-xcol    To ispf-xvalue                                      
039400     Unstring cob-ycols Delimited By All Space                            
039500         Into ispf-yvalue1                                                
039600              ispf-yvalue2                                                
039700              ispf-yvalue3                                                
039800              ispf-yvalue4                                                
039900              ispf-yvalue5                                                
040000              ispf-yvalue6                                                
040100              ispf-yvalue7                                                
040200              ispf-yvalue8                                                
040300              ispf-yvalue9                                                
040400              ispf-yvalue10                                               
040500              ispf-yvalue11                                               
040600              ispf-yvalue12                                               
040700              ispf-yvalue13                                               
040800              ispf-yvalue14                                               
040900              ispf-yvalue15                                               
041000              ispf-yvalue16                                               
041100              ispf-yvalue17                                               
041200              ispf-yvalue18                                               
041300              ispf-yvalue19                                               
041400              ispf-yvalue20                                               
041500     Perform AB-VDEFINE                                                   
041600     .                                                                    
041700     eject                                                                
041800 AA-VDEFINE Section.                                                      
041900     Move Length Of cob-table      To len                                 
042000     Call 'ISPLINK' Using vdefine ispf-table  cob-table                   
042100                                  char len v-opt                          
042200     Move Length Of cob-disp       To len                                 
042300     Call 'ISPLINK' Using vdefine ispf-disp   cob-disp                    
042400                                  char len v-opt                          
042500     Move Length Of cob-help       To len                                 
042600     Call 'ISPLINK' Using vdefine ispf-help   cob-help                    
042700                                  char len v-opt                          
042800     Move Length Of cob-isol       To len                                 
042900     Call 'ISPLINK' Using vdefine ispf-isol   cob-isol                    
043000                                  char len v-opt                          
043100     Move Length Of cob-format     To len                                 
043200     Call 'ISPLINK' Using vdefine ispf-format cob-format                  
043300                                  char len v-opt                          
043400     Move Length Of cob-data       To len                                 
043500     Call 'ISPLINK' Using vdefine ispf-data   cob-data                    
043600                                  char len v-opt                          
043700     Move Length Of cob-expl       To len                                 
043800     Call 'ISPLINK' Using vdefine ispf-expl   cob-expl                    
043900                                  char len v-opt                          
044000     Move Length Of cob-header     To len                                 
044100     Call 'ISPLINK' Using vdefine ispf-header cob-header                  
044200                                  char len v-opt                          
044300     Move +4                       To len                                 
044400     Call 'ISPLINK' Using vdefine ispf-headerl cob-headerl                
044500                                  fixed len v-opt                         
044600     Move Length Of cob-xlabels    To len                                 
044700     Call 'ISPLINK' Using vdefine ispf-xlabels cob-xlabels                
044800                                  char len v-opt                          
044900     Move +4                       To len                                 
045000     Call 'ISPLINK' Using vdefine ispf-xlabell cob-xlabell                
045100                                  fixed len v-opt                         
045200     Move Length Of cob-ykeys      To len                                 
045300     Call 'ISPLINK' Using vdefine ispf-ykeys   cob-ykeys                  
045400                                  char len v-opt                          
045500     Move Length Of cob-xcol       To len                                 
045600     Call 'ISPLINK' Using vdefine ispf-xcol    cob-xcol                   
045700                                  char len v-opt                          
045800     Move Length Of cob-ycols      To len                                 
045900     Call 'ISPLINK' Using vdefine ispf-ycols   cob-ycols                  
046000                                  char len v-opt                          
046100     Move +4                       To len                                 
046200     Call 'ISPLINK' Using vdefine ispf-ycolsn  cob-ycolsn                 
046300                                  fixed len v-opt                         
046400     Move +4                       To len                                 
046500     Call 'ISPLINK' Using vdefine ispf-rows    cob-rows                   
046600                                  fixed len v-opt                         
046700     Move Length Of cob-msg        To len                                 
046800     Call 'ISPLINK' Using vdefine ispf-msg     cob-msg                    
046900                                  char len v-opt                          
047000     Move +1                       To len                                 
047100     Call 'ISPLINK' Using vdefine ispf-formdel cob-formdel                
047200                                  char len v-opt                          
047300     Move +8                       To len                                 
047400     Call 'ISPLINK' Using vdefine ispf-output  cob-output                 
047500                                  char len v-opt                          
047600     Move +4                       To len                                 
047700     Call 'ISPLINK' Using vdefine ispf-copies  cob-copies                 
047800                                  fixed len v-opt                         
047900     .                                                                    
048000     eject                                                                
048100 AB-VDEFINE Section.                                                      
048200                                                                          
048300     Move Length Of cob-xvalue     To len                                 
048400     Call 'ISPLINK' Using vdefine ispf-xvalue  cob-xvalue                 
048500                                  char len v-opt                          
048600     Move 16                       To len                                 
048700     Call 'ISPLINK' Using vdefine ispf-yvalue1  cob-yvalue(1)             
048800                                  char  len v-opt                         
048900     Call 'ISPLINK' Using vdefine ispf-yvalue2  cob-yvalue(2)             
049000                                  char  len v-opt                         
049100     Call 'ISPLINK' Using vdefine ispf-yvalue3  cob-yvalue(3)             
049200                                  char  len v-opt                         
049300     Call 'ISPLINK' Using vdefine ispf-yvalue4  cob-yvalue(4)             
049400                                  char  len v-opt                         
049500     Call 'ISPLINK' Using vdefine ispf-yvalue5  cob-yvalue(5)             
049600                                  char  len v-opt                         
049700     Call 'ISPLINK' Using vdefine ispf-yvalue6  cob-yvalue(6)             
049800                                  char  len v-opt                         
049900     Call 'ISPLINK' Using vdefine ispf-yvalue7  cob-yvalue(7)             
050000                                  char  len v-opt                         
050100     Call 'ISPLINK' Using vdefine ispf-yvalue8  cob-yvalue(8)             
050200                                  char  len v-opt                         
050300     Call 'ISPLINK' Using vdefine ispf-yvalue9  cob-yvalue(9)             
050400                                  char  len v-opt                         
050500     Call 'ISPLINK' Using vdefine ispf-yvalue10 cob-yvalue(10)            
050600                                  char  len v-opt                         
050700     Call 'ISPLINK' Using vdefine ispf-yvalue11 cob-yvalue(11)            
050800                                  char  len v-opt                         
050900     Call 'ISPLINK' Using vdefine ispf-yvalue12 cob-yvalue(12)            
051000                                  char  len v-opt                         
051100     Call 'ISPLINK' Using vdefine ispf-yvalue13 cob-yvalue(13)            
051200                                  char  len v-opt                         
051300     Call 'ISPLINK' Using vdefine ispf-yvalue14 cob-yvalue(14)            
051400                                  char  len v-opt                         
051500     Call 'ISPLINK' Using vdefine ispf-yvalue15 cob-yvalue(15)            
051600                                  char  len v-opt                         
051700     Call 'ISPLINK' Using vdefine ispf-yvalue16 cob-yvalue(16)            
051800                                  char  len v-opt                         
051900     Call 'ISPLINK' Using vdefine ispf-yvalue17 cob-yvalue(17)            
052000                                  char  len v-opt                         
052100     Call 'ISPLINK' Using vdefine ispf-yvalue18 cob-yvalue(18)            
052200                                  char  len v-opt                         
052300     Call 'ISPLINK' Using vdefine ispf-yvalue19 cob-yvalue(19)            
052400                                  char  len v-opt                         
052500     Call 'ISPLINK' Using vdefine ispf-yvalue20 cob-yvalue(20)            
052600                                  char  len v-opt                         
052700     .                                                                    
052800     eject                                                                
052900 B-LAS-GRAF-DATA Section.                                                 
053000                                                                          
053100     Move Zero To ix-xrad                                                 
053200     Call 'ISPLINK' Using tbtop cob-table                                 
053300     Call 'ISPLINK' Using tbskip cob-table                                
053400     Move return-code To rc                                               
053500     If rc < 8                                                            
053600       Perform Until rc > 7 Or ix-xrad > 54 Or grafdata-ok = nej          
053700         Add +1 To ix-xrad                                                
053800         Perform BA-BEHANDLA-RAD                                          
053900         Call 'ISPLINK' Using tbskip cob-table                            
054000         Move return-code To rc                                           
054100       End-Perform                                                        
054200     Else                                                                 
054300       Move nej To grafdata-ok                                            
054400       Move 'TABELLEN TOM' To cob-msg                                     
054500**     TABELLEN TOM                                                       
054600     End-If                                                               
054700     .                                                                    
054800     eject                                                                
054900 BA-BEHANDLA-RAD Section.                                                 
055000                                                                          
055100     Move 1  To doix                                                      
055200     Perform Until doix > cob-ycolsn                                      
055300       Move cob-yvalue(doix) To dec-idfridata                             
055400       Move 11 To dec-kvheltal                                            
055500       Move 4  To dec-kvdecimal                                           
055600       If cob-formdel = '.' Or ' '                                        
055700         Move 'P' To dec-kdsvar                                           
055800       Else If cob-formdel = ','                                          
055900         Move 'K' To dec-kdsvar                                           
056000       Else If cob-formdel = ':'                                          
056100         Move '%' To dec-kdsvar                                           
056200       Else                                                               
056300         Move nej To grafdata-ok                                          
056400         Move 'Illegal decimal delimiter specified' To cob-msg            
056500         Move 99 To doix                                                  
056600       End-If                                                             
056700       End-If                                                             
056800       End-If                                                             
056900       Call 'WDECEDIT' Using dec-wdecarea                                 
057000       If dec-kdsvar = 'F'                                                
057100         Move nej To grafdata-ok                                          
057200         String 'Column ' doix ' contains invalid data'                   
057300           Delimited By Size                                              
057400           Into cob-msg                                                   
057500         Move 99 To doix                                                  
057600       Else                                                               
057700         Move dec-ideditdata    To y-temp(doix, ix-xrad)                  
057800       End-If                                                             
057900       Add +1 To doix                                                     
058000     End-Perform                                                          
058100     .                                                                    
058200     eject                                                                
058300 C-REDIGERA-GRAFDATA Section.                                             
058400                                                                          
058500     Move +1 To doix1 doix2 ix3                                           
058600     Perform Until doix1 > cob-ycolsn                                     
058700       Perform Until doix2 > pcsne                                        
058800         Move y-temp(doix1 doix2) To y-value(ix3)                         
058900         Add +1 To doix2 ix3                                              
059000       End-Perform                                                        
059100       Move +1 To doix2                                                   
059200       Add +1 To doix1                                                    
059300     End-Perform                                                          
059400     .                                                                    
059500     eject                                                                
059600 X-VISA-GRAF Section.                                                     
059700                                                                          
059800     Call 'FSINIT'                                                        
059900     Move +2 To len                                                       
060000     Call 'ESLIB' Using type-1 len ddsymbl                                
060100     Call 'ESLIB' Using type-4 len ddform                                 
060200     Call 'ESLIB' Using type-5 len dddata                                 
060300     Call 'ESLIB' Using type-7 len ddgdf                                  
060400                                                                          
060500                                                                          
060600     Call 'CHART' Using admtpcs, dummy,                                   
060700                        xdata, ydata,                                     
060800                        keys, xlabels, chart-head                         
060900                                                                          
061000                                                                          
061100     Call 'FSTERM'                                                        
061200                                                                          
061300     Call 'ISPLINK' Using isp-control isp-display refresh                 
061400     .                                                                    
