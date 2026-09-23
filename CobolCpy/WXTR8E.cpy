000100 01  WXTR8E.                                                              
000200*                                 PRIMƒREXTRAKT                           
000300*                                 FIL MED RETURINFO                       
000400*                                 LEDTIDS FIL                             
000500*                                 PRIMARY EXTRACT                         
000600*                                 RETURN INFORMATION                      
000700*                                 LEADTIME                                
000800     03 IDDC-RET             PIC X(2).                                    
000900*                                 MOTTAGANDE LAGER F÷R RETURER            
001000*                                 RECEIVING WAREHOUSE FOR RETURNS         
001100     03 IDDISTR              PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 IDFTG                PIC 9(2).                                    
001500*                                 F÷RETAGSID EKONOM REDOVISNING           
001600*                                 COMPANY IDENTITY ACCOUNTING             
001700     03 IDKUNDNR             PIC 9(6).                                    
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 IDRAPPNR             PIC 9(7).                                    
002100*                                 RAPPORT NUMMER                          
002200*                                 DISCREPANCY REPORT NUMBER               
002300     03 IDRT                 PIC X(3).                                    
002400*                                 RETURTERMINAL                           
002500*                                 RETURN TERMINAL                         
002600     03 TISAAPP-REMISS.                                                   
002700*                                 TIREMISS I ANNAT DATUMFORMAT            
002800        05 TISAAPP-REMISS-TISEKEL                                         
002900                             PIC 9(2).                                    
003000*                                 SEKEL I ≈RTALET                         
003100*                                 CENTURY                                 
003200        05 TISAAPP-REMISS-TIAAPP                                          
003300                             PIC 9(4).                                    
003400*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
003500*                                 12 PER ≈R                               
003600*                                 YEAR - PLANNING PERIOD (YYPP)           
003700*                                 12 PER YEAR                             
003800     03 TISAAPP-RETILL.                                                   
003900*                                 TIRETILL I ANNAT DATUMFORMAT            
004000        05 TISAAPP-RETILL-TISEKEL                                         
004100                             PIC 9(2).                                    
004200*                                 SEKEL I ≈RTALET                         
004300*                                 CENTURY                                 
004400        05 TISAAPP-RETILL-TIAAPP                                          
004500                             PIC 9(4).                                    
004600*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
004700*                                 12 PER ≈R                               
004800*                                 YEAR - PLANNING PERIOD (YYPP)           
004900*                                 12 PER YEAR                             
005000     03 TISAAPP-INLINL.                                                   
005100*                                 TIINLINL I ANNAT DATUMFORMAT            
005200        05 TISAAPP-INLINL-TISEKEL                                         
005300                             PIC 9(2).                                    
005400*                                 SEKEL I ≈RTALET                         
005500*                                 CENTURY                                 
005600        05 TISAAPP-INLINL-TIAAPP                                          
005700                             PIC 9(4).                                    
005800*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
005900*                                 12 PER ≈R                               
006000*                                 YEAR - PLANNING PERIOD (YYPP)           
006100*                                 12 PER YEAR                             
006200     03 TISAAPP-KNOTA.                                                    
006300*                                 TIKNOTA I ANNAT DATUMFORMAT             
006400        05 TISAAPP-KNOTA-TISEKEL                                          
006500                             PIC 9(2).                                    
006600*                                 SEKEL I ≈RTALET                         
006700*                                 CENTURY                                 
006800        05 TISAAPP-KNOTA-TIAAPP                                           
006900                             PIC 9(4).                                    
007000*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
007100*                                 12 PER ≈R                               
007200*                                 YEAR - PLANNING PERIOD (YYPP)           
007300*                                 12 PER YEAR                             
007400     03 TISAAPP-SNDDAT.                                                   
007500*                                 TISNDDAT I ANNAT DATUMFORMAT            
007600        05 TISAAPP-SNDDAT-TISEKEL                                         
007700                             PIC 9(2).                                    
007800*                                 SEKEL I ≈RTALET                         
007900*                                 CENTURY                                 
008000        05 TISAAPP-SNDDAT-TIAAPP                                          
008100                             PIC 9(4).                                    
008200*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
008300*                                 12 PER ≈R                               
008400*                                 YEAR - PLANNING PERIOD (YYPP)           
008500*                                 12 PER YEAR                             
008600     03 TISAAPP-RETANK.                                                   
008700*                                 TIRETANK I ANNAT DATUMFORMAT            
008800        05 TISAAPP-RETANK-TISEKEL                                         
008900                             PIC 9(2).                                    
009000*                                 SEKEL I ≈RTALET                         
009100*                                 CENTURY                                 
009200        05 TISAAPP-RETANK-TIAAPP                                          
009300                             PIC 9(4).                                    
009400*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
009500*                                 12 PER ≈R                               
009600*                                 YEAR - PLANNING PERIOD (YYPP)           
009700*                                 12 PER YEAR                             
009800     03 TISAAVV-REMISS.                                                   
009900*                                 TIREMISS I ANNAT DATUMFORMAT            
010000        05 TISAAVV-REMISS-TISEKEL                                         
010100                             PIC 9(2).                                    
010200*                                 SEKEL I ≈RTALET                         
010300*                                 CENTURY                                 
010400        05 TISAAVV-REMISS-TIAAVV                                          
010500                             PIC 9(4).                                    
010600*                                 ≈R - VECKA  (≈≈VV)                      
010700*                                 YEAR - WEEK  (YYWW)                     
010800     03 TISAAVV-RETILL.                                                   
010900*                                 TIRETILL I ANNAT DATUMFORMAT            
011000        05 TISAAVV-RETILL-TISEKEL                                         
011100                             PIC 9(2).                                    
011200*                                 SEKEL I ≈RTALET                         
011300*                                 CENTURY                                 
011400        05 TISAAVV-RETILL-TIAAVV                                          
011500                             PIC 9(4).                                    
011600*                                 ≈R - VECKA  (≈≈VV)                      
011700*                                 YEAR - WEEK  (YYWW)                     
011800     03 TISAAVV-INLINL.                                                   
011900*                                 TIINLINL I ANNAT DATUMFORMAT            
012000        05 TISAAVV-INLINL-TISEKEL                                         
012100                             PIC 9(2).                                    
012200*                                 SEKEL I ≈RTALET                         
012300*                                 CENTURY                                 
012400        05 TISAAVV-INLINL-TIAAVV                                          
012500                             PIC 9(4).                                    
012600*                                 ≈R - VECKA  (≈≈VV)                      
012700*                                 YEAR - WEEK  (YYWW)                     
012800     03 TISAAVV-KNOTA.                                                    
012900*                                 TIKNOTA I ANNAT DATUMFORMAT             
013000        05 TISAAVV-KNOTA-TISEKEL                                          
013100                             PIC 9(2).                                    
013200*                                 SEKEL I ≈RTALET                         
013300*                                 CENTURY                                 
013400        05 TISAAVV-KNOTA-TIAAVV                                           
013500                             PIC 9(4).                                    
013600*                                 ≈R - VECKA  (≈≈VV)                      
013700*                                 YEAR - WEEK  (YYWW)                     
013800     03 TISAAVV-SNDDAT.                                                   
013900*                                 TISNDDAT I ANNAT DATUMFORMAT            
014000        05 TISAAVV-SNDDAT-TISEKEL                                         
014100                             PIC 9(2).                                    
014200*                                 SEKEL I ≈RTALET                         
014300*                                 CENTURY                                 
014400        05 TISAAVV-SNDDAT-TIAAVV                                          
014500                             PIC 9(4).                                    
014600*                                 ≈R - VECKA  (≈≈VV)                      
014700*                                 YEAR - WEEK  (YYWW)                     
014800     03 TISAAVV-RETANK.                                                   
014900*                                 TIRETANK I ANNAT DATUMFORMAT            
015000        05 TISAAVV-RETANK-TISEKEL                                         
015100                             PIC 9(2).                                    
015200*                                 SEKEL I ≈RTALET                         
015300*                                 CENTURY                                 
015400        05 TISAAVV-RETANK-TIAAVV                                          
015500                             PIC 9(4).                                    
015600*                                 ≈R - VECKA  (≈≈VV)                      
015700*                                 YEAR - WEEK  (YYWW)                     
015800     03 KDANMORS             PIC X(2).                                    
015900*                                 ORSAK TILL LEVERANSANMƒRKNING           
016000*                                 DISCREPANCY REPORT REASON CODE          
016100     03 KDARBTYP-ADM         PIC X(8).                                    
016200*                                 ANSVARIG LEVERANSANMƒRKNINGSAVD         
016300*                                 RESPONSIBLE AT DISCREPANCYDEPT          
016400     03 IDPERSON-ADM         PIC 9(3).                                    
016500*                                 PERSONKOD LEVANM                        
016600*                                 STAFF CODE DISCREPANCY                  
016700     03 KDARBTYP-REM         PIC X(8).                                    
016800*                                 REMISSANSVARIG LEVANM                   
016900*                                 PERSON WHO CONSIDERED DISCR.            
017000     03 IDPERSON-REM         PIC 9(3).                                    
017100*                                 PERSONKOD REMISS                        
017200*                                 STAFF CODE CONSIDERATION                
017300     03 KDARBTYP-RET         PIC X(8).                                    
017400*                                 ANSVARIG RETURAVDELNINGEN               
017500*                                 RESPONSIBLE AT RETURNDEPARTMENT         
017600     03 IDPERSON-RET         PIC 9(3).                                    
017700*                                 PERSONKOD RETURAVD.                     
017800*                                 STAFF CODE RETURN DEPT.                 
017900     03 KVDAGAR-ADM          PIC 9(2).                                    
018000*                                 NO. OF DAYS ADMINISTRATION FOR          
018100*                                 A DESCRAPENCY REPORT                    
018200     03 KVDAGAR-REM          PIC 9(2).                                    
018300*                                 NO. OF DAYS REMITTING FOR               
018400*                                 A DESCRAPENCY REPORT                    
018500     03 KVDAGAR-RET          PIC 9(2).                                    
018600*                                 NO. OF DAYS RETURN HANDLING FOR         
018700*                                 A DESCRAPENCY REPORT                    
018800     03 KVDAGAR-INL          PIC 9(2).                                    
018900*                                 NO. OF DAYS BINNING FOR                 
019000*                                 A DESCRAPENCY REPORT                    
019100     03 KVDAGAR-TERM         PIC 9(2).                                    
019200*                                 NO. OF DAYS A DESCRAPENCY               
019300*                                 REPORT HAS BIN IN A RETURN-             
019400*                                 TERMINAL                                
019500     03 KVDAGAR-LOSSN        PIC 9(2).                                    
019600*                                 NO. OF DAYS UNLOADING FOR               
019700*                                 A DESCRAPENCY REPORT                    
019800*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
