000100 01  W42822.                                                              
000200*                                 FIL MED RETURINFO                       
000300*                                 LEDTIDS FIL                             
000400*                                 RETURN INFORMATION                      
000500     03 IDDC-RET             PIC X(2).                                    
000600*                                 MOTTAGANDE LAGER F÷R RETURER            
000700*                                 RECEIVING WAREHOUSE FOR RETURNS         
000800     03 IDDISTR              PIC 9(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100     03 IDFTG                PIC 9(2).                                    
001200*                                 F÷RETAGSID EKONOM REDOVISNING           
001300*                                 COMPANY IDENTITY ACCOUNTING             
001400     03 IDKUNDNR             PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 IDRAPPNR             PIC 9(7).                                    
001800*                                 RAPPORT NUMMER                          
001900*                                 DISCREPANCY REPORT NUMBER               
002000     03 IDRT                 PIC X(3).                                    
002100*                                 RETURTERMINAL                           
002200*                                 RETURN TERMINAL                         
002300     03 TISAAPP-REMISS.                                                   
002400*                                 TIREMISS I ANNAT DATUMFORMAT            
002500        05 TISAAPP-REMISS-TISEKEL                                         
002600                             PIC 9(2).                                    
002700*                                 SEKEL I ≈RTALET                         
002800*                                 CENTURY                                 
002900        05 TISAAPP-REMISS-TIAAPP                                          
003000                             PIC 9(4).                                    
003100*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
003200*                                 12 PER ≈R                               
003300*                                 YEAR - PLANNING PERIOD (YYPP)           
003400*                                 12 PER YEAR                             
003500     03 TISAAPP-RETILL.                                                   
003600*                                 TIRETILL I ANNAT DATUMFORMAT            
003700        05 TISAAPP-RETILL-TISEKEL                                         
003800                             PIC 9(2).                                    
003900*                                 SEKEL I ≈RTALET                         
004000*                                 CENTURY                                 
004100        05 TISAAPP-RETILL-TIAAPP                                          
004200                             PIC 9(4).                                    
004300*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
004400*                                 12 PER ≈R                               
004500*                                 YEAR - PLANNING PERIOD (YYPP)           
004600*                                 12 PER YEAR                             
004700     03 TISAAPP-INLINL.                                                   
004800*                                 TIINLINL I ANNAT DATUMFORMAT            
004900        05 TISAAPP-INLINL-TISEKEL                                         
005000                             PIC 9(2).                                    
005100*                                 SEKEL I ≈RTALET                         
005200*                                 CENTURY                                 
005300        05 TISAAPP-INLINL-TIAAPP                                          
005400                             PIC 9(4).                                    
005500*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
005600*                                 12 PER ≈R                               
005700*                                 YEAR - PLANNING PERIOD (YYPP)           
005800*                                 12 PER YEAR                             
005900     03 TISAAPP-KNOTA.                                                    
006000*                                 TIKNOTA I ANNAT DATUMFORMAT             
006100        05 TISAAPP-KNOTA-TISEKEL                                          
006200                             PIC 9(2).                                    
006300*                                 SEKEL I ≈RTALET                         
006400*                                 CENTURY                                 
006500        05 TISAAPP-KNOTA-TIAAPP                                           
006600                             PIC 9(4).                                    
006700*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
006800*                                 12 PER ≈R                               
006900*                                 YEAR - PLANNING PERIOD (YYPP)           
007000*                                 12 PER YEAR                             
007100     03 TISAAPP-SNDDAT.                                                   
007200*                                 TISNDDAT I ANNAT DATUMFORMAT            
007300        05 TISAAPP-SNDDAT-TISEKEL                                         
007400                             PIC 9(2).                                    
007500*                                 SEKEL I ≈RTALET                         
007600*                                 CENTURY                                 
007700        05 TISAAPP-SNDDAT-TIAAPP                                          
007800                             PIC 9(4).                                    
007900*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
008000*                                 12 PER ≈R                               
008100*                                 YEAR - PLANNING PERIOD (YYPP)           
008200*                                 12 PER YEAR                             
008300     03 TISAAPP-RETANK.                                                   
008400*                                 TIRETANK I ANNAT DATUMFORMAT            
008500        05 TISAAPP-RETANK-TISEKEL                                         
008600                             PIC 9(2).                                    
008700*                                 SEKEL I ≈RTALET                         
008800*                                 CENTURY                                 
008900        05 TISAAPP-RETANK-TIAAPP                                          
009000                             PIC 9(4).                                    
009100*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
009200*                                 12 PER ≈R                               
009300*                                 YEAR - PLANNING PERIOD (YYPP)           
009400*                                 12 PER YEAR                             
009500     03 TISAAVV-REMISS.                                                   
009600*                                 TIREMISS I ANNAT DATUMFORMAT            
009700        05 TISAAVV-REMISS-TISEKEL                                         
009800                             PIC 9(2).                                    
009900*                                 SEKEL I ≈RTALET                         
010000*                                 CENTURY                                 
010100        05 TISAAVV-REMISS-TIAAVV                                          
010200                             PIC 9(4).                                    
010300*                                 ≈R - VECKA  (≈≈VV)                      
010400*                                 YEAR - WEEK  (YYWW)                     
010500     03 TISAAVV-RETILL.                                                   
010600*                                 TIRETILL I ANNAT DATUMFORMAT            
010700        05 TISAAVV-RETILL-TISEKEL                                         
010800                             PIC 9(2).                                    
010900*                                 SEKEL I ≈RTALET                         
011000*                                 CENTURY                                 
011100        05 TISAAVV-RETILL-TIAAVV                                          
011200                             PIC 9(4).                                    
011300*                                 ≈R - VECKA  (≈≈VV)                      
011400*                                 YEAR - WEEK  (YYWW)                     
011500     03 TISAAVV-INLINL.                                                   
011600*                                 TIINLINL I ANNAT DATUMFORMAT            
011700        05 TISAAVV-INLINL-TISEKEL                                         
011800                             PIC 9(2).                                    
011900*                                 SEKEL I ≈RTALET                         
012000*                                 CENTURY                                 
012100        05 TISAAVV-INLINL-TIAAVV                                          
012200                             PIC 9(4).                                    
012300*                                 ≈R - VECKA  (≈≈VV)                      
012400*                                 YEAR - WEEK  (YYWW)                     
012500     03 TISAAVV-KNOTA.                                                    
012600*                                 TIKNOTA I ANNAT DATUMFORMAT             
012700        05 TISAAVV-KNOTA-TISEKEL                                          
012800                             PIC 9(2).                                    
012900*                                 SEKEL I ≈RTALET                         
013000*                                 CENTURY                                 
013100        05 TISAAVV-KNOTA-TIAAVV                                           
013200                             PIC 9(4).                                    
013300*                                 ≈R - VECKA  (≈≈VV)                      
013400*                                 YEAR - WEEK  (YYWW)                     
013500     03 TISAAVV-SNDDAT.                                                   
013600*                                 TISNDDAT I ANNAT DATUMFORMAT            
013700        05 TISAAVV-SNDDAT-TISEKEL                                         
013800                             PIC 9(2).                                    
013900*                                 SEKEL I ≈RTALET                         
014000*                                 CENTURY                                 
014100        05 TISAAVV-SNDDAT-TIAAVV                                          
014200                             PIC 9(4).                                    
014300*                                 ≈R - VECKA  (≈≈VV)                      
014400*                                 YEAR - WEEK  (YYWW)                     
014500     03 TISAAVV-RETANK.                                                   
014600*                                 TIRETANK I ANNAT DATUMFORMAT            
014700        05 TISAAVV-RETANK-TISEKEL                                         
014800                             PIC 9(2).                                    
014900*                                 SEKEL I ≈RTALET                         
015000*                                 CENTURY                                 
015100        05 TISAAVV-RETANK-TIAAVV                                          
015200                             PIC 9(4).                                    
015300*                                 ≈R - VECKA  (≈≈VV)                      
015400*                                 YEAR - WEEK  (YYWW)                     
015500     03 KDANMORS             PIC X(2).                                    
015600*                                 ORSAK TILL LEVERANSANMƒRKNING           
015700*                                 DISCREPANCY REPORT REASON CODE          
015800     03 KDARBTYP-ADM         PIC X(8).                                    
015900*                                 ANSVARIG LEVERANSANMƒRKNINGSAVD         
016000*                                 RESPONSIBLE AT DISCREPANCYDEPT          
016100     03 IDPERSON-ADM         PIC 9(3).                                    
016200*                                 PERSONKOD LEVANM                        
016300*                                 STAFF CODE DISCREPANCY                  
016400     03 KDARBTYP-REM         PIC X(8).                                    
016500*                                 REMISSANSVARIG LEVANM                   
016600*                                 PERSON WHO CONSIDERED DISCR.            
016700     03 IDPERSON-REM         PIC 9(3).                                    
016800*                                 PERSONKOD REMISS                        
016900*                                 STAFF CODE CONSIDERATION                
017000     03 KDARBTYP-RET         PIC X(8).                                    
017100*                                 ANSVARIG RETURAVDELNINGEN               
017200*                                 RESPONSIBLE AT RETURNDEPARTMENT         
017300     03 IDPERSON-RET         PIC 9(3).                                    
017400*                                 PERSONKOD RETURAVD.                     
017500*                                 STAFF CODE RETURN DEPT.                 
017600     03 KVDAGAR-ADM          PIC 9(2).                                    
017700*                                 NO. OF DAYS ADMINISTRATION FOR          
017800*                                 A DESCRAPENCY REPORT                    
017900     03 KVDAGAR-REM          PIC 9(2).                                    
018000*                                 NO. OF DAYS REMITTING FOR               
018100*                                 A DESCRAPENCY REPORT                    
018200     03 KVDAGAR-RET          PIC 9(2).                                    
018300*                                 NO. OF DAYS RETURN HANDLING FOR         
018400*                                 A DESCRAPENCY REPORT                    
018500     03 KVDAGAR-INL          PIC 9(2).                                    
018600*                                 NO. OF DAYS BINNING FOR                 
018700*                                 A DESCRAPENCY REPORT                    
018800     03 KVDAGAR-TERM         PIC 9(2).                                    
018900*                                 NO. OF DAYS A DESCRAPENCY               
019000*                                 REPORT HAS BIN IN A RETURN-             
019100*                                 TERMINAL                                
019200     03 KVDAGAR-LOSSN        PIC 9(2).                                    
019300*                                 NO. OF DAYS UNLOADING FOR               
019400*                                 A DESCRAPENCY REPORT                    
019500*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
