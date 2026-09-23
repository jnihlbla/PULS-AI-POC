000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ20TIME.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/04/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*    CONVERTS A DATE + TIME TO NUMBER OF SECONDS SINCE                    
001000*    OCTOBER 15, 1582.                                                    
001300*    THE DATE AND TIME SHOULD BE GIVEN AS A STRING IN THE                 
001400*    FORMAT "YYYYMMDDHHMMSS"                                              
001900*                                                                         
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400     EJECT                                                                
002500 01  CEEISEC         PIC X(8) VALUE 'CEEISEC'.                            
002700                                                                          
002800 01  W-DATE-TIME-IN.                                                      
002900     03 W-YYYY-IN    PIC 9999.                                            
003000     03 W-MO-IN      PIC 99.                                              
003010     03 W-DD-IN      PIC 99.                                              
003020     03 W-HH-IN      PIC 99.                                              
003030     03 W-MI-IN      PIC 99.                                              
003040     03 W-SS-IN      PIC 99.                                              
003300                                                                          
003400 01  W-YYYY-COMP     PIC S9(9) BINARY.                                    
003500 01  W-MO-COMP       PIC S9(9) BINARY.                                    
003600 01  W-DD-COMP       PIC S9(9) BINARY.                                    
003700 01  W-HH-COMP       PIC S9(9) BINARY.                                    
003710 01  W-MI-COMP       PIC S9(9) BINARY.                                    
003720 01  W-SS-COMP       PIC S9(9) BINARY.                                    
003721 01  W-MILLI-COMP    PIC S9(9) BINARY   VALUE ZERO.                       
003730                                                                          
003740 01  W-SECONDS-OUT   COMP-2.                                              
003900                                                                          
004000 01  FBC.                                                                 
004100     03 SEV          PIC S9(4) BINARY.                                    
004200     03 MSGNO        PIC S9(4) BINARY.                                    
004300     03 FILLER       PIC X(8).                                            
004400                                                                          
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010700*01 -COPY WZ20TIME                                                        
010800                                                                          
010900*                                                                         
011000 PROCEDURE DIVISION USING TIME-WZ20TIME.                                  
011100                                                                          
011200     PERFORM A-CHECK-PREPARE                                              
011400     PERFORM B-DATE-TIME-TO-SECONDS                                       
011900                                                                          
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300                                                                          
012400 A-CHECK-PREPARE   SECTION.                                               
012500                                                                          
012600*    -- NO ERROR YET                                                      
012700     MOVE ZERO TO TIME-KDRC                                               
012800                                                                          
012900     MOVE TIME-TIDATETIME  TO W-DATE-TIME-IN                              
013000     MOVE W-YYYY-IN   TO W-YYYY-COMP                                      
013010     MOVE W-MO-IN     TO W-MO-COMP                                        
013020     MOVE W-DD-IN     TO W-DD-COMP                                        
013030     MOVE W-HH-IN     TO W-HH-COMP                                        
013040     MOVE W-MI-IN     TO W-MI-COMP                                        
013050     MOVE W-SS-IN     TO W-SS-COMP                                        
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
014500 B-DATE-TIME-TO-SECONDS  SECTION.                                         
014600                                                                          
017600     CALL CEEISEC USING W-YYYY-COMP                                       
017700                        W-MO-COMP                                         
017800                        W-DD-COMP                                         
017900                        W-HH-COMP                                         
018000                        W-MI-COMP                                         
018100                        W-SS-COMP                                         
018101                        W-MILLI-COMP                                      
018110                        W-SECONDS-OUT                                     
018200                        FBC                                               
023400     IF SEV > 0                                                           
023500       MOVE 8 TO TIME-KDRC                                                
023510     ELSE                                                                 
023520       MOVE W-SECONDS-OUT TO TIME-TISECONDS                               
023600     END-IF                                                               
023700     .                                                                    
