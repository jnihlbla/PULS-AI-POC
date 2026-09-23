000100*COMPOPT ISPFPGM=YES                                                      
000200 Id Division.                                                             
000300     Skip2                                                                
000400 Program-Id.     CASE0100.                                                
000500*Author.         Odd Olsen.                                               
000600*Date-Written.   91/04/09.                                                
000610*                                                                         
000620* Användes av edit-macro STYLE för att ändra i ett Cobol-program          
000630* så att det får en väl avvägd kombination av versala och                 
000640* gemena bokstäver.( se detta program )                                   
000650*                                                                         
000660*                                                                         
000700                                                                          
000800 Environment Division.                                                    
000900 Data Division.                                                           
001000 Working-Storage Section.                                                 
001001                                                                          
001010*    -- CHECKED BY WY2000                                                 
001100     eject                                                                
001200 01  ja                 Pic X     Value 'J'.                              
001300 01  nej                Pic X     Value 'N'.                              
001320 01  to-line            Pic 9(6)  Value 0 Comp.                           
001330 01  ix                 Pic 9(6)  Value 0 Comp.                           
001400                                                                          
001500 77  caseconv           Pic X(8)  Value 'CASECONV'.                       
001600                                                                          
001700 01  alla               Pic X(4)  Value Space.                            
001710 01  rad                Pic X(80) Value Space.                            
001800 01  frange             Pic 9(6)  Value 0 Comp.                           
001900 01  lrange             Pic 9(6)  Value 0 Comp.                           
002000 01  firstl             Pic 9(6)  Value 0 Comp.                           
002100 01  lastl              Pic 9(6)  Value 0 Comp.                           
002110 01  lptr               Pic 9(6)  Value 0 Comp.                           
002200                                                                          
002300 01  isredit            Pic X(8)  Value 'ISREDIT '.                       
002400 01  vdefine            Pic X(8)  Value 'VDEFINE '.                       
002500 01  qalla              Pic X(8)  Value '(ALLA)  '.                       
002510 01  qrad               Pic X(8)  Value '(RAD)   '.                       
002600 01  qfrange            Pic X(8)  Value '(FRANGE)'.                       
002700 01  qlrange            Pic X(8)  Value '(LRANGE)'.                       
002800 01  qfirstl            Pic X(8)  Value '(FIRSTL)'.                       
002900 01  qlastl             Pic X(8)  Value '(LASTL) '.                       
002910 01  qlptr              Pic X(8)  Value '(LPTR)  '.                       
003000                                                                          
003001 01  char               Pic X(8)  Value 'CHAR    '.                       
003002 01  fixed              Pic X(8)  Value 'FIXED   '.                       
003003 01  len0               Pic 9(6)  Value  0 Comp.                          
003004 01  len4               Pic 9(6)  Value  4 Comp.                          
003005 01  len80              Pic 9(6)  Value 80 Comp.                          
003010                                                                          
003100 01  em1   Pic X(50) Value '§ MACRO (ALLA) NOPROCESS §'.                  
003200 01  em1a  Pic X(50) Value '§ PROCESS RANGE S §'.                         
003201 01  em2   Pic X(50) Value '§ (FIRSTL) = LINENUM .ZFIRST §'.              
003210 01  em3   Pic X(50) Value '§ (LASTL) = LINENUM .ZLAST §'.                
003220 01  em4   Pic X(50) Value '§ (FRANGE) = LINENUM .ZFRANGE §'.             
003221 01  em5   Pic X(50) Value '§ (LRANGE) = LINENUM .ZLRANGE §'.             
003230 01  em6   Pic X(50) Value '§ (RAD) = LINE &LPTR §'.                      
003240 01  em7   Pic X(50) Value '§ LINE &LPTR = (RAD) §'.                      
003300     eject                                                                
003400 01  lnk-area.                                                            
003500     03 lnk-reservd-case    Pic X Value 'F'.                              
003600     03 lnk-data-name-case  Pic X Value 'L'.                              
003700     03 lnk-proc-case       Pic X Value 'U'.                              
003710     03 Filler.                                                           
003800       05                   Pic X(06).                                    
003801       05 record-area       Pic X(74).                                    
003810     eject                                                                
003900 Procedure Division.                                                      
004000     skip2                                                                
004100 STYR Section.                                                            
004200     Call 'ISPLINK' Using  vdefine qalla alla char len4                   
004300     Call 'ISPLINK' Using  vdefine qrad rad char len80                    
004400     Call 'ISPLINK' Using  vdefine qfrange frange fixed len4              
004410     Call 'ISPLINK' Using  vdefine qlrange lrange fixed len4              
004420     Call 'ISPLINK' Using  vdefine qfirstl firstl fixed len4              
004430     Call 'ISPLINK' Using  vdefine qlastl  lastl  fixed len4              
004431     Call 'ISPLINK' Using  vdefine qlptr   lptr   fixed len4              
004432                                                                          
004433     Call 'ISPLINK' Using isredit len0 em1                                
004434     Call 'ISPLINK' Using isredit len0 em1a                               
004440     If alla = 'ALL' Or alla = 'ALLA'                                     
004441       Call 'ISPLINK' Using isredit len0 em2                              
004442       Call 'ISPLINK' Using isredit len0 em3                              
004450     Else                                                                 
004451       Call 'ISPLINK' Using isredit len0 em4                              
004452       Call 'ISPLINK' Using isredit len0 em5                              
004453       Move frange To firstl                                              
004454       Move lrange To lastl                                               
004460     End-If                                                               
004461     Move lastl  To to-line                                               
004462     Move firstl To lptr                                                  
004463     Add  +1     To to-line                                               
004464     Perform Until lptr = to-line                                         
004465       Call 'ISPLINK' Using isredit len0 em6                              
004500       Move rad To record-area                                            
004600       Call CASECONV Using lnk-area                                       
004601       Move record-area To rad                                            
004610       Call 'ISPLINK' Using isredit len0 em7                              
004611       Call 'ISPLINK' Using isredit len0 em6                              
004620       Add +1 To lptr                                                     
004630     End-Perform                                                          
004900     Goback                                                               
005000     .                                                                    
