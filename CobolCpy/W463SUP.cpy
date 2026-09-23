000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS FÖR ATT KONTROLLERA              
000400*                            *** SUPPLIERKODER.                           
000500*                            *** I TRANSAR FÖR DIRECT-BUSINESS.           
000600*                            ***                                          
000700*                            *************************************        
000800*                                                                         
000900*                                                                         
001000 01  W463SUP-KOD             PIC X(3).                                    
001100*                                                                         
001200     88  W463SUP-VTYR-OK     VALUE 'ATP' 'BRI' 'CON' 'CPK' 'DKB'          
001200                                   'DUN' 'ESA' 'FAS' 'FIB' 'FUL'          
001300                                   'GOD' 'HAN' 'ISK' 'JPA' 'KWI'          
001500                                   'MDV' 'MIC' 'MSX' 'MTY' 'NOK'          
001600                                   'NEX' 'PIR'                            
001700                                   'PSD' 'RJN' 'STS' 'SUM'                
001800                                   'VBD' 'VIA' 'VRE' 'WLT' 'WYZ'.         
001900*                                                                         
002000     88  W463SUP-VCON-OK     VALUE 'ANA' 'ATC' 'AUT' 'BER' 'BSF'          
002010                                   'CAS' 'DIN'                            
002100                                   'ESA' 'FAB' 'FOE' 'GDW' 'GET'          
002101                                   'HAR' 'HOL' 'INO' 'KON' 'LHO'          
002102                                   'OHM' 'SNA' 'SOD' 'SUP' 'VEI'          
002103                                   'WUE' 'SKI' 'SPD'.                     
003000*                                                                         
004000     88  W463SUP-VSER-OK     VALUE 'ATH' 'VCC' 'VOC'.                     
005000*                                                                         
006000     88  W463SUP-VMER-OK     VALUE 'SCC'.                                 
007000*                                                                         
008000*** END COPY W463SUP     LENGTH=3                                         
