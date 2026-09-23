000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS VID TEST AV:                     
000400*                            ***  - DISTRIKT FÖR NY KUNDSTRUKTUR          
000500*                            ***    IN (VR TILL RS). DVS FÖR DESSA        
000600*                            ***    DISTRIKT FLYTTAS DISTRIKT TILL        
000700*                            ***    KUNDNUMMER OCH IMPORTÖRENS            
000800*                            ***    DIRLEV DISTRIKT LÄGGS I DISTR.        
000900*                            ***  - DANMARK-VEST = DISTR 974              
001000*                            ***    DANMARK-OST  = DISTR 978              
001100*                            *************************************        
001200                                                                          
001300 01  DIST59-IDDISTR          PIC  9(5)    COMP-3.                         
001400                                                                          
001500   88  DIST59-DANMARK-VEST    VALUE  0900 THRU 0903                       
001600                                     0905 0909 0910 0911                  
001700                                     0913 THRU 0918                       
001800                                     0922 THRU 0928                       
001900                                     0936 0937 0938                       
002000                                     0946 0948 0949                       
002100                                     0953 0955 0957 0959                  
002200                                     0960 0961 0962                       
002300                                     0968 0969                            
002400                                     0971 0972 0974 0975                  
002500                                     0980 0982 0983                       
002600                                     0987 0989 0991                       
002700                                     0993 0994 0995                       
002800                                     0998.                                
002900                                                                          
003000   88  DIST59-DANMARK-OST    VALUE  0906 0907 0908 0912                   
003100                                    0919 THRU 0921                        
003200                                    0933 0934 0935                        
003300                                    0939 THRU 0942                        
003400                                    0944 0945                             
003500                                    0950 0951 0952 0954                   
003600                                    0956 0958                             
003700                                    0963 THRU 0967                        
003800                                    0973 0976 0977 0978                   
003900                                    0979 0981 0984                        
004000                                    0985 0986 0988                        
004100                                    0996.                                 
004200                                                                          
004300   88  DIST59-NORGE          VALUE  0800 THRU 0806                        
004400                                    0807 THRU 0837                        
004500                                    0839 THRU 0879                        
004600                                    0882 THRU 0889                        
004700                                    0893 0896                             
004800                                    1115 1117                             
004900                                    1123                                  
005000                                    1130 1135 1137                        
005100                                    1142 1144                             
005200                                    1144 THRU 1146                        
005300                                    1152                                  
005400                                    1159 THRU 1163                        
005500                                    1169.                                 
005600                                                                          
005700   88  DIST59-SVERIGE        VALUE  0100 THRU 0799.                       
005800                                                                          
005900   88  DIST59-PORTUGAL       VALUE  01920 THRU 01922                      
006000                                    01924 THRU 01927                      
006100                                    01940 THRU 01949                      
006200                                    01950 THRU 01959                      
006300                                    01960 THRU 01971.                     
006400                                                                          
006500   88  DIST59-GREKLAND       VALUE  1578  THRU 1581                       
006600                                    1558 1610.                            
006700                                                                          
006800   88  DIST59-TJECKIEN       VALUE  1379.                                 
006900                                                                          
007000   88  DIST59-SLOVENIEN      VALUE  1823.                                 
007100                                                                          
007200   88  DIST59-ITALIEN        VALUE  1822.                                 
007210                                                                          
007220   88  DIST59-SCHWEIZ        VALUE  2078.                                 
007230                                                                          
007240   88  DIST59-FRANKRIKE      VALUE  1478.                                 
007250                                                                          
007260   88  DIST59-FINLAND        VALUE  1090.                                 
007270                                                                          
007280   88  DIST59-ENGLAND        VALUE  1378.                                 
007290                                                                          
007291   88  DIST59-IRLAND         VALUE  1778.                                 
                                                                                
         88  DIST59-BRAZIL         VALUE  7051.                                 
007292                                                                          
007300*** END COPY WWDIST59C0  LENGTH=3     OLD LENGTH=                         
