000100*                            *************************************        
000200*                            *** FACTORS FOR CONVERSION OF                
000201*                            *** - WEIGHT                                 
000203*                            *** - VOLUME                                 
000205*                            *** - LENGTH                                 
000208*                            ***                                          
000209*                            *** EXAMPLES:                                
000210*                            *** YOU HAVE KILOGRAMS AND WANT              
000211*                            *** POUNDS:                                  
000212*                            *** KILOGRAMS * CONV-KG-TO-LB                
000213*                            *** = POUNDS                                 
000214*                            ***                                          
000215*                            *** YOU HAVE POUNDS AND WANT                 
000216*                            *** KILOGRAMS:                               
000217*                            *** POUNDS * CONV-LB-TO-KG                   
000218*                            *** = KILOGRAMS                              
000219*                            *************************************        
000220*           ****   VOLUME    ****                                         
000221*                                                                         
000222 01  CONV-M3-TO-FT3          PIC S9(2)V9(7)   COMP-3                      
000223                             VALUE 35.3146667.                            
000230*                  CUBIC METERS TO CUBIC FEET                             
000231*                                                                         
000232 01  CONV-M3-TO-YD3          PIC S9(1)V9(8)   COMP-3                      
000233                             VALUE 1.30795062.                            
000234*                  CUBIC METERS TO CUBIC YARDS                            
000235*                                                                         
000236 01  CONV-CM3-TO-IN3         PIC SV9(9)       COMP-3                      
000237                             VALUE 0.061023744.                           
000238*                  CUBIC CENTIMETERS TO CUBIC INCHES                      
000239*                                                                         
000240 01  CONV-LIT-TO-GAL         PIC SV9(9)       COMP-3                      
000241                             VALUE 0.264172053.                           
000242*                  LITERS TO GALLONS                                      
000243*                                                                         
000244 01  CONV-FT3-TO-M3          PIC SV9(9)       COMP-3                      
000245                             VALUE 0.028316847.                           
000246*                  CUBIC FEET TO CUBIC METERS                             
000247*                                                                         
000248 01  CONV-YD3-TO-M3          PIC SV9(9)       COMP-3                      
000249                             VALUE 0.764554858.                           
000250*                  CUBIC YARDS TO CUBIC METERS                            
000251*                                                                         
000252 01  CONV-IN3-TO-CM3         PIC S9(2)V9(7)   COMP-3                      
000253                             VALUE 16.3870640.                            
000254*                  CUBIC YARDS TO CUBIC METERS                            
000255*                                                                         
000256 01  CONV-GAL-TO-LIT         PIC S9V9(8)      COMP-3                      
000257                             VALUE 3.78541178.                            
000258*                  GALLONS TO LITERS                                      
000259*                                                                         
000260*           ****   WEIGHT    ****                                         
000261*                                                                         
000262 01  CONV-KG-TO-LB           PIC S9V9(8)      COMP-3                      
000263                             VALUE 2.20462262.                            
000270*                  KILOGRAMS TO POUNDS                                    
000600*                                                                         
000601 01  CONV-KG-TO-OZ           PIC S9(2)V9(7)    COMP-3                     
000602                             VALUE 35.2739620.                            
000603*                  KILOGRAMS TO OUNCES                                    
000604*                                                                         
000605 01  CONV-GR-TO-LB           PIC S9V9(9)       COMP-3                     
000606                             VALUE 0.002204623.                           
000607*                  GRAMS TO POUNDS                                        
000608*                                                                         
000609 01  CONV-GR-TO-OZ           PIC SV9(9)       COMP-3                      
000610                             VALUE 0.035273962.                           
000611*                  GRAMS TO OUNCES                                        
000612*                                                                         
000613 01  CONV-LB-TO-KG           PIC SV9(9)       COMP-3                      
000614                             VALUE 0.453592370.                           
000615*                  POUNDS TO KILOGRAMS                                    
000616*                                                                         
000617 01  CONV-LB-TO-GR           PIC S9(3)V9(6)   COMP-3                      
000618                             VALUE 453.592370.                            
000619*                  POUNDS TO GRAMS                                        
000620*                                                                         
000621 01  CONV-OZ-TO-KG           PIC SV9(9)       COMP-3                      
000622                             VALUE 0.028349523.                           
000623*                  OUNCES TO KILOGRAMS                                    
000624*                                                                         
000625 01  CONV-OZ-TO-GR           PIC S9(2)V9(7)   COMP-3                      
000626                             VALUE 28.3495231.                            
000627*                  OUNCES TO GRAMS                                        
000628*                                                                         
000629*           ****   LENGTH    ****                                         
000630*                                                                         
000631 01  CONV-CM-TO-IN           PIC SV9(9)       COMP-3                      
000632                             VALUE 0.393700787.                           
000633*                  CENTIMETERS TO INCHES                                  
000634*                                                                         
000635 01  CONV-M-TO-IN            PIC S9(2)V9(7)   COMP-3                      
000636                             VALUE 39.3700787.                            
000637*                  METERS TO INCHES                                       
000638*                                                                         
000639 01  CONV-M-TO-YD            PIC S9V9(8)      COMP-3                      
000640                             VALUE 1.09361330.                            
000641*                  METERS TO YARDS                                        
000642*                                                                         
000643 01  CONV-IN-TO-CM           PIC S9V9(6)      COMP-3                      
000644                             VALUE 2.540000.                              
000645*                  INCHES TO CENTIMETERS                                  
000646*                                                                         
000650 01  CONV-IN-TO-M            PIC SV9(7)       COMP-3                      
000651                             VALUE 0.0254000.                             
000652*                  INCHES TO METERS                                       
000660*                                                                         
000670 01  CONV-YD-TO-M            PIC SV9(7)       COMP-3                      
000671                             VALUE 0.9144000.                             
000680*                  YARDS TO METERS                                        
000690*                                                                         
000700*** END COPY WWOMVAND                                                     
