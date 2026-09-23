//W476S5RE JOB (670W4760100W476S5RE,W100),'RTN W476S5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W476.W476S5.W47668(+0)                              
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//      EXEC WSOP                                                               
        ORDER W476S7                                                            
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W476.W476S5.W47668(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//EMPTYT3 EXEC WEMPTST2,DSIN=W476.W476S5.W47674                                 
//*                                                                             
//  IF (EMPTYT3.T.RC = 0) THEN                                                  
//      EXEC WSOP                                                               
        ORDER W611S6                                                            
//  ELSE                                                                        
//    IF (EMPTYT3.LISTCAT.RC = 0) THEN                                          
//      EXEC PGM=IEFBR14                                                        
//DD3   DD DSN=W476.W476S5.W47674(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//  ENDIF                                                                       
//*                                                                             
//EMPTYT4 EXEC WEMPTST2,DSIN=W476.W476S5.W47656                                 
//*                                                                             
//  IF (EMPTYT4.T.RC > 0) THEN                                                  
//    IF (EMPTYT4.LISTCAT.RC = 0) THEN                                          
//      EXEC PGM=IEFBR14                                                        
//DD4   DD DSN=W476.W476S5.W47656(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//  ENDIF                                                                       
//*                                                                             
//EMPTYT6 EXEC WEMPTST,DSIN=W476.W476S5.W47670(+0)                              
//*                                                                             
//    IF (EMPTYT6.T.RC = 0) THEN                                                
//      EXEC WSOP                                                               
        ORDER W476S8                                                            
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD6   DD DSN=W476.W476S5.W47670(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//EMPTYT7 EXEC WEMPTST,DSIN=W476.W476S5.W47692(+0)                              
//*                                                                             
//    IF (EMPTYT7.T.RC = 0) THEN                                                
//      EXEC WSOP                                                               
        ORDER W611S7                                                            
//    ELSE                                                                      
//      EXEC PGM=IEFBR14                                                        
//DD7   DD DSN=W476.W476S5.W47692(+0),DISP=(OLD,DELETE)                         
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476S5RE                                         
