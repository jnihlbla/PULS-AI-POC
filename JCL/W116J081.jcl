//W116J081 JOB (640W1160100W116J081,W100),'RTN W116S3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYST3                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P081                                                         
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116S3.W11681(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116S3.W11681(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SPAREPARTINFO                                          
/*                                                                              
//*   VCOM-D&P                                                                  
// EXEC WZ14DAP4,DSIN=&&W11681                                                  
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J081                                         
