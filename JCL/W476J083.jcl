//W476J083 JOB (650W4760100W476J083,W100),'RTN W476DA',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W476    EXEC W476P083,                                                        
//             INDIN=W476.W476DA,                                               
//             INDUT=W476.W476DA                                                
//*                                                                             
//TOM     EXEC WEMPTST,DSIN=W476.W476DA.W47683(+1)                              
//  IF (TOM.T.RC = 0) THEN                                                      
//*                                                                             
//      EXEC WZ14PDAP,DSIN=W476.W476DA.W47683(+1)                               
//*                                                                             
W47683-001                                                                      
W4768300                                                                        
//*                                                                             
//  ENDIF                                                                       
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W476J083                                         
