//W222J314 JOB (670W2220100W222J314,W100),'RTN W200V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W222    EXEC W222P014,                                                        
//             INDUT=W222.W200V1                                                
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W222.W200V1.W22204(+1)                              
//*                                                                             
//    IF (EMPTY.T.RC NE 4) THEN                                                 
//SOP     EXEC WSOP,COMMAND='ORDER W222J314'                                    
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J314                                         
